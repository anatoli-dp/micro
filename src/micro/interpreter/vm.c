#include "./vm.h"

#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "./common.h"
#include "./compiler/compiler.h"
#include "./debug.h"
#include "./memory.h"
#include "./obj_native.h"
#include "./object.h"
#include "./native_registry.h"

#ifndef MICRO_SCRATCH_POOL_BYTES
#define MICRO_SCRATCH_POOL_BYTES 4096  // tune for MCU RAM
#endif

static inline uint32_t ms_align8(uint32_t x) { return (x + 7u) & ~7u; }

bool debugTraceExecution = false;

static void resetStack(VM *vm)
{
    vm->stackTop = vm->stack;
    vm->frameCount = 0;
    vm->openUpvalues = NULL;
}

void runtimeError(VM *vm, const char *format, ...)
{
    va_list args;
    va_start(args, format);
    vprintf(format, args);
    va_end(args);
    puts("\n");

    for (int i = vm->frameCount - 1; i >= 0; i--)
    {
        CallFrame *frame = &vm->frames[i];
        ObjFunction *function = frame->function;
        size_t instruction = frame->ip - function->chunk.code - 1;
        printf("[line %d] in ", function->chunk.lines[instruction]);
        if (function->name == NULL) {
            printf("script\n");
        } else {
            printf("%s()\n", function->name->chars);
        }
    }

    resetStack(vm);
}

static bool checkArity(VM *vm, int expected, int actual)
{
    if (expected != actual) runtimeError(vm, "Expected %d arguments but got %d.", expected, actual);
    return expected == actual;
}

static bool checkIndexBounds(VM *vm, const char *type, int bounds, Value indexValue)
{
    if (!IS_NUMBER(indexValue))
    {
        runtimeError(vm, "%s must be a number.", type);
        return false;
    }

    double indexNum = AS_NUMBER(indexValue);

    if (indexNum < 0 || indexNum >= (double) bounds)
    {
        runtimeError(vm, "%s (%g) out of bounds (%d).", type, indexNum, bounds);
        return false;
    }

    if ((double) (int) indexNum != indexNum)
    {
        runtimeError(vm, "%s (%g) must be a whole number.", type, indexNum);
        return false;
    }

    return true;
}

static bool checkStringIndex(VM *vm, ObjString *string, Value indexValue)
{
    return checkIndexBounds(vm, "String index", string->length, indexValue);
}

static bool clockNative(VM *vm, int argCount, Value *args)
{
    (void) args;
    if (!checkArity(vm, 0, argCount)) return false;
    push(vm, NUMBER_VAL((double)clock() / CLOCKS_PER_SEC));
    return true;
}

static void defineNative(VM *vm, const char *name, NativeFn function)
{
    push(vm, OBJ_VAL(copyString(&vm->gc, &vm->strings, name, (int)strlen(name))));
    push(vm, OBJ_VAL(newNative(&vm->gc, function)));
    int slot = vm->globalSlots.count;
    assert(slot < UINT16_MAX);
    writeValueArray(&vm->gc, &vm->globalSlots, vm->stack[1]);
    tableSet(&vm->gc, &vm->globals, AS_STRING(vm->stack[0]), NUMBER_VAL(slot));
    pop(vm);
    pop(vm);
}

static void vmMarkRoots(GC *gc, void *arg)
{
    VM *vm = (VM *) arg;

    for (int i = 0; i < vm->args.count; i++)
    {
        markValue(gc, vm->args.values[i]);
    }

    for (Value *slot = vm->stack; slot < vm->stackTop; slot++)
    {
        markValue(gc, *slot);
    }

    for (int i = 0; i < vm->frameCount; i++)
    {
        markObject(gc, (Obj *) vm->frames[i].closure);
        markObject(gc, (Obj *) vm->frames[i].function);

        // keep any bound lists alive for this frame
        for (int h = 0; h < MS_MAX_LBIND; ++h) {
            if (vm->frames[i].mh[h].list) {
                markObject(gc, (Obj *)vm->frames[i].mh[h].list);
            }
        }
    }

    for (ObjUpvalue *upvalue = vm->openUpvalues; upvalue != NULL;
         upvalue = upvalue->next)
    {
        markObject(gc, (Obj *) upvalue);
    }

    markTable(gc, &vm->globals);
    for (int i = 0; i < vm->globalSlots.count; i++)
    {
        markValue(gc, vm->globalSlots.values[i]);
    }

    markObject(gc, (Obj *) vm->initString);
    markObject(gc, (Obj *) vm->listClass);
    markObject(gc, (Obj *) vm->mapClass);
    markObject(gc, (Obj *) vm->stringClass);
}

static void defineNativeMethod(VM *vm, ObjClass *klass, const char *name, NativeFn fn)
{
    ObjString *str = copyString(&vm->gc, &vm->strings, name, (int) strlen(name));
    pushTemp(&vm->gc, OBJ_VAL(str));

    ObjNative *native = newNative(&vm->gc, fn);
    pushTemp(&vm->gc, OBJ_VAL(native));

    tableSet(&vm->gc, &klass->methods, str, OBJ_VAL(native));

    popTemp(&vm->gc); // str
    popTemp(&vm->gc); // native
}

static bool checkListIndex(VM *vm, Value listValue, Value indexValue)
{
    ObjList *list = AS_LIST(listValue);
    return checkIndexBounds(vm, "List index", list->elements.count, indexValue);
}

static bool listInsert(VM *vm, int argCount, Value *args)
{
    if (!checkArity(vm, 2, argCount)) return false;
    if (!checkListIndex(vm, args[-1], args[0])) return false;
    ObjList *list = AS_LIST(args[-1]);
    int pos = (int) AS_NUMBER(args[0]);
    insertValueArray(&vm->gc, &list->elements, pos, args[1]);
    push(vm, NULL_VAL);
    return true;
}

static bool listPop(VM *vm, int argCount, Value *args)
{
    if (!checkArity(vm, 0, argCount)) return false;
    ObjList *list = AS_LIST(args[-1]);
    if (list->elements.count == 0)
    {
        runtimeError(vm, "Can't pop from an empty list.");
        return false;
    }
    push(vm, removeValueArray(&list->elements, list->elements.count - 1));
    return true;
}

static bool listPush(VM *vm, int argCount, Value *args)
{
    if (!checkArity(vm, 1, argCount)) return false;
    ObjList *list = AS_LIST(args[-1]);
    writeValueArray(&vm->gc, &list->elements, args[0]);
    push(vm, NULL_VAL);
    return true;
}

static bool listRemove(VM *vm, int argCount, Value *args)
{
    if (!checkArity(vm, 1, argCount)) return false;
    if (!checkListIndex(vm, args[-1], args[0])) return false;
    ObjList *list = AS_LIST(args[-1]);
    int pos = (int) AS_NUMBER(args[0]);
    push(vm, removeValueArray(&list->elements, pos));
    return true;
}

static bool listSize(VM *vm, int argCount, Value *args)
{
    if (!checkArity(vm, 0, argCount)) return false;
    ObjList *list = AS_LIST(args[-1]);
    push(vm, NUMBER_VAL((double)list->elements.count));
    return true;
}

static void initListClass(VM *vm)
{
    const char listStr[] = "(List)";
    ObjString *listClassName = copyString(&vm->gc, &vm->strings, listStr, sizeof(listStr) - 1);
    pushTemp(&vm->gc, OBJ_VAL(listClassName));
    vm->listClass = newClass(&vm->gc, listClassName);
    popTemp(&vm->gc);

    defineNativeMethod(vm, vm->listClass, "insert", listInsert);
    defineNativeMethod(vm, vm->listClass, "push", listPush);
    defineNativeMethod(vm, vm->listClass, "pop", listPop);
    defineNativeMethod(vm, vm->listClass, "size", listSize);
    defineNativeMethod(vm, vm->listClass, "remove", listRemove);
}

static bool mapCount(VM *vm, int argCount, Value *args)
{
    if (!checkArity(vm, 0, argCount)) return false;
    ObjMap *map = AS_MAP(args[-1]);
    int count = 0;
    for (int i = 0; i < map->table.capacity; ++i)
    {
        count += !!(map->table.entries[i].key);
    }
    push(vm, NUMBER_VAL((double)count));
    return true;
}

static bool mapHas(VM *vm, int argCount, Value *args)
{
    if (!checkArity(vm, 1, argCount)) return false;
    if (!IS_STRING(args[0]))
    {
        runtimeError(vm, "Maps can only be indexed by string.");
        return false;
    }
    ObjMap *map = AS_MAP(args[-1]);
    ObjString *key = AS_STRING(args[0]);
    Value value;
    push(vm, BOOL_VAL(tableGet(&map->table, key, &value)));
    return true;
}

static bool mapKeys(VM *vm, int argCount, Value *args)
{
    if (!checkArity(vm, 0, argCount)) return false;
    ObjMap *map = AS_MAP(args[-1]);
    ObjList *keys = newList(&vm->gc);
    push(vm, OBJ_VAL(keys));
    for (int i = 0; i < map->table.capacity; ++i)
    {
        Entry *entry = &map->table.entries[i];
        if (entry->key == NULL) continue;
        writeValueArray(&vm->gc, &keys->elements, OBJ_VAL(entry->key));
    }
    return true;
}

static bool mapRemove(VM *vm, int argCount, Value *args)
{
    if (!checkArity(vm, 1, argCount)) return false;
    if (!IS_STRING(args[0]))
    {
        runtimeError(vm, "Maps can only be indexed by string.");
        return false;
    }
    ObjMap *map = AS_MAP(args[-1]);
    ObjString *key = AS_STRING(args[0]);
    push(vm, BOOL_VAL(tableDelete(&map->table, key)));
    return true;
}

static void initMapClass(VM *vm)
{
    const char mapStr[] = "(Map)";
    ObjString *mapClassName = copyString(&vm->gc, &vm->strings, mapStr, sizeof(mapStr) - 1);
    pushTemp(&vm->gc, OBJ_VAL(mapClassName));
    vm->mapClass = newClass(&vm->gc, mapClassName);
    popTemp(&vm->gc);

    defineNativeMethod(vm, vm->mapClass, "count", mapCount);
    defineNativeMethod(vm, vm->mapClass, "has", mapHas);
    defineNativeMethod(vm, vm->mapClass, "keys", mapKeys);
    defineNativeMethod(vm, vm->mapClass, "remove", mapRemove);
}

static bool stringParseNum(VM *vm, int argCount, Value *args)
{
    if (!checkArity(vm, 0, argCount)) return false;
    ObjString *string = AS_STRING(args[-1]);
    char *after;
    double result = strtod(string->chars, &after);
    while (after < string->chars + string->length)
    {
        if (!isspace(*after)) break;
        ++after;
    }
    if (after == string->chars + string->length) {
        push(vm, NUMBER_VAL(result));
    } else {
        push(vm, NULL_VAL);
    }
    return true;
}

static bool stringSize(VM *vm, int argCount, Value *args)
{
    if (!checkArity(vm, 0, argCount)) return false;
    ObjString *string = AS_STRING(args[-1]);
    push(vm, NUMBER_VAL((double)string->length));
    return true;
}

static bool substrIndex(VM *vm, Value input, const char *type, int length, int *index)
{
    if (!IS_NUMBER(input))
    {
        runtimeError(vm, "%s must be a number.", type);
        return false;
    }
    double num = AS_NUMBER(input);
    if (num <= (double) INT_MIN) {
        *index = INT_MIN;
    } else if (num >= (double) INT_MAX) {
        *index = INT_MAX;
    } else
    {
        if ((double) (int) num != num)
        {
            runtimeError(vm, "%s (%g) must be a whole number.", type, num);
            return false;
        }
        *index = num;
    }
    if (*index < 0) *index += length + 1;
    if (*index < 0) {
        *index = 0;
    } else if (*index > length) {
        *index = length;
    }
    return true;
}

static bool stringSubstr(VM *vm, int argCount, Value *args)
{
    if (!checkArity(vm, 2, argCount)) return false;
    ObjString *string = AS_STRING(args[-1]);
    int start;
    int end;
    if (!substrIndex(vm, args[0], "Start", string->length, &start)) return false;
    if (!substrIndex(vm, args[1], "End", string->length, &end)) return false;
    const char *chars = "";
    int length = 0;
    if (start < end)
    {
        chars = string->chars + start;
        length = end - start;
    }
    push(vm, OBJ_VAL(copyString(&vm->gc, &vm->strings, chars, length)));
    return true;
}

static void initStringClass(VM *vm)
{
    const char stringStr[] = "(String)";
    ObjString *stringClassName = copyString(&vm->gc, &vm->strings, stringStr, sizeof(stringStr) - 1);
    pushTemp(&vm->gc, OBJ_VAL(stringClassName));
    vm->stringClass = newClass(&vm->gc, stringClassName);
    popTemp(&vm->gc);

    defineNativeMethod(vm, vm->stringClass, "parsenum", stringParseNum);
    defineNativeMethod(vm, vm->stringClass, "size", stringSize);
    defineNativeMethod(vm, vm->stringClass, "substr", stringSubstr);
}

static void removeTrailingZeros_helper (char* s)
{
    char *p;
    int count;

    p = strchr (s,'.');
    if (p != NULL)
    {
        count = 6;
        while (count >= 0)
        {
            count--;
            if (*p == '\0') break;
            p++;
        }

        *p-- = '\0';
        while (*p == '0') *p-- = '\0';
        if (*p == '.') *p = '\0';
    }
}

void toString_helper (char* outStr, Value* args, int start, int end)
{
    memset(outStr, '\0', sizeof(outStr));
    sprintf(outStr, "");

    for (int i = start; i < end; i++)
    {
        if (IS_STRING(args[i]))
        {
            char* s = AS_CSTRING(args[i]);
            strcat(outStr, s);
        } else if (IS_BOOL(args[i])) {
            bool b = AS_BOOL(args[i]);
            if (b) {
                char* s = "TRUE";
                strcat(outStr, s);
            } else {
                char* s = "FALSE";
                strcat(outStr, s);
            }
        } else if (IS_NUMBER(args[i])) {
            double number = AS_NUMBER(args[i]);

            char s[30];
            memset(s, '\0', sizeof(s));
            sprintf(s, "%lf", number);
            removeTrailingZeros_helper(s);

            strcat(outStr, s);
        } else if (IS_NULL(args[i])) {
            strcat(outStr, "NULL");
        }
    }
}

static bool strNative(VM* vm, int argCount, Value* args) {
    char text[300];
    memset(text, '\0', sizeof(text));
    sprintf(text, "");
    toString_helper(text, args, 0, argCount);
    ObjString* string = copyString(&vm->gc, &vm->strings, text, strlen(text));
    push(vm, OBJ_VAL(string));
    return true;
}

static bool printNative(VM *vm, int argCount, Value *args)
{
    char text[300];
    memset(text, '\0', sizeof(text));
    sprintf(text, "");
    toString_helper(text, args, 0, argCount);
    printf("%s\n", text);

    push(vm, NULL_VAL);
    return true;
}

void initVM(VM *vm)
{
    resetStack(vm);
    initGC(&vm->gc);
    vm->gc.markRoots = vmMarkRoots;
    vm->gc.markRootsArg = vm;
    vm->gc.fixWeak = (void (*)(void *)) tableRemoveWhite;
    vm->gc.fixWeakArg = &vm->strings;

    initValueArray(&vm->args);
    initTable(&vm->globals, 0.75);
    initValueArray(&vm->globalSlots);
    initTable(&vm->strings, 0.75);

    vm->initString = NULL;
    vm->listClass = NULL;
    vm->mapClass = NULL;
    vm->stringClass = NULL;

    vm->initString = copyString(&vm->gc, &vm->strings, "init", 4);
    initListClass(vm);
    initMapClass(vm);
    initStringClass(vm);

    // Add slow-path versions of fast natives to the global scope
    // This allows them to be stored in variables, e.g. `var mySin = sin;`
    int nativeCount = getFastNativeCount();
    for (int i = 0; i < nativeCount; i++) {
        defineNative(vm, G_FAST_NATIVES[i].name, G_FAST_NATIVES[i].function);
    }

    defineNative(vm, "clock", clockNative);
    defineNative(vm, "str", strNative);
    defineNative(vm, "println", printNative);

    for (int i = 0; i < 8; i++) {
        vm->dreg[i] = 0.0;
        vm->ireg[i] = 0;
    }

    vm->ms_size = MICRO_SCRATCH_POOL_BYTES;
    vm->ms_sp   = 0;
    vm->ms_pool = (uint8_t*)malloc(vm->ms_size);
    if (!vm->ms_pool) {
        fprintf(stderr, "Fatal: micro scratch pool alloc failed.\n");
        exit(1);
    }
}

void freeVM(VM *vm)
{
    freeValueArray(&vm->gc, &vm->args);
    freeTable(&vm->gc, &vm->globals);
    freeValueArray(&vm->gc, &vm->globalSlots);
    freeTable(&vm->gc, &vm->strings);
    vm->initString = NULL;
    freeGC(&vm->gc);

    if (vm->ms_pool) {
        free(vm->ms_pool);
        vm->ms_pool = NULL;
        vm->ms_sp = vm->ms_size = 0;
    }

}

void argsVM(VM *vm, int argc, const char *argv[])
{
    for (int i = 0; i < argc; ++i)
    {
        const char *ptr = argv[i] ? argv[i] : "";
        ObjString *s = copyString(&vm->gc, &vm->strings, ptr, strlen(ptr));
        Value arg = OBJ_VAL(s);
        pushTemp(&vm->gc, arg);
        writeValueArray(&vm->gc, &vm->args, arg);
        popTemp(&vm->gc);
    }
}

void push(VM *vm, Value value)
{
    assert(vm->stackTop < vm->stack + STACK_MAX);
    *vm->stackTop = value;
    vm->stackTop++;
}

Value pop(VM *vm)
{
    assert(vm->stackTop > vm->stack);
    vm->stackTop--;
    return *vm->stackTop;
}

static Value peek(VM *vm, int distance)
{
    assert(distance < vm->stackTop - vm->stack);
    return vm->stackTop[-1 - distance];
}

static bool call(VM *vm, Obj *callable, int argCount)
{
    ObjClosure *closure;
    ObjFunction *function;

    if (callable->type == OBJ_CLOSURE) {
        closure = (ObjClosure *) callable;
        function = closure->function;
    } else {
        assert(callable->type == OBJ_FUNCTION);
        closure = NULL;
        function = (ObjFunction *) callable;
    }

    if (!checkArity(vm, function->arity, argCount)) return false;

    if (vm->frameCount == FRAMES_MAX) {
        runtimeError(vm, "Stack overflow.");
        return false;
    }

    CallFrame *frame = &vm->frames[vm->frameCount++];

    // ---- microasm scratch setup for this frame ----
    frame->ms_sp_mark = vm->ms_sp;
    frame->msi = NULL; frame->msd = NULL;
    frame->msi_cap = 0; frame->msd_cap = 0;

    // ---- clear all bound-list handles and caches (h0..h15) ----
    for (int h = 0; h < MS_MAX_LBIND; ++h) {
        frame->mh[h].list  = NULL;
        frame->mh[h].elems = NULL;
        frame->mh[h].len   = 0u;
    }

    uint16_t need_i = 0, need_f = 0;
    {
        ObjFunction *fn = function; // already set above
        if (fn) { need_i = fn->micro_i_slots; need_f = fn->micro_f_slots; }
    }

    if (need_i || need_f) {
        uint32_t need_bytes =
            (uint32_t)need_i * (uint32_t)sizeof(int64_t) +
            (uint32_t)need_f * (uint32_t)sizeof(double);

        uint32_t sp = ms_align8(vm->ms_sp);
        if (sp + ms_align8(need_bytes) > vm->ms_size) {
            runtimeError(vm, "Micro scratch exhausted (need %u bytes).",
                         (unsigned)need_bytes);
            return false;
        }

        // Reserve and split
        uint8_t *base = vm->ms_pool + sp;
        frame->msi = (need_i ? (int64_t*)base : NULL);
        if (need_i) base += (uint32_t)need_i * (uint32_t)sizeof(int64_t);
        frame->msd = (need_f ? (double*)base : NULL);

        vm->ms_sp = sp + ms_align8(need_bytes);
        frame->msi_cap = need_i;
        frame->msd_cap = need_f;
    }

    frame->closure = closure;
    frame->function = function;
    frame->ip = function->chunk.code;
    frame->slots = vm->stackTop - argCount - 1;
    return true;
}

static bool callValue(VM *vm, Value callee, int argCount)
{
    if (IS_OBJ(callee))
    {
        switch (OBJ_TYPE(callee))
        {
            case OBJ_FUNCTION:
            case OBJ_CLOSURE:
                return call(vm, AS_OBJ(callee), argCount);
            case OBJ_BOUND_METHOD:
            {
                ObjBoundMethod *bound = AS_BOUND_METHOD(callee);
                vm->stackTop[-argCount - 1] = bound->receiver;
                return callValue(vm, OBJ_VAL(bound->method), argCount);
            }
            case OBJ_CLASS:
            {
                ObjClass *klass = AS_CLASS(callee);
                vm->stackTop[-argCount - 1] = OBJ_VAL(newInstance(&vm->gc, klass));
                Value initializer;
                if (tableGet(&klass->methods, vm->initString, &initializer)) {
                    return callValue(vm, initializer, argCount);
                } else if (!checkArity(vm, 0, argCount)) {
                    return false;
                }
                return true;
            }
            case OBJ_NATIVE:
            {
                NativeFn native = AS_NATIVE(callee);
                if (!native(vm, argCount, vm->stackTop - argCount)) return false;
                Value result = pop(vm);
                vm->stackTop -= argCount + 1;
                push(vm, result);
                return true;
            }
            default: break; // Non-callable object type.
        }
    }
    runtimeError(vm, "Can only call functions and classes.");
    return false;
}

static bool invokeFromClass(VM *vm, ObjClass *klass, ObjString *name, int argCount)
{
    Value method;
    if (!tableGet(&klass->methods, name, &method))
    {
        runtimeError(vm, "Undefined property '%s'.", name->chars);
        return false;
    }
    return callValue(vm, method, argCount);
}

static bool invoke(VM *vm, ObjString *name, int argCount)
{
    Value receiver = peek(vm, argCount);
    ObjClass *klass;

    if (IS_LIST(receiver)) {
        klass = vm->listClass;
    } else if (IS_MAP(receiver)) {
        klass = vm->mapClass;
    } else if (IS_STRING(receiver)) {
        klass = vm->stringClass;
    } else if (IS_INSTANCE(receiver)) {
        ObjInstance *instance = AS_INSTANCE(receiver);
        Value value;
        if (tableGet(&instance->fields, name, &value))
        {
            vm->stackTop[-argCount - 1] = value;
            return callValue(vm, value, argCount);
        }
        klass = instance->klass;
    } else {
        runtimeError(vm, "Only lists, maps, strings and instances have methods.");
        return false;
    }

    return invokeFromClass(vm, klass, name, argCount);
}

static bool bindMethod(VM *vm, ObjClass *klass, ObjString *name)
{
    Value method;
    if (!tableGet(&klass->methods, name, &method))
    {
        runtimeError(vm, "Undefined property '%s'.", name->chars);
        return false;
    }

    ObjBoundMethod *bound = newBoundMethod(&vm->gc, peek(vm, 0), AS_OBJ(method));
    pop(vm);
    push(vm, OBJ_VAL(bound));
    return true;
}

static ObjUpvalue *captureUpvalue(VM *vm, Value *local)
{
    ObjUpvalue *prevUpvalue = NULL;
    ObjUpvalue *upvalue = vm->openUpvalues;
    while (upvalue != NULL && upvalue->location > local)
    {
        prevUpvalue = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue != NULL && upvalue->location == local) return upvalue;

    ObjUpvalue *createdUpvalue = newUpvalue(&vm->gc, local);
    createdUpvalue->next = upvalue;

    if (prevUpvalue == NULL) {
        vm->openUpvalues = createdUpvalue;
    } else {
        prevUpvalue->next = createdUpvalue;
    }

    return createdUpvalue;
}

static void closeUpvalues(VM *vm, Value *last)
{
    while (vm->openUpvalues != NULL && vm->openUpvalues->location >= last)
    {
        ObjUpvalue *upvalue = vm->openUpvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm->openUpvalues = upvalue->next;
    }
}

static void defineMethod(VM *vm, ObjString *name)
{
    Value method = peek(vm, 0);
    ObjClass *klass = AS_CLASS(peek(vm, 1));
    tableSet(&vm->gc, &klass->methods, name, method);
    pop(vm);
}

static bool isFalsey(Value value)
{
    return IS_NULL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate(VM *vm, Value aValue, Value bValue, bool popTwice)
{
    ObjString *b = AS_STRING(bValue);
    ObjString *a = AS_STRING(aValue);
    ObjString *result = concatStrings(&vm->gc, &vm->strings, a->chars, a->length, a->hash, b->chars, b->length);
    pop(vm);
    if (popTwice) pop(vm);
    push(vm, OBJ_VAL(result));
}

static void trace(VM *vm, CallFrame *frame)
{
    printf("          ");
    for (Value *slot = vm->stack; slot < vm->stackTop; slot++)
    {
        printf("[ ");
        printValue(*slot);
        printf(" ]");
    }
    printf("\n");
    disassembleInstruction(&frame->function->chunk, (int) (frame->ip - frame->function->chunk.code));
}

static InterpretResult run(VM *vm)
{
    CallFrame *frame = &vm->frames[vm->frameCount - 1];

#define READ_BYTE() ((uint8_t)(*frame->ip++))
#define READ_OPCODE() (*frame->ip++)
#define READ_SHORT() (*frame->ip++)
#define READ_CONSTANT() (frame->function->chunk.constants.values[READ_SHORT()])
#define READ_STRING() AS_STRING(READ_CONSTANT())

#define REG_POS1(x)   (((x) >> 12) & 0xF)
#define REG_POS2(x)   (((x) >> 8)  & 0xF)
#define REG_POS3(x)   (((x) >> 4)  & 0xF)
#define REG_POS4(x)   ((x) & 0xF)

// Read 8 bytes as int64/double from ip (uint16_t*) and advance by 4 words
#define READ_I64(var)  do { memcpy(&(var), frame->ip, 8); frame->ip += 4; } while (0)
#define READ_DBL(var)  do { memcpy(&(var), frame->ip, 8); frame->ip += 4; } while (0)

#define BINARY_OP(valueType, op) \
    do { \
        if (!IS_NUMBER(peek(vm, 0)) || !IS_NUMBER(peek(vm, 1))) { \
            runtimeError(vm, "Operands must be numbers."); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
        double b = AS_NUMBER(pop(vm)); \
        double a = AS_NUMBER(pop(vm)); \
        push(vm, valueType(a op b)); \
    } while (false)

#define BINARY_OP_C(valueType, op) \
    do { \
        Value bValue = READ_CONSTANT(); \
        if (!IS_NUMBER(bValue) || !IS_NUMBER(peek(vm, 0))) { \
            runtimeError(vm, "Operands must be numbers."); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
        double b = AS_NUMBER(bValue); \
        double a = AS_NUMBER(pop(vm)); \
        push(vm, valueType(a op b)); \
    } while (false)

#define BINARY_BIT_OP(op) \
    do { \
        if (!IS_NUMBER(peek(vm, 0)) || !IS_NUMBER(peek(vm, 1))) { \
            runtimeError(vm, "Operands must be numbers."); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
        int64_t b = (int64_t)AS_NUMBER(pop(vm)); \
        int64_t a = (int64_t)AS_NUMBER(pop(vm)); \
        push(vm, NUMBER_VAL(((int64_t)a) op ((int64_t)b))); \
    } while (false)

#define BINARY_BIT_OP_C(op) \
    do { \
        Value bValue = READ_CONSTANT(); \
        if (!IS_NUMBER(bValue) || !IS_NUMBER(peek(vm, 0))) { \
            runtimeError(vm, "Operands must be numbers."); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
        int64_t b = (int64_t)AS_NUMBER(bValue); \
        int64_t a = (int64_t)AS_NUMBER(pop(vm)); \
        push(vm, NUMBER_VAL(((int64_t)a) op ((int64_t)b))); \
    } while (false)

#define BINARY_REG_OP(op) \
    do { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst   = REG_POS1(regs); \
        uint8_t aReg  = REG_POS2(regs); \
        uint8_t bReg  = REG_POS3(regs); \
        uint8_t flags = REG_POS4(regs); \
        if (dst < 8) { \
            int64_t a, b; \
            if (flags & 0x1) { READ_I64(a); } else { a = vm->ireg[aReg]; } \
            if (flags & 0x2) { READ_I64(b); } else { b = vm->ireg[bReg]; } \
            vm->ireg[dst] = a op b; \
        } else { \
            double a, b; \
            if (flags & 0x1) { READ_DBL(a); } else { a = vm->dreg[aReg - 8]; } \
            if (flags & 0x2) { READ_DBL(b); } else { b = vm->dreg[bReg - 8]; } \
            vm->dreg[dst - 8] = a op b; \
        } \
    } while (0)

#define PREPARE_MAP_KEY(key_val) \
    if (IS_NUMBER(key_val)) { \
        double number = AS_NUMBER(key_val); \
        char buffer[32]; \
        int length = snprintf(buffer, sizeof(buffer), "%g", number); \
        key_val = OBJ_VAL(copyString(&vm->gc, &vm->strings, buffer, length)); \
        pushTemp(&vm->gc, key_val); \
    } else if (!IS_STRING(key_val)) { \
        runtimeError(vm, "Map keys must be strings or numbers."); \
        return INTERPRET_RUNTIME_ERROR; \
    }

#define POP_MAP_KEY_TEMP(key_val) \
    if (IS_NUMBER(key_val)) { \
        popTemp(&vm->gc); \
    }

#define JUMP_ENTRY(op) [op] = &&CASE_##op
    static void *jumps[MAX_OPCODES] = {
        JUMP_ENTRY(PUSH_CONST),
        JUMP_ENTRY(PUSH_NULL),
        JUMP_ENTRY(PUSH_TRUE),
        JUMP_ENTRY(PUSH_FALSE),
        JUMP_ENTRY(DUP),
        JUMP_ENTRY(DUP2),
        JUMP_ENTRY(POP),
        JUMP_ENTRY(LD_LOCAL),
        JUMP_ENTRY(ST_LOCAL),
        JUMP_ENTRY(LD_GLOBAL),
        JUMP_ENTRY(LD_GLOBAL_I),
        JUMP_ENTRY(DEF_GLOBAL),
        JUMP_ENTRY(ST_GLOBAL),
        JUMP_ENTRY(ST_GLOBAL_I),
        JUMP_ENTRY(LD_UPVAL),
        JUMP_ENTRY(ST_UPVAL),
        JUMP_ENTRY(LD_PROP),
        JUMP_ENTRY(ST_PROP),
        JUMP_ENTRY(LD_INDEX),
        JUMP_ENTRY(ST_INDEX),
        JUMP_ENTRY(LD_SUPER),
        JUMP_ENTRY(EQ),
        JUMP_ENTRY(NEQ),
        JUMP_ENTRY(EQ_C),
        JUMP_ENTRY(NEQ_C),
        JUMP_ENTRY(GT),
        JUMP_ENTRY(GT_C),
        JUMP_ENTRY(LT),
        JUMP_ENTRY(LT_C),
        JUMP_ENTRY(ADD),
        JUMP_ENTRY(ADD_C),
        JUMP_ENTRY(SUB),
        JUMP_ENTRY(SUB_C),
        JUMP_ENTRY(MUL),
        JUMP_ENTRY(MUL_C),
        JUMP_ENTRY(DIV),
        JUMP_ENTRY(DIV_C),
        JUMP_ENTRY(MOD),
        JUMP_ENTRY(MOD_C),
        JUMP_ENTRY(NOT),
        JUMP_ENTRY(NEG),
        JUMP_ENTRY(BAND),
        JUMP_ENTRY(BAND_C),
        JUMP_ENTRY(BOR),
        JUMP_ENTRY(BOR_C),
        JUMP_ENTRY(BXOR),
        JUMP_ENTRY(BXOR_C),
        JUMP_ENTRY(BNOT),
        JUMP_ENTRY(BSHL),
        JUMP_ENTRY(BSHL_C),
        JUMP_ENTRY(BSHR),
        JUMP_ENTRY(BSHR_C),
        JUMP_ENTRY(DBG_PRINT),
        JUMP_ENTRY(JMP),
        JUMP_ENTRY(JMP_IF_FALSE),
        JUMP_ENTRY(PJMP_IF_FALSE),
        JUMP_ENTRY(JMP_IF_TRUE),
        JUMP_ENTRY(PJMP_IF_TRUE),
        JUMP_ENTRY(JMP_BACK),
        JUMP_ENTRY(CALL),
        JUMP_ENTRY(FAST_NATIVE_CALL),
        JUMP_ENTRY(INVOKE),
        JUMP_ENTRY(INVOKE_SUPER),
        JUMP_ENTRY(MAKE_CLOSURE),
        JUMP_ENTRY(CLOSE_UPVAL),
        JUMP_ENTRY(MAKE_LIST),
        JUMP_ENTRY(LIST_DATA),
        JUMP_ENTRY(MAKE_MAP),
        JUMP_ENTRY(MAP_DATA),
        JUMP_ENTRY(RET),
        JUMP_ENTRY(MAKE_CLASS),
        JUMP_ENTRY(CLASS_INHERIT),
        JUMP_ENTRY(DEF_METHOD),

        // Prefix/Postfix ops
        JUMP_ENTRY(PRE_INC_LOCAL),
        JUMP_ENTRY(PRE_DEC_LOCAL),
        JUMP_ENTRY(POST_INC_LOCAL),
        JUMP_ENTRY(POST_DEC_LOCAL),
        JUMP_ENTRY(PRE_INC_GLOBAL),
        JUMP_ENTRY(PRE_DEC_GLOBAL),
        JUMP_ENTRY(POST_INC_GLOBAL),
        JUMP_ENTRY(POST_DEC_GLOBAL),
        JUMP_ENTRY(PRE_INC_GLOBAL_I),
        JUMP_ENTRY(PRE_DEC_GLOBAL_I),
        JUMP_ENTRY(POST_INC_GLOBAL_I),
        JUMP_ENTRY(POST_DEC_GLOBAL_I),
        JUMP_ENTRY(PRE_INC_UPVAL),
        JUMP_ENTRY(PRE_DEC_UPVAL),
        JUMP_ENTRY(POST_INC_UPVAL),
        JUMP_ENTRY(POST_DEC_UPVAL),
        JUMP_ENTRY(PRE_INC_PROP),
        JUMP_ENTRY(PRE_DEC_PROP),
        JUMP_ENTRY(POST_INC_PROP),
        JUMP_ENTRY(POST_DEC_PROP),
        JUMP_ENTRY(PRE_INC_INDEX),
        JUMP_ENTRY(PRE_DEC_INDEX),
        JUMP_ENTRY(POST_INC_INDEX),
        JUMP_ENTRY(POST_DEC_INDEX),

        // microasm: typed immediate loads
        JUMP_ENTRY(R_ILOAD_IMM),
        JUMP_ENTRY(R_FLOAD_IMM),

        // microasm: fast reg <-> local/global and ret
        JUMP_ENTRY(R_LD_LOCAL_R),
        JUMP_ENTRY(R_ST_LOCAL_R),
        JUMP_ENTRY(R_LD_UPVAL_R),
        JUMP_ENTRY(R_ST_UPVAL_R),
        JUMP_ENTRY(R_LD_GLOBAL_R),
        JUMP_ENTRY(R_ST_GLOBAL_R),
        JUMP_ENTRY(R_RET_R),

        // microasm: scratch typed
        JUMP_ENTRY(R_SST),
        JUMP_ENTRY(R_SLD),

        // microasm: reg-conditional jumps
        JUMP_ENTRY(R_JZ),
        JUMP_ENTRY(R_JNZ),

        // microasm: typed ALU (int)
        JUMP_ENTRY(R_IADD_RR), JUMP_ENTRY(R_IADD_RI), JUMP_ENTRY(R_IADD_IR), JUMP_ENTRY(R_IADD_II),
        JUMP_ENTRY(R_ISUB_RR), JUMP_ENTRY(R_ISUB_RI), JUMP_ENTRY(R_ISUB_IR), JUMP_ENTRY(R_ISUB_II),
        JUMP_ENTRY(R_IMUL_RR), JUMP_ENTRY(R_IMUL_RI), JUMP_ENTRY(R_IMUL_IR), JUMP_ENTRY(R_IMUL_II),
        JUMP_ENTRY(R_IDIV_RR), JUMP_ENTRY(R_IDIV_RI), JUMP_ENTRY(R_IDIV_IR), JUMP_ENTRY(R_IDIV_II),
        JUMP_ENTRY(R_IMOD_RR), JUMP_ENTRY(R_IMOD_RI), JUMP_ENTRY(R_IMOD_IR), JUMP_ENTRY(R_IMOD_II),

        // microasm: typed ALU (float)
        JUMP_ENTRY(R_FADD_RR), JUMP_ENTRY(R_FADD_RI), JUMP_ENTRY(R_FADD_IR), JUMP_ENTRY(R_FADD_II),
        JUMP_ENTRY(R_FSUB_RR), JUMP_ENTRY(R_FSUB_RI), JUMP_ENTRY(R_FSUB_IR), JUMP_ENTRY(R_FSUB_II),
        JUMP_ENTRY(R_FMUL_RR), JUMP_ENTRY(R_FMUL_RI), JUMP_ENTRY(R_FMUL_IR), JUMP_ENTRY(R_FMUL_II),
        JUMP_ENTRY(R_FDIV_RR), JUMP_ENTRY(R_FDIV_RI), JUMP_ENTRY(R_FDIV_IR), JUMP_ENTRY(R_FDIV_II),
        JUMP_ENTRY(R_FMOD_RR), JUMP_ENTRY(R_FMOD_RI), JUMP_ENTRY(R_FMOD_IR), JUMP_ENTRY(R_FMOD_II),

        // microasm: INT64 bitwise/shifts/rotates/bit-ops
        JUMP_ENTRY(R_IAND_RR), JUMP_ENTRY(R_IAND_RI), JUMP_ENTRY(R_IAND_IR), JUMP_ENTRY(R_IAND_II),
        JUMP_ENTRY(R_IOR_RR),  JUMP_ENTRY(R_IOR_RI),  JUMP_ENTRY(R_IOR_IR),  JUMP_ENTRY(R_IOR_II),
        JUMP_ENTRY(R_IXOR_RR), JUMP_ENTRY(R_IXOR_RI), JUMP_ENTRY(R_IXOR_IR), JUMP_ENTRY(R_IXOR_II),

        JUMP_ENTRY(R_ISHL_RR), JUMP_ENTRY(R_ISHL_RI), JUMP_ENTRY(R_ISHL_IR), JUMP_ENTRY(R_ISHL_II),
        JUMP_ENTRY(R_ISHR_RR), JUMP_ENTRY(R_ISHR_RI), JUMP_ENTRY(R_ISHR_IR), JUMP_ENTRY(R_ISHR_II), // logical >>
        JUMP_ENTRY(R_ISAR_RR), JUMP_ENTRY(R_ISAR_RI), JUMP_ENTRY(R_ISAR_IR), JUMP_ENTRY(R_ISAR_II), // arithmetic >>

        JUMP_ENTRY(R_IROL_RR), JUMP_ENTRY(R_IROL_RI), JUMP_ENTRY(R_IROL_IR), JUMP_ENTRY(R_IROL_II),
        JUMP_ENTRY(R_IROR_RR), JUMP_ENTRY(R_IROR_RI), JUMP_ENTRY(R_IROR_IR), JUMP_ENTRY(R_IROR_II),

        JUMP_ENTRY(R_IBTST_RR), JUMP_ENTRY(R_IBTST_RI), JUMP_ENTRY(R_IBTST_IR), JUMP_ENTRY(R_IBTST_II),
        JUMP_ENTRY(R_IBSET_RR), JUMP_ENTRY(R_IBSET_RI), JUMP_ENTRY(R_IBSET_IR), JUMP_ENTRY(R_IBSET_II),
        JUMP_ENTRY(R_IBCLR_RR), JUMP_ENTRY(R_IBCLR_RI), JUMP_ENTRY(R_IBCLR_IR), JUMP_ENTRY(R_IBCLR_II),
        JUMP_ENTRY(R_IBTGL_RR), JUMP_ENTRY(R_IBTGL_RI), JUMP_ENTRY(R_IBTGL_IR), JUMP_ENTRY(R_IBTGL_II),

        JUMP_ENTRY(R_IBNOT),

        // microasm: bound list (fast)
        JUMP_ENTRY(R_LBIND_LOCAL),
        JUMP_ENTRY(R_LBIND_UPVAL),
        JUMP_ENTRY(R_LBIND_GLOBAL),
        JUMP_ENTRY(R_LLEN),
        JUMP_ENTRY(R_LLOAD_R),
        JUMP_ENTRY(R_LLOAD_I),
        JUMP_ENTRY(R_LSTORE_R),
        JUMP_ENTRY(R_LSTORE_I),
        JUMP_ENTRY(R_LLOADU_R),
        JUMP_ENTRY(R_LLOADU_I),
        JUMP_ENTRY(R_LSTOREU_R),
        JUMP_ENTRY(R_LSTOREU_I),
        JUMP_ENTRY(R_LLOAD_PI),
        JUMP_ENTRY(R_LLOADU_PI),
        JUMP_ENTRY(R_LSTORE_PI),
        JUMP_ENTRY(R_LSTOREU_PI),

        // Registers
        JUMP_ENTRY(R_PUSH),
        JUMP_ENTRY(R_POP),
        JUMP_ENTRY(R_MOV),
    };
#undef JUMP_ENTRY
    for (size_t i = 0; i < MAX_OPCODES; ++i) assert(jumps[i] != NULL);
#define OP(c) CASE_##c:
#define DEFAULT CASE_##DEFAULT:
#define NEXT \
    do { \
        if (debugTraceExecution) trace(vm, frame); \
        uint16_t op = READ_OPCODE(); \
        if (op >= MAX_OPCODES) goto CASE_DEFAULT; \
        goto* jumps[op]; \
    } while (0)

    NEXT;
    OP(PUSH_CONST)
    {
        Value constant = READ_CONSTANT();
        push(vm, constant);
        NEXT;
    }
    OP(PUSH_NULL)
    {
        push(vm, NULL_VAL);
        NEXT;
    }
    OP(PUSH_TRUE)
    {
        push(vm, BOOL_VAL(true));
        NEXT;
    }
    OP(PUSH_FALSE)
    {
        push(vm, BOOL_VAL(false));
        NEXT;
    }
    OP(DUP)
    {
        push(vm, peek(vm, 0));
        NEXT;
    }
    OP(DUP2)
    {
        Value b = peek(vm, 0);
        Value a = peek(vm, 1);
        push(vm, a);
        push(vm, b);
        NEXT;
    }
    OP(POP)
    {
        pop(vm);
        NEXT;
    }
    OP(LD_LOCAL)
    {
        uint8_t slot = READ_BYTE();
        push(vm, frame->slots[slot]);
        NEXT;
    }
    OP(ST_LOCAL)
    {
        uint8_t slot = READ_BYTE();
        frame->slots[slot] = peek(vm, 0);
        NEXT;
    }
    OP(LD_GLOBAL)
    {
        ObjString *name = READ_STRING();
        Value slot;
        if (!tableGet(&vm->globals, name, &slot))
        {
            runtimeError(vm, "Undefined variable '%s'.", name->chars);
            return INTERPRET_RUNTIME_ERROR;
        }
        uint16_t slotInt = (uint16_t) AS_NUMBER(slot);
        frame->ip[-2] = LD_GLOBAL_I;
        frame->ip[-1] = slotInt;
        frame->ip -= 2;
        NEXT;
    }
    OP(LD_GLOBAL_I)
    {
        push(vm, vm->globalSlots.values[READ_SHORT()]);
        NEXT;
    }
    OP(DEF_GLOBAL)
    {
        ObjString *name = READ_STRING();
        Value slot;
        if (!tableGet(&vm->globals, name, &slot)) {
            int newSlot = vm->globalSlots.count;
            if (newSlot > UINT16_MAX)
            {
                runtimeError(vm, "Can't have more than %u globals.", UINT16_MAX + 1);
                return INTERPRET_RUNTIME_ERROR;
            }
            writeValueArray(&vm->gc, &vm->globalSlots, peek(vm, 0));
            pop(vm);
            slot = NUMBER_VAL((double)newSlot);
            tableSet(&vm->gc, &vm->globals, name, slot);
        } else {
            vm->globalSlots.values[(int) AS_NUMBER(slot)] = peek(vm, 0);
        }
        NEXT;
    }
    OP(ST_GLOBAL)
    {
        ObjString *name = READ_STRING();
        Value slot;
        if (!tableGet(&vm->globals, name, &slot))
        {
            runtimeError(vm, "Undefined variable '%s'.", name->chars);
            return INTERPRET_RUNTIME_ERROR;
        }
        uint16_t slotInt = (uint16_t) AS_NUMBER(slot);
        frame->ip[-2] = ST_GLOBAL_I;
        frame->ip[-1] = slotInt;
        frame->ip -= 2;
        NEXT;
    }
    OP(ST_GLOBAL_I)
    {
        vm->globalSlots.values[READ_SHORT()] = peek(vm, 0);
        NEXT;
    }
    OP(LD_UPVAL)
    {
        uint8_t slot = READ_BYTE();
        push(vm, *frame->closure->upvalues[slot]->location);
        NEXT;
    }
    OP(ST_UPVAL)
    {
        uint8_t slot = READ_BYTE();
        *frame->closure->upvalues[slot]->location = peek(vm, 0);
        NEXT;
    }
    OP(LD_PROP)
    {
        ObjString *name = READ_STRING();
        Value receiver = peek(vm, 0);

        if (IS_MAP(receiver)) {
            ObjMap* map = AS_MAP(receiver);
            Value value;
            if (tableGet(&map->table, name, &value)) {
                pop(vm); // Map.
                push(vm, value);
                NEXT;
            }
            runtimeError(vm, "Undefined key '%s' in map.", name->chars);
            return INTERPRET_RUNTIME_ERROR;
        }

        if (IS_INSTANCE(receiver)) {
            ObjInstance *instance = AS_INSTANCE(receiver);
            Value value;
            if (tableGet(&instance->fields, name, &value))
            {
                pop(vm); // Instance.
                push(vm, value);
                NEXT;
            }
            // If not a field, it might be a method.
            if (!bindMethod(vm, instance->klass, name)) return INTERPRET_RUNTIME_ERROR;
            NEXT;
        }

        // Check for built-in methods on other types
        ObjClass *klass = NULL;
        if (IS_LIST(receiver)) klass = vm->listClass;
        else if (IS_STRING(receiver)) klass = vm->stringClass;

        if (klass != NULL) {
            if (!bindMethod(vm, klass, name)) return INTERPRET_RUNTIME_ERROR;
            NEXT;
        }

        runtimeError(vm, "Only instances and maps have properties.");
        return INTERPRET_RUNTIME_ERROR;
    }
    OP(ST_PROP)
    {
        Value receiver = peek(vm, 1);
        ObjString* name = READ_STRING();

        if (IS_MAP(receiver)) {
            ObjMap* map = AS_MAP(receiver);
            tableSet(&vm->gc, &map->table, name, peek(vm, 0));
            Value value = pop(vm);
            pop(vm); // Map
            push(vm, value);
            NEXT;
        }

        if (!IS_INSTANCE(receiver))
        {
            runtimeError(vm, "Only instances and maps have fields.");
            return INTERPRET_RUNTIME_ERROR;
        }

        ObjInstance *instance = AS_INSTANCE(receiver);
        tableSet(&vm->gc, &instance->fields, name, peek(vm, 0));
        Value value = pop(vm);
        pop(vm);
        push(vm, value);
        NEXT;
    }
    OP(LD_INDEX)
    {
        if (IS_LIST(peek(vm, 1))) {
            if (!checkListIndex(vm, peek(vm, 1), peek(vm, 0))) return INTERPRET_RUNTIME_ERROR;
            int index = (int) AS_NUMBER(pop(vm));
            ObjList *list = AS_LIST(pop(vm));
            push(vm, list->elements.values[index]);
            NEXT;
        } else if (IS_MAP(peek(vm, 1))) {
            Value key_val = peek(vm, 0);
            PREPARE_MAP_KEY(key_val);
            ObjString *key = AS_STRING(key_val);
            ObjMap *map = AS_MAP(peek(vm, 1));
            Value value;
            bool exists = tableGet(&map->table, key, &value);
            POP_MAP_KEY_TEMP(peek(vm, 0));
            if (exists)
            {
                pop(vm); // Key.
                pop(vm); // Map.
                push(vm, value);
                NEXT;
            }
            runtimeError(vm, "Undefined key '%s'.", key->chars);
        } else if (IS_STRING(peek(vm, 1))) {
            ObjString *string = AS_STRING(peek(vm, 1));
            if (!checkStringIndex(vm, string, peek(vm, 0))) return INTERPRET_RUNTIME_ERROR;
            int index = (int) AS_NUMBER(pop(vm));
            char c = string->chars[index];
            pop(vm); // String.
            push(vm, NUMBER_VAL((double)c));
            NEXT;
        } else if (IS_INSTANCE(peek(vm, 1))) {
            if (!IS_STRING(peek(vm, 0)))
            {
                runtimeError(vm, "Instances can only be indexed by string.");
                return INTERPRET_RUNTIME_ERROR;
            }
            ObjString *name = AS_STRING(peek(vm, 0));
            ObjInstance *instance = AS_INSTANCE(peek(vm, 1));
            Value value;
            if (tableGet(&instance->fields, name, &value))
            {
                pop(vm); // Name.
                pop(vm); // Instance.
                push(vm, value);
                NEXT;
            }
            runtimeError(vm, "Undefined property '%s'.", name->chars);
        } else {
            runtimeError(vm, "Can only index lists, maps, strings and instances.");
        }
        return INTERPRET_RUNTIME_ERROR;
    }
    OP(ST_INDEX)
    {
        if (IS_LIST(peek(vm, 2))) {
            if (!checkListIndex(vm, peek(vm, 2), peek(vm, 1))) return INTERPRET_RUNTIME_ERROR;
            Value value = pop(vm);
            int index = (int) AS_NUMBER(pop(vm));
            ObjList *list = AS_LIST(pop(vm));
            list->elements.values[index] = value;
            push(vm, value);
            NEXT;
        } else if (IS_MAP(peek(vm, 2))) {
            Value key_val = peek(vm, 1);
            PREPARE_MAP_KEY(key_val);
            ObjString *key = AS_STRING(key_val);
            ObjMap *map = AS_MAP(peek(vm, 2));
            tableSet(&vm->gc, &map->table, key, peek(vm, 0));
            POP_MAP_KEY_TEMP(peek(vm, 1));
            Value value = pop(vm);
            pop(vm); // Key.
            pop(vm); // Map.
            push(vm, value);
            NEXT;
        } else if (IS_INSTANCE(peek(vm, 2))) {
            if (!IS_STRING(peek(vm, 1)))
            {
                runtimeError(vm, "Instances can only be indexed by string.");
                return INTERPRET_RUNTIME_ERROR;
            }
            ObjString *name = AS_STRING(peek(vm, 1));
            ObjInstance *instance = AS_INSTANCE(peek(vm, 2));
            tableSet(&vm->gc, &instance->fields, name, peek(vm, 0));
            Value value = pop(vm);
            pop(vm); // Name.
            pop(vm); // Instance.
            push(vm, value);
            NEXT;
        } else {
            runtimeError(vm, "Can only set index of lists, maps and instances.");
        }
        return INTERPRET_RUNTIME_ERROR;
    }
    OP(LD_SUPER)
    {
        ObjString *name = READ_STRING();
        ObjClass *superclass = AS_CLASS(pop(vm));

        if (!bindMethod(vm, superclass, name)) return INTERPRET_RUNTIME_ERROR;
        NEXT;
    }
    OP(EQ)
    {
        Value b = pop(vm);
        Value a = pop(vm);
        push(vm, BOOL_VAL(valuesEqual(a, b)));
        NEXT;
    }
    OP(NEQ)
    {
        Value b = pop(vm);
        Value a = pop(vm);
        push(vm, BOOL_VAL(!valuesEqual(a, b)));
        NEXT;
    }
    OP(EQ_C)
    {
        Value b = READ_CONSTANT();
        Value a = pop(vm);
        push(vm, BOOL_VAL(valuesEqual(a, b)));
        NEXT;
    }
    OP(NEQ_C)
    {
        Value b = READ_CONSTANT();
        Value a = pop(vm);
        push(vm, BOOL_VAL(!valuesEqual(a, b)));
        NEXT;
    }
    OP(GT)
    {
        BINARY_OP(BOOL_VAL, >);
        NEXT;
    }
    OP(GT_C)
    {
        BINARY_OP_C(BOOL_VAL, >);
        NEXT;
    }
    OP(LT)
    {
        BINARY_OP(BOOL_VAL, <);
        NEXT;
    }
    OP(LT_C)
    {
        BINARY_OP_C(BOOL_VAL, <);
        NEXT;
    }
    OP(ADD)
    {
        Value bValue = peek(vm, 0);
        Value aValue = peek(vm, 1);
        if (IS_STRING(bValue) && IS_STRING(aValue)) {
            concatenate(vm, aValue, bValue, true);
        } else if (IS_NUMBER(bValue) && IS_NUMBER(aValue)) {
            double b = AS_NUMBER(bValue);
            double a = AS_NUMBER(aValue);
            pop(vm);
            pop(vm);
            push(vm, NUMBER_VAL(a + b));
        } else {
            runtimeError(vm, "Operands must be two numbers or two strings.");
            return INTERPRET_RUNTIME_ERROR;
        }
        NEXT;
    }
    OP(ADD_C)
    {
        Value bValue = READ_CONSTANT();
        Value aValue = peek(vm, 0);
        if (IS_STRING(bValue) && IS_STRING(aValue)) {
            concatenate(vm, aValue, bValue, false);
        } else if (IS_NUMBER(bValue) && IS_NUMBER(aValue)) {
            double b = AS_NUMBER(bValue);
            double a = AS_NUMBER(aValue);
            pop(vm);
            push(vm, NUMBER_VAL(a + b));
        } else {
            runtimeError(vm, "Operands must be two numbers or two strings.");
            return INTERPRET_RUNTIME_ERROR;
        }
        NEXT;
    }
    OP(SUB)
    {
        BINARY_OP(NUMBER_VAL, -);
        NEXT;
    }
    OP(SUB_C)
    {
        BINARY_OP_C(NUMBER_VAL, -);
        NEXT;
    }
    OP(MUL)
    {
        BINARY_OP(NUMBER_VAL, *);
        NEXT;
    }
    OP(MUL_C)
    {
        BINARY_OP_C(NUMBER_VAL, *);
        NEXT;
    }
    OP(DIV)
    {
        BINARY_OP(NUMBER_VAL, /);
        NEXT;
    }
    OP(DIV_C)
    {
        BINARY_OP_C(NUMBER_VAL, /);
        NEXT;
    }
    OP(MOD)
    {
        if (!IS_NUMBER(peek(vm, 0)) || !IS_NUMBER(peek(vm, 1)))
        {
            runtimeError(vm, "Operands must be numbers.");
            return INTERPRET_RUNTIME_ERROR;
        }
        double b = AS_NUMBER(pop(vm));
        double a = AS_NUMBER(pop(vm));
        push(vm, NUMBER_VAL(fmod(a, b)));
        NEXT;
    }
    OP(MOD_C)
    {
        Value bValue = READ_CONSTANT();
        if (!IS_NUMBER(bValue) || !IS_NUMBER(peek(vm, 0)))
        {
            runtimeError(vm, "Operands must be numbers.");
            return INTERPRET_RUNTIME_ERROR;
        }
        double b = AS_NUMBER(bValue);
        double a = AS_NUMBER(pop(vm));
        push(vm, NUMBER_VAL(fmod(a, b)));
        NEXT;
    }
    OP(NOT)
    {
        push(vm, BOOL_VAL(isFalsey(pop(vm))));
        NEXT;
    }
    OP(NEG)
    {
        if (!IS_NUMBER(peek(vm, 0)))
        {
            runtimeError(vm, "Operand must be a number.");
            return INTERPRET_RUNTIME_ERROR;
        }
        push(vm, NUMBER_VAL(-AS_NUMBER(pop(vm))));
        NEXT;
    }
    OP(BAND)
    {
        BINARY_BIT_OP(&);
        NEXT;
    }
    OP(BAND_C)
    {
        BINARY_BIT_OP_C(&);
        NEXT;
    }
    OP(BOR)
    {
        BINARY_BIT_OP(|);
        NEXT;
    }
    OP(BOR_C)
    {
        BINARY_BIT_OP_C(|);
        NEXT;
    }
    OP(BXOR)
    {
        BINARY_BIT_OP(^);
        NEXT;
    }
    OP(BXOR_C)
    {
        BINARY_BIT_OP_C(^);
        NEXT;
    }
    OP(BSHL)
    {
        BINARY_BIT_OP(<<);
        NEXT;
    }
    OP(BSHL_C)
    {
        BINARY_BIT_OP_C(<<);
        NEXT;
    }
    OP(BSHR)
    {
        BINARY_BIT_OP(>>);
        NEXT;
    }
    OP(BSHR_C)
    {
        BINARY_BIT_OP_C(>>);
        NEXT;
    }
    OP(BNOT)
    {
        if (!IS_NUMBER(peek(vm, 0))) {
            runtimeError(vm, "Operand must be a number.");
            return INTERPRET_RUNTIME_ERROR;
        }
        int64_t a = (int64_t)AS_NUMBER(pop(vm));
        push(vm, NUMBER_VAL(~((int64_t)a)));
        NEXT;
    }
    OP(DBG_PRINT)
    {
        printValue(pop(vm));
        printf("\n");
        NEXT;
    }
    OP(JMP)
    {
        uint16_t offset = READ_SHORT();
        frame->ip += offset;
        NEXT;
    }
    OP(JMP_IF_FALSE)
    {
        uint16_t offset = READ_SHORT();
        if (isFalsey(peek(vm, 0))) frame->ip += offset;
        NEXT;
    }
    OP(PJMP_IF_FALSE)
    {
        uint16_t offset = READ_SHORT();
        if (isFalsey(peek(vm, 0))) frame->ip += offset;
        pop(vm);
        NEXT;
    }
    OP(JMP_IF_TRUE)
    {
        uint16_t offset = READ_SHORT();
        if (!isFalsey(peek(vm, 0))) frame->ip += offset;
        NEXT;
    }
    OP(PJMP_IF_TRUE)
    {
        uint16_t offset = READ_SHORT();
        if (!isFalsey(peek(vm, 0))) frame->ip += offset;
        pop(vm);
        NEXT;
    }
    OP(JMP_BACK)
    {
        uint16_t offset = READ_SHORT();
        frame->ip -= offset;
        NEXT;
    }
    OP(CALL)
    {
        int argCount = READ_BYTE();
        if (!callValue(vm, peek(vm, argCount), argCount)) return INTERPRET_RUNTIME_ERROR;
        frame = &vm->frames[vm->frameCount - 1];
        NEXT;
    }
    OP(FAST_NATIVE_CALL)
    {
        uint16_t nativeId = READ_SHORT();
        uint8_t argCount = READ_BYTE();
        FastNative* native = &G_FAST_NATIVES[nativeId];

        if (argCount != native->arity) {
            runtimeError(vm, "Expected %d arguments but got %d for %s().", native->arity, argCount, native->name);
            return INTERPRET_RUNTIME_ERROR;
        }

        if (!native->function(vm, argCount, vm->stackTop - argCount)) {
            return INTERPRET_RUNTIME_ERROR;
        }

        // The native function leaves its return value on top of the stack.
        // We need to pop the arguments from under it.
        Value result = pop(vm);
        vm->stackTop -= argCount;
        push(vm, result);
        NEXT;
    }
    OP(INVOKE)
    {
        ObjString *method = READ_STRING();
        int argCount = READ_BYTE();
        if (!invoke(vm, method, argCount)) return INTERPRET_RUNTIME_ERROR;
        frame = &vm->frames[vm->frameCount - 1];
        NEXT;
    }
    OP(INVOKE_SUPER)
    {
        ObjString *method = READ_STRING();
        int argCount = READ_BYTE();
        ObjClass *superclass = AS_CLASS(pop(vm));
        if (!invokeFromClass(vm, superclass, method, argCount)) return INTERPRET_RUNTIME_ERROR;
        frame = &vm->frames[vm->frameCount - 1];
        NEXT;
    }
    OP(MAKE_CLOSURE)
    {
        ObjFunction *function = AS_FUNCTION(READ_CONSTANT());
        ObjClosure *closure = newClosure(&vm->gc, function);
        push(vm, OBJ_VAL(closure));
        for (int i = 0; i < closure->upvalueCount; i++)
        {
            uint8_t isLocal = READ_BYTE();
            uint8_t index = READ_BYTE();
            if (isLocal) {
                closure->upvalues[i] = captureUpvalue(vm, frame->slots + index);
            } else {
                closure->upvalues[i] = frame->closure->upvalues[index];
            }
        }
        NEXT;
    }
    OP(CLOSE_UPVAL)
    {
        closeUpvalues(vm, vm->stackTop - 1);
        pop(vm);
        NEXT;
    }
    OP(MAKE_LIST)
    {
        push(vm, OBJ_VAL(newList(&vm->gc)));
        NEXT;
    }
    OP(LIST_DATA)
    {
        if (!IS_LIST(peek(vm, 1)))
        {
            runtimeError(vm, "List data can only be added to a list.");
            return INTERPRET_RUNTIME_ERROR;
        }
        ObjList *list = AS_LIST(peek(vm, 1));
        writeValueArray(&vm->gc, &list->elements, peek(vm, 0));
        pop(vm);
        NEXT;
    }
    OP(MAKE_MAP)
    {
        push(vm, OBJ_VAL(newMap(&vm->gc)));
        NEXT;
    }
    OP(MAP_DATA)
    {
        if (!IS_MAP(peek(vm, 2)))
        {
            runtimeError(vm, "Map data can only be added to a map.");
            return INTERPRET_RUNTIME_ERROR;
        }
        if (!IS_STRING(peek(vm, 1)))
        {
            runtimeError(vm, "Map key must be a string.");
            return INTERPRET_RUNTIME_ERROR;
        }
        ObjMap *map = AS_MAP(peek(vm, 2));
        ObjString *key = AS_STRING(peek(vm, 1));
        tableSet(&vm->gc, &map->table, key, peek(vm, 0));
        pop(vm); // Value.
        pop(vm); // Key.
        NEXT;
    }
    OP(RET)
    {
        Value result = pop(vm);
        closeUpvalues(vm, frame->slots);
        // ---- free scratch for this frame ----
        vm->ms_sp = frame->ms_sp_mark;

        vm->frameCount--;
        if (vm->frameCount == 0)
        {
            pop(vm);
            return INTERPRET_OK;
        }
        vm->stackTop = frame->slots;
        push(vm, result);
        frame = &vm->frames[vm->frameCount - 1];
        NEXT;
    }
    OP(MAKE_CLASS)
    {
        push(vm, OBJ_VAL(newClass(&vm->gc, READ_STRING())));
        NEXT;
    }
    OP(CLASS_INHERIT)
    {
        Value superclass = peek(vm, 1);
        if (!IS_CLASS(superclass))
        {
            runtimeError(vm, "Superclass must be a class.");
            return INTERPRET_RUNTIME_ERROR;
        }

        ObjClass *subclass = AS_CLASS(peek(vm, 0));
        tableAddAll(&vm->gc, &AS_CLASS(superclass)->methods, &subclass->methods);
        pop(vm); // Subclass.
        NEXT;
    }
    OP(DEF_METHOD)
    {
        defineMethod(vm, READ_STRING());
        NEXT;
    }

    // ---- BEGIN ++/-- OPCODES ----
#define HANDLE_INCDEC(val, op)                                       \
    if (!IS_NUMBER(val)) {                                           \
        runtimeError(vm, "Operand must be a number.");               \
        return INTERPRET_RUNTIME_ERROR;                              \
    }                                                                \
    push(vm, NUMBER_VAL(AS_NUMBER(val) op 1));

    // Prefix: push new value
    OP(PRE_INC_LOCAL) { uint8_t s = READ_BYTE(); HANDLE_INCDEC(frame->slots[s], +); frame->slots[s] = peek(vm, 0); NEXT; }
    OP(PRE_DEC_LOCAL) { uint8_t s = READ_BYTE(); HANDLE_INCDEC(frame->slots[s], -); frame->slots[s] = peek(vm, 0); NEXT; }
    OP(PRE_INC_UPVAL) {
        uint8_t s = READ_BYTE();
        HANDLE_INCDEC(*frame->closure->upvalues[s]->location, +);
        *frame->closure->upvalues[s]->location = peek(vm, 0);
        NEXT;
    }

    OP(PRE_DEC_UPVAL) {
        uint8_t s = READ_BYTE();
        HANDLE_INCDEC(*frame->closure->upvalues[s]->location, -);
        *frame->closure->upvalues[s]->location = peek(vm, 0);
        NEXT;
    }

    OP(PRE_INC_GLOBAL) {
        ObjString *name = READ_STRING();
        Value slotVal;
        if (!tableGet(&vm->globals, name, &slotVal)) {
            runtimeError(vm, "Undefined variable '%s'.", name->chars);
            return INTERPRET_RUNTIME_ERROR;
        }
        uint16_t slot = (uint16_t)AS_NUMBER(slotVal);
        HANDLE_INCDEC(vm->globalSlots.values[slot], +);
        vm->globalSlots.values[slot] = peek(vm, 0);
        frame->ip[-2] = PRE_INC_GLOBAL_I;
        frame->ip[-1] = slot;
        NEXT;
    }

    OP(PRE_DEC_GLOBAL) {
        ObjString *name = READ_STRING();
        Value slotVal;
        if (!tableGet(&vm->globals, name, &slotVal)) {
            runtimeError(vm, "Undefined variable '%s'.", name->chars);
            return INTERPRET_RUNTIME_ERROR;
        }
        uint16_t slot = (uint16_t)AS_NUMBER(slotVal);
        HANDLE_INCDEC(vm->globalSlots.values[slot], -);
        vm->globalSlots.values[slot] = peek(vm, 0);
        frame->ip[-2] = PRE_DEC_GLOBAL_I;
        frame->ip[-1] = slot;
        NEXT;
    }

    OP(PRE_INC_GLOBAL_I) { uint16_t s = READ_SHORT(); HANDLE_INCDEC(vm->globalSlots.values[s], +); vm->globalSlots.values[s] = peek(vm, 0); NEXT; }
    OP(PRE_DEC_GLOBAL_I) { uint16_t s = READ_SHORT(); HANDLE_INCDEC(vm->globalSlots.values[s], -); vm->globalSlots.values[s] = peek(vm, 0); NEXT; }

    OP(PRE_INC_PROP) {
        ObjString* name = READ_STRING();
        Value receiver = peek(vm, 0);
        if (IS_MAP(receiver)) {
            ObjMap* map = AS_MAP(receiver);
            Value current_val;
            if (!tableGet(&map->table, name, &current_val)) {
                runtimeError(vm, "Undefined key '%s' in map.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            HANDLE_INCDEC(current_val, +);
            tableSet(&vm->gc, &map->table, name, peek(vm,0));
        } else if (IS_INSTANCE(receiver)) {
            ObjInstance *instance = AS_INSTANCE(receiver);
            Value current_val;
            if (!tableGet(&instance->fields, name, &current_val)) {
                runtimeError(vm, "Undefined property '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            HANDLE_INCDEC(current_val, +);
            tableSet(&vm->gc, &instance->fields, name, peek(vm,0));
        } else {
            runtimeError(vm, "Only instances and maps have properties.");
            return INTERPRET_RUNTIME_ERROR;
        }
        NEXT;
    }

    OP(PRE_DEC_PROP) {
        ObjString* name = READ_STRING();
        Value receiver = peek(vm, 0);
        if (IS_MAP(receiver)) {
            ObjMap* map = AS_MAP(receiver);
            Value current_val;
            if (!tableGet(&map->table, name, &current_val)) {
                runtimeError(vm, "Undefined key '%s' in map.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            HANDLE_INCDEC(current_val, -);
            tableSet(&vm->gc, &map->table, name, peek(vm,0));
        } else if (IS_INSTANCE(receiver)) {
            ObjInstance *instance = AS_INSTANCE(receiver);
            Value current_val;
            if (!tableGet(&instance->fields, name, &current_val)) {
                runtimeError(vm, "Undefined property '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            HANDLE_INCDEC(current_val, -);
            tableSet(&vm->gc, &instance->fields, name, peek(vm,0));
        } else {
            runtimeError(vm, "Only instances and maps have properties.");
            return INTERPRET_RUNTIME_ERROR;
        }
        NEXT;
    }

    OP(PRE_INC_INDEX) {
        Value index_val = pop(vm);
        Value collection = pop(vm);
        if (IS_LIST(collection)) {
            if (!checkListIndex(vm, collection, index_val)) return INTERPRET_RUNTIME_ERROR;
            int index = (int)AS_NUMBER(index_val);
            ObjList* list = AS_LIST(collection);
            HANDLE_INCDEC(list->elements.values[index], +);
            list->elements.values[index] = peek(vm,0);
        } else if (IS_MAP(collection)) {
            PREPARE_MAP_KEY(index_val);
            ObjMap* map = AS_MAP(collection);
            ObjString* key = AS_STRING(index_val);
            Value current_val;
            if (!tableGet(&map->table, key, &current_val)) {
                POP_MAP_KEY_TEMP(index_val);
                runtimeError(vm, "Undefined key '%s' in map.", key->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            HANDLE_INCDEC(current_val, +); // Pushes the new value
            tableSet(&vm->gc, &map->table, key, peek(vm,0)); // Store the new value
            POP_MAP_KEY_TEMP(index_val);
        } else {
            runtimeError(vm, "Can only increment/decrement elements of lists or maps.");
            return INTERPRET_RUNTIME_ERROR;
        }
        NEXT;
    }

    OP(PRE_DEC_INDEX) {
        Value index_val = pop(vm);
        Value collection = pop(vm);
        if (IS_LIST(collection)) {
            if (!checkListIndex(vm, collection, index_val)) return INTERPRET_RUNTIME_ERROR;
            int index = (int)AS_NUMBER(index_val);
            ObjList* list = AS_LIST(collection);
            HANDLE_INCDEC(list->elements.values[index], -);
            list->elements.values[index] = peek(vm,0);
        } else if (IS_MAP(collection)) {
            PREPARE_MAP_KEY(index_val);
            ObjMap* map = AS_MAP(collection);
            ObjString* key = AS_STRING(index_val);
            Value current_val;
            if (!tableGet(&map->table, key, &current_val)) {
                POP_MAP_KEY_TEMP(index_val);
                runtimeError(vm, "Undefined key '%s' in map.", key->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            HANDLE_INCDEC(current_val, -);
            tableSet(&vm->gc, &map->table, key, peek(vm,0));
            POP_MAP_KEY_TEMP(index_val);
        } else {
            runtimeError(vm, "Can only increment/decrement elements of lists or maps.");
            return INTERPRET_RUNTIME_ERROR;
        }
        NEXT;
    }

    // Postfix: push old value
    OP(POST_INC_LOCAL) { uint8_t s = READ_BYTE(); Value v = frame->slots[s]; push(vm, v); HANDLE_INCDEC(v, +); frame->slots[s] = pop(vm); NEXT; }
    OP(POST_DEC_LOCAL) { uint8_t s = READ_BYTE(); Value v = frame->slots[s]; push(vm, v); HANDLE_INCDEC(v, -); frame->slots[s] = pop(vm); NEXT; }

    OP(POST_INC_UPVAL) {
        uint8_t s = READ_BYTE();
        Value v = *frame->closure->upvalues[s]->location;
        push(vm, v);
        HANDLE_INCDEC(v, +);
        *frame->closure->upvalues[s]->location = pop(vm);
        NEXT;
    }

    OP(POST_DEC_UPVAL) {
        uint8_t s = READ_BYTE();
        Value v = *frame->closure->upvalues[s]->location;
        push(vm, v);
        HANDLE_INCDEC(v, -);
        *frame->closure->upvalues[s]->location = pop(vm);
        NEXT;
    }

    OP(POST_INC_GLOBAL) {
        ObjString *name = READ_STRING();
        Value slotVal;
        if (!tableGet(&vm->globals, name, &slotVal)) {
            runtimeError(vm, "Undefined variable '%s'.", name->chars);
            return INTERPRET_RUNTIME_ERROR;
        }
        uint16_t slot = (uint16_t)AS_NUMBER(slotVal);
        Value v = vm->globalSlots.values[slot];
        push(vm, v);
        HANDLE_INCDEC(v, +);
        vm->globalSlots.values[slot] = pop(vm);
        frame->ip[-2] = POST_INC_GLOBAL_I;
        frame->ip[-1] = slot;
        NEXT;
    }
    OP(POST_DEC_GLOBAL) {
        ObjString *name = READ_STRING();
        Value slotVal;
        if (!tableGet(&vm->globals, name, &slotVal)) {
            runtimeError(vm, "Undefined variable '%s'.", name->chars);
            return INTERPRET_RUNTIME_ERROR;
        }
        uint16_t slot = (uint16_t)AS_NUMBER(slotVal);
        Value v = vm->globalSlots.values[slot];
        push(vm, v);
        HANDLE_INCDEC(v, -);
        vm->globalSlots.values[slot] = pop(vm);
        frame->ip[-2] = POST_DEC_GLOBAL_I;
        frame->ip[-1] = slot;
        NEXT;
    }

    OP(POST_INC_GLOBAL_I) { uint16_t s = READ_SHORT(); Value v = vm->globalSlots.values[s]; push(vm, v); HANDLE_INCDEC(v, +); vm->globalSlots.values[s] = pop(vm); NEXT; }
    OP(POST_DEC_GLOBAL_I) { uint16_t s = READ_SHORT(); Value v = vm->globalSlots.values[s]; push(vm, v); HANDLE_INCDEC(v, -); vm->globalSlots.values[s] = pop(vm); NEXT; }

    OP(POST_INC_PROP) {
        ObjString* name = READ_STRING();
        Value receiver = pop(vm);
        if (IS_MAP(receiver)) {
            ObjMap* map = AS_MAP(receiver);
            Value current_val;
            if (!tableGet(&map->table, name, &current_val)) {
                runtimeError(vm, "Undefined key '%s' in map.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            push(vm, current_val);
            HANDLE_INCDEC(current_val, +);
            tableSet(&vm->gc, &map->table, name, pop(vm));
        } else if (IS_INSTANCE(receiver)) {
            ObjInstance *instance = AS_INSTANCE(receiver);
            Value current_val;
            if (!tableGet(&instance->fields, name, &current_val)) {
                runtimeError(vm, "Undefined property '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            push(vm, current_val);
            HANDLE_INCDEC(current_val, +);
            tableSet(&vm->gc, &instance->fields, name, pop(vm));
        } else {
            runtimeError(vm, "Only instances and maps have properties.");
            return INTERPRET_RUNTIME_ERROR;
        }
        NEXT;
    }

    OP(POST_DEC_PROP) {
        ObjString* name = READ_STRING();
        Value receiver = pop(vm);
        if (IS_MAP(receiver)) {
            ObjMap* map = AS_MAP(receiver);
            Value current_val;
            if (!tableGet(&map->table, name, &current_val)) {
                runtimeError(vm, "Undefined key '%s' in map.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            push(vm, current_val);
            HANDLE_INCDEC(current_val, -);
            tableSet(&vm->gc, &map->table, name, pop(vm));
        } else if (IS_INSTANCE(receiver)) {
            ObjInstance *instance = AS_INSTANCE(receiver);
            Value current_val;
            if (!tableGet(&instance->fields, name, &current_val)) {
                runtimeError(vm, "Undefined property '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            push(vm, current_val);
            HANDLE_INCDEC(current_val, -);
            tableSet(&vm->gc, &instance->fields, name, pop(vm));
        } else {
            runtimeError(vm, "Only instances and maps have properties.");
            return INTERPRET_RUNTIME_ERROR;
        }
        NEXT;
    }

    OP(POST_INC_INDEX) {
        Value index_val = pop(vm);
        Value collection = pop(vm);
        if (IS_LIST(collection)) {
            if (!checkListIndex(vm, collection, index_val)) return INTERPRET_RUNTIME_ERROR;
            int index = (int)AS_NUMBER(index_val);
            ObjList* list = AS_LIST(collection);
            Value current_val = list->elements.values[index];
            push(vm, current_val);
            HANDLE_INCDEC(current_val, +);
            list->elements.values[index] = pop(vm);
        } else if (IS_MAP(collection)) {
            PREPARE_MAP_KEY(index_val);
            ObjMap* map = AS_MAP(collection);
            ObjString* key = AS_STRING(index_val);
            Value current_val;
            if (!tableGet(&map->table, key, &current_val)) {
                POP_MAP_KEY_TEMP(index_val);
                runtimeError(vm, "Undefined key '%s' in map.", key->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            push(vm, current_val);
            HANDLE_INCDEC(current_val, +);
            tableSet(&vm->gc, &map->table, key, pop(vm));
            POP_MAP_KEY_TEMP(index_val);
        } else {
            runtimeError(vm, "Can only increment/decrement elements of lists or maps.");
            return INTERPRET_RUNTIME_ERROR;
        }
        NEXT;
    }

    OP(POST_DEC_INDEX) {
        Value index_val = pop(vm);
        Value collection = pop(vm);
        if (IS_LIST(collection)) {
            if (!checkListIndex(vm, collection, index_val)) return INTERPRET_RUNTIME_ERROR;
            int index = (int)AS_NUMBER(index_val);
            ObjList* list = AS_LIST(collection);
            Value current_val = list->elements.values[index];
            push(vm, current_val);
            HANDLE_INCDEC(current_val, -);
            list->elements.values[index] = pop(vm);
        } else if (IS_MAP(collection)) {
            PREPARE_MAP_KEY(index_val);
            ObjMap* map = AS_MAP(collection);
            ObjString* key = AS_STRING(index_val);
            Value current_val;
            if (!tableGet(&map->table, key, &current_val)) {
                POP_MAP_KEY_TEMP(index_val);
                runtimeError(vm, "Undefined key '%s' in map.", key->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            push(vm, current_val);
            HANDLE_INCDEC(current_val, -);
            tableSet(&vm->gc, &map->table, key, pop(vm));
            POP_MAP_KEY_TEMP(index_val);
        } else {
            runtimeError(vm, "Can only increment/decrement elements of lists or maps.");
            return INTERPRET_RUNTIME_ERROR;
        }
        NEXT;
    }
#undef HANDLE_INCDEC
    // ---- END ++/-- OPCODES ----

    // microasm: typed immediate loads
    OP(R_ILOAD_IMM)
    {
        uint16_t regs = READ_SHORT();
        uint8_t dst = REG_POS1(regs);
        if (dst >= 8) {
            runtimeError(vm, "ILOAD requires an integer register (i0..i7).");
            return INTERPRET_RUNTIME_ERROR;
        }
        int64_t v; READ_I64(v);
        vm->ireg[dst] = v;
        NEXT;
    }

    OP(R_FLOAD_IMM)
    {
        uint16_t regs = READ_SHORT();
        uint8_t dst = REG_POS1(regs);
        if (dst < 8) {
            runtimeError(vm, "FLOAD requires a float register (f0..f7 or d0..d7).");
            return INTERPRET_RUNTIME_ERROR;
        }
        double v; READ_DBL(v);
        vm->dreg[dst - 8] = v;
        NEXT;
    }

    // microasm: fast register <-> local/global and ret
    OP(R_LD_LOCAL_R)
    {
        uint16_t regs = READ_SHORT();
        uint8_t dst = REG_POS1(regs);
        uint16_t slot = READ_SHORT(); // local index

        Value v = frame->slots[slot];
        if (!IS_NUMBER(v)) { runtimeError(vm, "Local is not a number."); return INTERPRET_RUNTIME_ERROR; }
        double d = AS_NUMBER(v);
        if (dst < 8) vm->ireg[dst] = (int64_t)d;
        else         vm->dreg[dst - 8] = d;
        NEXT;
    }
    OP(R_ST_LOCAL_R)
    {
        uint16_t regs = READ_SHORT();
        uint8_t src = REG_POS1(regs);
        uint16_t slot = READ_SHORT(); // local index

        double d = (src < 8) ? (double)vm->ireg[src] : vm->dreg[src - 8];
        frame->slots[slot] = NUMBER_VAL(d);
        NEXT;
    }
    OP(R_LD_UPVAL_R)
    {
        uint16_t regs = READ_SHORT();
        uint8_t dst = REG_POS1(regs);
        uint16_t idx = READ_SHORT(); // upvalue index (we store in 16 bits)

        if (frame->closure == NULL) { runtimeError(vm, "No closure for upvalue load."); return INTERPRET_RUNTIME_ERROR; }
        ObjUpvalue* uv = frame->closure->upvalues[(uint8_t)idx];
        Value v = *uv->location;

        if (!IS_NUMBER(v)) { runtimeError(vm, "Upvalue is not a number."); return INTERPRET_RUNTIME_ERROR; }
        double d = AS_NUMBER(v);
        if (dst < 8) vm->ireg[dst] = (int64_t)d;
        else         vm->dreg[dst - 8] = d;
        NEXT;
    }
    OP(R_ST_UPVAL_R)
    {
        uint16_t regs = READ_SHORT();
        uint8_t src = REG_POS1(regs);
        uint16_t idx = READ_SHORT(); // upvalue index

        if (frame->closure == NULL) { runtimeError(vm, "No closure for upvalue store."); return INTERPRET_RUNTIME_ERROR; }
        ObjUpvalue* uv = frame->closure->upvalues[(uint8_t)idx];

        double d = (src < 8) ? (double)vm->ireg[src] : vm->dreg[src - 8];
        *uv->location = NUMBER_VAL(d);
        NEXT;
    }
    OP(R_LD_GLOBAL_R)
    {
        uint16_t regs = READ_SHORT();
        uint8_t dst = REG_POS1(regs);
        uint16_t nameConst = READ_SHORT(); // constant index with symbol
        ObjString* name = AS_STRING(frame->function->chunk.constants.values[nameConst]);

        Value slotVal;
        if (!tableGet(&vm->globals, name, &slotVal)) {
            runtimeError(vm, "Undefined variable '%s'.", name->chars);
            return INTERPRET_RUNTIME_ERROR;
        }
        uint16_t slot = (uint16_t)AS_NUMBER(slotVal);
        Value v = vm->globalSlots.values[slot];

        if (!IS_NUMBER(v)) { runtimeError(vm, "Global is not a number."); return INTERPRET_RUNTIME_ERROR; }
        double d = AS_NUMBER(v);
        if (dst < 8) vm->ireg[dst] = (int64_t)d;
        else         vm->dreg[dst - 8] = d;
        NEXT;
    }
    OP(R_ST_GLOBAL_R)
    {
        uint16_t regs = READ_SHORT();
        uint8_t src = REG_POS1(regs);
        uint16_t nameConst = READ_SHORT(); // constant index with symbol
        ObjString* name = AS_STRING(frame->function->chunk.constants.values[nameConst]);

        Value slotVal;
        if (!tableGet(&vm->globals, name, &slotVal)) {
            runtimeError(vm, "Undefined variable '%s'.", name->chars);
            return INTERPRET_RUNTIME_ERROR;
        }
        uint16_t slot = (uint16_t)AS_NUMBER(slotVal);
        double d = (src < 8) ? (double)vm->ireg[src] : vm->dreg[src - 8];
        vm->globalSlots.values[slot] = NUMBER_VAL(d);
        NEXT;
    }
    OP(R_RET_R)
    {
        uint16_t regs = READ_SHORT();
        uint8_t r = REG_POS1(regs);

        double d = (r < 8) ? (double)vm->ireg[r] : vm->dreg[r - 8];
        Value result = NUMBER_VAL(d);

        closeUpvalues(vm, frame->slots);
        vm->frameCount--;
        if (vm->frameCount == 0) {
            pop(vm);
            return INTERPRET_OK;
        }

        vm->stackTop = frame->slots;
        push(vm, result);
        frame = &vm->frames[vm->frameCount - 1];
        NEXT;
    }

    // microasm: scratch typed
    OP(R_SST)
    {
        uint16_t regs = READ_SHORT();
        uint8_t src = REG_POS1(regs);
        uint16_t idx = READ_SHORT();

        if (src < 8) {
            if (!frame->msi || idx >= frame->msi_cap) {
                runtimeError(vm, "Integer scratch index %u out of range (cap=%u).", idx, frame->msi_cap);
                return INTERPRET_RUNTIME_ERROR;
            }
            frame->msi[idx] = vm->ireg[src];
        } else {
            if (!frame->msd || idx >= frame->msd_cap) {
                runtimeError(vm, "Float scratch index %u out of range (cap=%u).", idx, frame->msd_cap);
                return INTERPRET_RUNTIME_ERROR;
            }
            frame->msd[idx] = vm->dreg[src - 8];
        }
        NEXT;
    }
    OP(R_SLD)
    {
        uint16_t regs = READ_SHORT();
        uint8_t dst = REG_POS1(regs);
        uint16_t idx = READ_SHORT();

        if (dst < 8) {
            if (!frame->msi || idx >= frame->msi_cap) {
                runtimeError(vm, "Integer scratch index %u out of range (cap=%u).", idx, frame->msi_cap);
                return INTERPRET_RUNTIME_ERROR;
            }
            vm->ireg[dst] = frame->msi[idx];
        } else {
            if (!frame->msd || idx >= frame->msd_cap) {
                runtimeError(vm, "Float scratch index %u out of range (cap=%u).", idx, frame->msd_cap);
                return INTERPRET_RUNTIME_ERROR;
            }
            vm->dreg[dst - 8] = frame->msd[idx];
        }
        NEXT;
    }

    // ======== Typed ALU helpers (local to run()) ========
#define MAKE_I_RR_OP(OPCODE, OPER) \
    OP(OPCODE) { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst = REG_POS1(regs), a = REG_POS2(regs), b = REG_POS3(regs); \
        vm->ireg[dst] = vm->ireg[a] OPER vm->ireg[b]; \
        NEXT; \
    }

#define MAKE_I_RI_OP(OPCODE, OPER) \
    OP(OPCODE) { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst = REG_POS1(regs), a = REG_POS2(regs); \
        int64_t imm; READ_I64(imm); \
        vm->ireg[dst] = vm->ireg[a] OPER imm; \
        NEXT; \
    }

#define MAKE_I_IR_OP(OPCODE, OPER) \
    OP(OPCODE) { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst = REG_POS1(regs), b = REG_POS3(regs); \
        int64_t imm; READ_I64(imm); \
        vm->ireg[dst] = imm OPER vm->ireg[b]; \
        NEXT; \
    }

#define MAKE_I_II_OP(OPCODE, OPER) \
    OP(OPCODE) { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst = REG_POS1(regs); \
        int64_t a, b; READ_I64(a); READ_I64(b); \
        vm->ireg[dst] = a OPER b; \
        NEXT; \
    }

#define MAKE_F_RR_OP(OPCODE, OPER) \
    OP(OPCODE) { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst = REG_POS1(regs), a = REG_POS2(regs), b = REG_POS3(regs); \
        vm->dreg[dst - 8] = vm->dreg[a - 8] OPER vm->dreg[b - 8]; \
        NEXT; \
    }

#define MAKE_F_RI_OP(OPCODE, OPER) \
    OP(OPCODE) { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst = REG_POS1(regs), a = REG_POS2(regs); \
        double imm; READ_DBL(imm); \
        vm->dreg[dst - 8] = vm->dreg[a - 8] OPER imm; \
        NEXT; \
    }

#define MAKE_F_IR_OP(OPCODE, OPER) \
    OP(OPCODE) { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst = REG_POS1(regs), b = REG_POS3(regs); \
        double imm; READ_DBL(imm); \
        vm->dreg[dst - 8] = imm OPER vm->dreg[b - 8]; \
        NEXT; \
    }

#define MAKE_F_II_OP(OPCODE, OPER) \
    OP(OPCODE) { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst = REG_POS1(regs); \
        double a, b; READ_DBL(a); READ_DBL(b); \
        vm->dreg[dst - 8] = a OPER b; \
        NEXT; \
    }

// --- Special cases for MOD (int guard, float fmod) ---
#define MAKE_I_RR_MOD(OPCODE) \
    OP(OPCODE) { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst = REG_POS1(regs), a = REG_POS2(regs), b = REG_POS3(regs); \
        int64_t bv = vm->ireg[b]; \
        if (bv == 0) { runtimeError(vm, "Division by zero in integer modulo."); return INTERPRET_RUNTIME_ERROR; } \
        vm->ireg[dst] = vm->ireg[a] % bv; \
        NEXT; \
    }

#define MAKE_I_RI_MOD(OPCODE) \
    OP(OPCODE) { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst = REG_POS1(regs), a = REG_POS2(regs); \
        int64_t imm; READ_I64(imm); \
        if (imm == 0) { runtimeError(vm, "Division by zero in integer modulo."); return INTERPRET_RUNTIME_ERROR; } \
        vm->ireg[dst] = vm->ireg[a] % imm; \
        NEXT; \
    }

#define MAKE_I_IR_MOD(OPCODE) \
    OP(OPCODE) { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst = REG_POS1(regs), b = REG_POS3(regs); \
        int64_t imm; READ_I64(imm); \
        int64_t bv = vm->ireg[b]; \
        if (bv == 0) { runtimeError(vm, "Division by zero in integer modulo."); return INTERPRET_RUNTIME_ERROR; } \
        vm->ireg[dst] = imm % bv; \
        NEXT; \
    }

#define MAKE_I_II_MOD(OPCODE) \
    OP(OPCODE) { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst = REG_POS1(regs); \
        int64_t a, b; READ_I64(a); READ_I64(b); \
        if (b == 0) { runtimeError(vm, "Division by zero in integer modulo."); return INTERPRET_RUNTIME_ERROR; } \
        vm->ireg[dst] = a % b; \
        NEXT; \
    }

#define MAKE_F_RR_MOD(OPCODE) \
    OP(OPCODE) { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst = REG_POS1(regs), a = REG_POS2(regs), b = REG_POS3(regs); \
        vm->dreg[dst - 8] = fmod(vm->dreg[a - 8], vm->dreg[b - 8]); \
        NEXT; \
    }

#define MAKE_F_RI_MOD(OPCODE) \
    OP(OPCODE) { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst = REG_POS1(regs), a = REG_POS2(regs); \
        double imm; READ_DBL(imm); \
        vm->dreg[dst - 8] = fmod(vm->dreg[a - 8], imm); \
        NEXT; \
    }

#define MAKE_F_IR_MOD(OPCODE) \
    OP(OPCODE) { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst = REG_POS1(regs), b = REG_POS3(regs); \
        double imm; READ_DBL(imm); \
        vm->dreg[dst - 8] = fmod(imm, vm->dreg[b - 8]); \
        NEXT; \
    }

#define MAKE_F_II_MOD(OPCODE) \
    OP(OPCODE) { \
        uint16_t regs = READ_SHORT(); \
        uint8_t dst = REG_POS1(regs); \
        double a, b; READ_DBL(a); READ_DBL(b); \
        vm->dreg[dst - 8] = fmod(a, b); \
        NEXT; \
    }

    // ======== Typed ALU: INT ========
    MAKE_I_RR_OP(R_IADD_RR, +)
    MAKE_I_RI_OP(R_IADD_RI, +)
    MAKE_I_IR_OP(R_IADD_IR, +)
    MAKE_I_II_OP(R_IADD_II, +)

    MAKE_I_RR_OP(R_ISUB_RR, -)
    MAKE_I_RI_OP(R_ISUB_RI, -)
    MAKE_I_IR_OP(R_ISUB_IR, -)
    MAKE_I_II_OP(R_ISUB_II, -)

    MAKE_I_RR_OP(R_IMUL_RR, *)
    MAKE_I_RI_OP(R_IMUL_RI, *)
    MAKE_I_IR_OP(R_IMUL_IR, *)
    MAKE_I_II_OP(R_IMUL_II, *)

    MAKE_I_RR_OP(R_IDIV_RR, /)
    MAKE_I_RI_OP(R_IDIV_RI, /)
    MAKE_I_IR_OP(R_IDIV_IR, /)
    MAKE_I_II_OP(R_IDIV_II, /)

    MAKE_I_RR_MOD(R_IMOD_RR)
    MAKE_I_RI_MOD(R_IMOD_RI)
    MAKE_I_IR_MOD(R_IMOD_IR)
    MAKE_I_II_MOD(R_IMOD_II)

    // ======== Typed ALU: FLOAT ========
    MAKE_F_RR_OP(R_FADD_RR, +)
    MAKE_F_RI_OP(R_FADD_RI, +)
    MAKE_F_IR_OP(R_FADD_IR, +)
    MAKE_F_II_OP(R_FADD_II, +)

    MAKE_F_RR_OP(R_FSUB_RR, -)
    MAKE_F_RI_OP(R_FSUB_RI, -)
    MAKE_F_IR_OP(R_FSUB_IR, -)
    MAKE_F_II_OP(R_FSUB_II, -)

    MAKE_F_RR_OP(R_FMUL_RR, *)
    MAKE_F_RI_OP(R_FMUL_RI, *)
    MAKE_F_IR_OP(R_FMUL_IR, *)
    MAKE_F_II_OP(R_FMUL_II, *)

    MAKE_F_RR_OP(R_FDIV_RR, /)
    MAKE_F_RI_OP(R_FDIV_RI, /)
    MAKE_F_IR_OP(R_FDIV_IR, /)
    MAKE_F_II_OP(R_FDIV_II, /)

    MAKE_F_RR_MOD(R_FMOD_RR)
    MAKE_F_RI_MOD(R_FMOD_RI)
    MAKE_F_IR_MOD(R_FMOD_IR)
    MAKE_F_II_MOD(R_FMOD_II)

    // ======== Helpers for shifts/rotates/bitops (INT64) ========
#define SH_MASK(v)   ((uint32_t)((v) & 63))

    // ---- SHL (logical left) ----
#define MAKE_I_RR_SHL(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r), b=REG_POS3(r); \
        uint32_t n = SH_MASK(vm->ireg[b]); \
        vm->ireg[d] = (int64_t)((uint64_t)vm->ireg[a] << n); \
        NEXT; \
    }
#define MAKE_I_RI_SHL(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r); int64_t imm; READ_I64(imm); \
        uint32_t n = SH_MASK(imm); \
        vm->ireg[d] = (int64_t)((uint64_t)vm->ireg[a] << n); \
        NEXT; \
    }
#define MAKE_I_IR_SHL(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), b=REG_POS3(r); int64_t imm; READ_I64(imm); \
        uint32_t n = SH_MASK(vm->ireg[b]); \
        vm->ireg[d] = (int64_t)((uint64_t)imm << n); \
        NEXT; \
    }
#define MAKE_I_II_SHL(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r); int64_t a, b; READ_I64(a); READ_I64(b); \
        uint32_t n = SH_MASK(b); \
        vm->ireg[d] = (int64_t)((uint64_t)a << n); \
        NEXT; \
    }

    // ---- SHR (logical right) ----
#define MAKE_I_RR_SHR(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r), b=REG_POS3(r); \
        uint32_t n = SH_MASK(vm->ireg[b]); \
        vm->ireg[d] = (int64_t)((uint64_t)vm->ireg[a] >> n); \
        NEXT; \
    }
#define MAKE_I_RI_SHR(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r); int64_t imm; READ_I64(imm); \
        uint32_t n = SH_MASK(imm); \
        vm->ireg[d] = (int64_t)((uint64_t)vm->ireg[a] >> n); \
        NEXT; \
    }
#define MAKE_I_IR_SHR(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), b=REG_POS3(r); int64_t imm; READ_I64(imm); \
        uint32_t n = SH_MASK(vm->ireg[b]); \
        vm->ireg[d] = (int64_t)((uint64_t)imm >> n); \
        NEXT; \
    }
#define MAKE_I_II_SHR(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r); int64_t a, b; READ_I64(a); READ_I64(b); \
        uint32_t n = SH_MASK(b); \
        vm->ireg[d] = (int64_t)((uint64_t)a >> n); \
        NEXT; \
    }

    // ---- SAR (arithmetic right) ----
#define MAKE_I_RR_SAR(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r), b=REG_POS3(r); \
        uint32_t n = SH_MASK(vm->ireg[b]); \
        vm->ireg[d] = (int64_t)((int64_t)vm->ireg[a] >> n); \
        NEXT; \
    }
#define MAKE_I_RI_SAR(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r); int64_t imm; READ_I64(imm); \
        uint32_t n = SH_MASK(imm); \
        vm->ireg[d] = (int64_t)((int64_t)vm->ireg[a] >> n); \
        NEXT; \
    }
#define MAKE_I_IR_SAR(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), b=REG_POS3(r); int64_t imm; READ_I64(imm); \
        uint32_t n = SH_MASK(vm->ireg[b]); \
        vm->ireg[d] = (int64_t)((int64_t)imm >> n); \
        NEXT; \
    }
#define MAKE_I_II_SAR(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r); int64_t a, b; READ_I64(a); READ_I64(b); \
        uint32_t n = SH_MASK(b); \
        vm->ireg[d] = (int64_t)((int64_t)a >> n); \
        NEXT; \
    }

    // ---- ROL / ROR (rotate) ----
#define MAKE_I_RR_ROL(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r), b=REG_POS3(r); \
        uint64_t A=(uint64_t)vm->ireg[a]; uint32_t n=SH_MASK(vm->ireg[b]); \
        vm->ireg[d] = (int64_t)( n ? ((A<<n) | (A>>(64-n))) : A ); \
        NEXT; \
    }
#define MAKE_I_RI_ROL(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r); int64_t imm; READ_I64(imm); \
        uint64_t A=(uint64_t)vm->ireg[a]; uint32_t n=SH_MASK(imm); \
        vm->ireg[d] = (int64_t)( n ? ((A<<n) | (A>>(64-n))) : A ); \
        NEXT; \
    }
#define MAKE_I_IR_ROL(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), b=REG_POS3(r); int64_t imm; READ_I64(imm); \
        uint64_t A=(uint64_t)imm; uint32_t n=SH_MASK(vm->ireg[b]); \
        vm->ireg[d] = (int64_t)( n ? ((A<<n) | (A>>(64-n))) : A ); \
        NEXT; \
    }
#define MAKE_I_II_ROL(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r); int64_t a,b; READ_I64(a); READ_I64(b); \
        uint64_t A=(uint64_t)a; uint32_t n=SH_MASK(b); \
        vm->ireg[d] = (int64_t)( n ? ((A<<n) | (A>>(64-n))) : A ); \
        NEXT; \
    }

#define MAKE_I_RR_ROR(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r), b=REG_POS3(r); \
        uint64_t A=(uint64_t)vm->ireg[a]; uint32_t n=SH_MASK(vm->ireg[b]); \
        vm->ireg[d] = (int64_t)( n ? ((A>>n) | (A<<(64-n))) : A ); \
        NEXT; \
    }
#define MAKE_I_RI_ROR(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r); int64_t imm; READ_I64(imm); \
        uint64_t A=(uint64_t)vm->ireg[a]; uint32_t n=SH_MASK(imm); \
        vm->ireg[d] = (int64_t)( n ? ((A>>n) | (A<<(64-n))) : A ); \
        NEXT; \
    }
#define MAKE_I_IR_ROR(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), b=REG_POS3(r); int64_t imm; READ_I64(imm); \
        uint64_t A=(uint64_t)imm; uint32_t n=SH_MASK(vm->ireg[b]); \
        vm->ireg[d] = (int64_t)( n ? ((A>>n) | (A<<(64-n))) : A ); \
        NEXT; \
    }
#define MAKE_I_II_ROR(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r); int64_t a,b; READ_I64(a); READ_I64(b); \
        uint64_t A=(uint64_t)a; uint32_t n=SH_MASK(b); \
        vm->ireg[d] = (int64_t)( n ? ((A>>n) | (A<<(64-n))) : A ); \
        NEXT; \
    }

    // ---- Bit test / set / clear / toggle ----
    // btst: dst = ((uint64)a >> (idx&63)) & 1
#define MAKE_I_RR_BTST(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r), b=REG_POS3(r); \
        uint32_t n = SH_MASK(vm->ireg[b]); \
        vm->ireg[d] = (int64_t)((((uint64_t)vm->ireg[a]) >> n) & 1ULL); \
        NEXT; \
    }
#define MAKE_I_RI_BTST(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r); int64_t imm; READ_I64(imm); \
        uint32_t n = SH_MASK(imm); \
        vm->ireg[d] = (int64_t)((((uint64_t)vm->ireg[a]) >> n) & 1ULL); \
        NEXT; \
    }
#define MAKE_I_IR_BTST(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), b=REG_POS3(r); int64_t imm; READ_I64(imm); \
        uint32_t n = SH_MASK(vm->ireg[b]); \
        vm->ireg[d] = (int64_t)((((uint64_t)imm) >> n) & 1ULL); \
        NEXT; \
    }
#define MAKE_I_II_BTST(OPC) \
    OP(OPC) { \
        uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r); int64_t a,b; READ_I64(a); READ_I64(b); \
        uint32_t n = SH_MASK(b); \
        vm->ireg[d] = (int64_t)((((uint64_t)a) >> n) & 1ULL); \
        NEXT; \
    }

    // helpers for bset/bclr/btgl
#define _MASK_FROM_IDX(idx64) ( (uint64_t)1ULL << SH_MASK((idx64)) )

#define MAKE_I_RR_BSET(OPC) \
    OP(OPC) { uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r), b=REG_POS3(r); \
        vm->ireg[d] = (int64_t)((uint64_t)vm->ireg[a] | _MASK_FROM_IDX(vm->ireg[b])); NEXT; }
#define MAKE_I_RI_BSET(OPC) \
    OP(OPC) { uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r); int64_t i; READ_I64(i); \
        vm->ireg[d] = (int64_t)((uint64_t)vm->ireg[a] | _MASK_FROM_IDX(i)); NEXT; }
#define MAKE_I_IR_BSET(OPC) \
    OP(OPC) { uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), b=REG_POS3(r); int64_t a; READ_I64(a); \
        vm->ireg[d] = (int64_t)((uint64_t)a | _MASK_FROM_IDX(vm->ireg[b])); NEXT; }
#define MAKE_I_II_BSET(OPC) \
    OP(OPC) { uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r); int64_t a,b; READ_I64(a); READ_I64(b); \
        vm->ireg[d] = (int64_t)((uint64_t)a | _MASK_FROM_IDX(b)); NEXT; }

#define MAKE_I_RR_BCLR(OPC) \
    OP(OPC) { uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r), b=REG_POS3(r); \
        vm->ireg[d] = (int64_t)((uint64_t)vm->ireg[a] & ~_MASK_FROM_IDX(vm->ireg[b])); NEXT; }
#define MAKE_I_RI_BCLR(OPC) \
    OP(OPC) { uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r); int64_t i; READ_I64(i); \
        vm->ireg[d] = (int64_t)((uint64_t)vm->ireg[a] & ~_MASK_FROM_IDX(i)); NEXT; }
#define MAKE_I_IR_BCLR(OPC) \
    OP(OPC) { uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), b=REG_POS3(r); int64_t a; READ_I64(a); \
        vm->ireg[d] = (int64_t)((uint64_t)a & ~_MASK_FROM_IDX(vm->ireg[b])); NEXT; }
#define MAKE_I_II_BCLR(OPC) \
    OP(OPC) { uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r); int64_t a,b; READ_I64(a); READ_I64(b); \
        vm->ireg[d] = (int64_t)((uint64_t)a & ~_MASK_FROM_IDX(b)); NEXT; }

#define MAKE_I_RR_BTGL(OPC) \
    OP(OPC) { uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r), b=REG_POS3(r); \
        vm->ireg[d] = (int64_t)((uint64_t)vm->ireg[a] ^ _MASK_FROM_IDX(vm->ireg[b])); NEXT; }
#define MAKE_I_RI_BTGL(OPC) \
    OP(OPC) { uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), a=REG_POS2(r); int64_t i; READ_I64(i); \
        vm->ireg[d] = (int64_t)((uint64_t)vm->ireg[a] ^ _MASK_FROM_IDX(i)); NEXT; }
#define MAKE_I_IR_BTGL(OPC) \
    OP(OPC) { uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r), b=REG_POS3(r); int64_t a; READ_I64(a); \
        vm->ireg[d] = (int64_t)((uint64_t)a ^ _MASK_FROM_IDX(vm->ireg[b])); NEXT; }
#define MAKE_I_II_BTGL(OPC) \
    OP(OPC) { uint16_t r=READ_SHORT(); uint8_t d=REG_POS1(r); int64_t a,b; READ_I64(a); READ_I64(b); \
        vm->ireg[d] = (int64_t)((uint64_t)a ^ _MASK_FROM_IDX(b)); NEXT; }

    // ======== INT64 bitwise (register/immediate variants) ========
    MAKE_I_RR_OP(R_IAND_RR, &)
    MAKE_I_RI_OP(R_IAND_RI, &)
    MAKE_I_IR_OP(R_IAND_IR, &)
    MAKE_I_II_OP(R_IAND_II, &)

    MAKE_I_RR_OP(R_IOR_RR, |)
    MAKE_I_RI_OP(R_IOR_RI, |)
    MAKE_I_IR_OP(R_IOR_IR, |)
    MAKE_I_II_OP(R_IOR_II, |)

    MAKE_I_RR_OP(R_IXOR_RR, ^)
    MAKE_I_RI_OP(R_IXOR_RI, ^)
    MAKE_I_IR_OP(R_IXOR_IR, ^)
    MAKE_I_II_OP(R_IXOR_II, ^)

    // ======== INT64 shifts ========
    // SHL
    MAKE_I_RR_SHL(R_ISHL_RR)
    MAKE_I_RI_SHL(R_ISHL_RI)
    MAKE_I_IR_SHL(R_ISHL_IR)
    MAKE_I_II_SHL(R_ISHL_II)
    // SHR (logical)
    MAKE_I_RR_SHR(R_ISHR_RR)
    MAKE_I_RI_SHR(R_ISHR_RI)
    MAKE_I_IR_SHR(R_ISHR_IR)
    MAKE_I_II_SHR(R_ISHR_II)
    // SAR (arithmetic)
    MAKE_I_RR_SAR(R_ISAR_RR)
    MAKE_I_RI_SAR(R_ISAR_RI)
    MAKE_I_IR_SAR(R_ISAR_IR)
    MAKE_I_II_SAR(R_ISAR_II)

    // ======== INT64 rotates ========
    MAKE_I_RR_ROL(R_IROL_RR)
    MAKE_I_RI_ROL(R_IROL_RI)
    MAKE_I_IR_ROL(R_IROL_IR)
    MAKE_I_II_ROL(R_IROL_II)

    MAKE_I_RR_ROR(R_IROR_RR)
    MAKE_I_RI_ROR(R_IROR_RI)
    MAKE_I_IR_ROR(R_IROR_IR)
    MAKE_I_II_ROR(R_IROR_II)

    // ======== INT64 bit test / set / clear / toggle ========
    MAKE_I_RR_BTST(R_IBTST_RR)
    MAKE_I_RI_BTST(R_IBTST_RI)
    MAKE_I_IR_BTST(R_IBTST_IR)
    MAKE_I_II_BTST(R_IBTST_II)

    MAKE_I_RR_BSET(R_IBSET_RR)
    MAKE_I_RI_BSET(R_IBSET_RI)
    MAKE_I_IR_BSET(R_IBSET_IR)
    MAKE_I_II_BSET(R_IBSET_II)

    MAKE_I_RR_BCLR(R_IBCLR_RR)
    MAKE_I_RI_BCLR(R_IBCLR_RI)
    MAKE_I_IR_BCLR(R_IBCLR_IR)
    MAKE_I_II_BCLR(R_IBCLR_II)

    MAKE_I_RR_BTGL(R_IBTGL_RR)
    MAKE_I_RI_BTGL(R_IBTGL_RI)
    MAKE_I_IR_BTGL(R_IBTGL_IR)
    MAKE_I_II_BTGL(R_IBTGL_II)

#undef MAKE_I_RR_OP
#undef MAKE_I_RI_OP
#undef MAKE_I_IR_OP
#undef MAKE_I_II_OP
#undef MAKE_F_RR_OP
#undef MAKE_F_RI_OP
#undef MAKE_F_IR_OP
#undef MAKE_F_II_OP
#undef MAKE_I_RR_MOD
#undef MAKE_I_RI_MOD
#undef MAKE_I_IR_MOD
#undef MAKE_I_II_MOD
#undef MAKE_F_RR_MOD
#undef MAKE_F_RI_MOD
#undef MAKE_F_IR_MOD
#undef MAKE_F_II_MOD

#undef SH_MASK
#undef MAKE_I_RR_SHL
#undef MAKE_I_RI_SHL
#undef MAKE_I_IR_SHL
#undef MAKE_I_II_SHL
#undef MAKE_I_RR_SHR
#undef MAKE_I_RI_SHR
#undef MAKE_I_IR_SHR
#undef MAKE_I_II_SHR
#undef MAKE_I_RR_SAR
#undef MAKE_I_RI_SAR
#undef MAKE_I_IR_SAR
#undef MAKE_I_II_SAR
#undef MAKE_I_RR_ROL
#undef MAKE_I_RI_ROL
#undef MAKE_I_IR_ROL
#undef MAKE_I_II_ROL
#undef MAKE_I_RR_ROR
#undef MAKE_I_RI_ROR
#undef MAKE_I_IR_ROR
#undef MAKE_I_II_ROR
#undef MAKE_I_RR_BTST
#undef MAKE_I_RI_BTST
#undef MAKE_I_IR_BTST
#undef MAKE_I_II_BTST
#undef _MASK_FROM_IDX
#undef MAKE_I_RR_BSET
#undef MAKE_I_RI_BSET
#undef MAKE_I_IR_BSET
#undef MAKE_I_II_BSET
#undef MAKE_I_RR_BCLR
#undef MAKE_I_RI_BCLR
#undef MAKE_I_IR_BCLR
#undef MAKE_I_II_BCLR
#undef MAKE_I_RR_BTGL
#undef MAKE_I_RI_BTGL
#undef MAKE_I_IR_BTGL
#undef MAKE_I_II_BTGL

    // ======== INT64 unary bitwise not ========
    OP(R_IBNOT)
    {
        uint16_t regs = READ_SHORT();
        uint8_t dst = REG_POS1(regs), src = REG_POS2(regs);
        vm->ireg[dst] = ~vm->ireg[src];
        NEXT;
    }

    // ---- Bound list helpers (unified, local to run()) ----
    // We rely on MS_MAX_LBIND from vm.h (16). Handles are encoded in regs nibble.
#define HND(regs)   (REG_POS3(regs))  // handle in nibble #3 for list ops

#define ENSURE_HANDLE(h) \
    do { \
        if ((h) >= MS_MAX_LBIND) { \
            runtimeError(vm, "Invalid list handle h%u (max h%u).", (unsigned)(h), (unsigned)(MS_MAX_LBIND-1)); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
    } while (0)

#define ENSURE_BOUND(h) \
    do { \
        ENSURE_HANDLE(h); \
        if (!frame->mh[(h)].list) { \
            runtimeError(vm, "No list bound in h%u (use lbind).", (unsigned)(h)); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
    } while (0)

#define H_LIST(h)   (frame->mh[(h)].list)
#define H_BASE(h)   (frame->mh[(h)].elems)
#define H_LEN(h)    (frame->mh[(h)].len)

#define IDX_FROM_IREG(ir, out)  do { (out) = vm->ireg[(ir)]; } while (0)
#define IDX_FROM_IMM(out)       do { READ_I64(out); } while (0)

#define CHECK_BOUNDS(h, idx64) \
    do { \
        uint64_t _n = (uint64_t)H_LEN(h); \
        int64_t _i = (idx64); \
        if (_i < 0 || (uint64_t)_i >= _n) { \
            runtimeError(vm, "List index %lld out of range (size=%u) on h%u.", \
            (long long)_i, (unsigned)H_LEN(h), (unsigned)(h)); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
    } while (0)

#define LOAD_ELEM(h, i64)       (H_BASE(h)[(i64)])
#define STORE_ELEM(h, i64, v)   (H_BASE(h)[(i64)] = (v))

    OP(R_LBIND_LOCAL)
    {
        uint16_t regs = READ_SHORT();
        uint8_t lenDst = REG_POS1(regs);
        uint8_t h      = HND(regs);
        uint16_t slot  = READ_SHORT();

        ENSURE_HANDLE(h);
        Value v = frame->slots[slot];
        if (!IS_LIST(v)) { runtimeError(vm, "lbind: target is not a list."); return INTERPRET_RUNTIME_ERROR; }
        ObjList* L = AS_LIST(v);

        frame->mh[h].list  = L;
        frame->mh[h].elems = L->elements.values;
        frame->mh[h].len   = (uint32_t)L->elements.count;

        if (lenDst < 8) vm->ireg[lenDst] = (int64_t)frame->mh[h].len;
        NEXT;
    }

    OP(R_LBIND_UPVAL)
    {
        uint16_t regs = READ_SHORT();
        uint8_t lenDst = REG_POS1(regs);
        uint8_t h      = HND(regs);
        uint16_t idx   = READ_SHORT();

        ENSURE_HANDLE(h);
        if (!frame->closure) { runtimeError(vm, "lbind upvalue: no closure."); return INTERPRET_RUNTIME_ERROR; }
        ObjUpvalue* uv = frame->closure->upvalues[(uint8_t)idx];
        Value v = *uv->location;
        if (!IS_LIST(v)) { runtimeError(vm, "lbind: upvalue is not a list."); return INTERPRET_RUNTIME_ERROR; }
        ObjList* L = AS_LIST(v);

        frame->mh[h].list  = L;
        frame->mh[h].elems = L->elements.values;
        frame->mh[h].len   = (uint32_t)L->elements.count;

        if (lenDst < 8) vm->ireg[lenDst] = (int64_t)frame->mh[h].len;
        NEXT;
    }

    OP(R_LBIND_GLOBAL)
    {
        uint16_t regs = READ_SHORT();
        uint8_t lenDst = REG_POS1(regs);
        uint8_t h      = HND(regs);
        uint16_t nameConst = READ_SHORT();
        ObjString* name = AS_STRING(frame->function->chunk.constants.values[nameConst]);

        ENSURE_HANDLE(h);
        Value slotVal;
        if (!tableGet(&vm->globals, name, &slotVal)) {
            runtimeError(vm, "Undefined variable '%s'.", name->chars);
            return INTERPRET_RUNTIME_ERROR;
        }
        uint16_t slot = (uint16_t)AS_NUMBER(slotVal);
        Value v = vm->globalSlots.values[slot];
        if (!IS_LIST(v)) { runtimeError(vm, "lbind: global is not a list."); return INTERPRET_RUNTIME_ERROR; }
        ObjList* L = AS_LIST(v);

        frame->mh[h].list  = L;
        frame->mh[h].elems = L->elements.values;
        frame->mh[h].len   = (uint32_t)L->elements.count;

        if (lenDst < 8) vm->ireg[lenDst] = (int64_t)frame->mh[h].len;
        NEXT;
    }

    OP(R_LLEN)
    {
        uint16_t regs = READ_SHORT();
        uint8_t dst = REG_POS1(regs);
        uint8_t h   = HND(regs);
        if (dst >= 8) { runtimeError(vm, "llen: destination must be i0..i7."); return INTERPRET_RUNTIME_ERROR; }
        ENSURE_BOUND(h);
        vm->ireg[dst] = (int64_t)H_LEN(h);
        NEXT;
    }

    OP(R_LLOAD_R)
    {
        uint16_t regs = READ_SHORT();
        uint8_t dst = REG_POS1(regs);
        uint8_t ir  = REG_POS2(regs);
        uint8_t h   = HND(regs);
        ENSURE_BOUND(h);

        int64_t idx; IDX_FROM_IREG(ir, idx);
        CHECK_BOUNDS(h, idx);

        Value v = LOAD_ELEM(h, idx);
        if (!IS_NUMBER(v)) { runtimeError(vm, "List element is not a number."); return INTERPRET_RUNTIME_ERROR; }
        double d = AS_NUMBER(v);
        if (dst < 8) vm->ireg[dst] = (int64_t)d; else vm->dreg[dst - 8] = d;
        NEXT;
    }

    OP(R_LLOAD_I)
    {
        uint16_t regs = READ_SHORT();
        uint8_t dst = REG_POS1(regs);
        uint8_t h   = HND(regs);
        ENSURE_BOUND(h);

        int64_t idx; IDX_FROM_IMM(idx);
        CHECK_BOUNDS(h, idx);

        Value v = LOAD_ELEM(h, idx);
        if (!IS_NUMBER(v)) { runtimeError(vm, "List element is not a number."); return INTERPRET_RUNTIME_ERROR; }
        double d = AS_NUMBER(v);
        if (dst < 8) vm->ireg[dst] = (int64_t)d; else vm->dreg[dst - 8] = d;
        NEXT;
    }

    OP(R_LSTORE_R)
    {
        uint16_t regs = READ_SHORT();
        uint8_t src = REG_POS1(regs);
        uint8_t ir  = REG_POS2(regs);
        uint8_t h   = HND(regs);
        ENSURE_BOUND(h);

        int64_t idx; IDX_FROM_IREG(ir, idx);
        CHECK_BOUNDS(h, idx);

        double d = (src < 8) ? (double)vm->ireg[src] : vm->dreg[src - 8];
        STORE_ELEM(h, idx, NUMBER_VAL(d));
        NEXT;
    }

    OP(R_LSTORE_I)
    {
        uint16_t regs = READ_SHORT();
        uint8_t src = REG_POS1(regs);
        uint8_t h   = HND(regs);
        ENSURE_BOUND(h);

        int64_t idx; IDX_FROM_IMM(idx);
        CHECK_BOUNDS(h, idx);

        double d = (src < 8) ? (double)vm->ireg[src] : vm->dreg[src - 8];
        STORE_ELEM(h, idx, NUMBER_VAL(d));
        NEXT;
    }

    // UNSAFE variants
    OP(R_LLOADU_R)
    {
        uint16_t regs = READ_SHORT();
        uint8_t dst = REG_POS1(regs);
        uint8_t ir  = REG_POS2(regs);
        uint8_t h   = HND(regs);
        ENSURE_BOUND(h);

        int64_t idx; IDX_FROM_IREG(ir, idx);
        double d = AS_NUMBER(LOAD_ELEM(h, idx));  // UB if OOB or non-number
        if (dst < 8) vm->ireg[dst] = (int64_t)d; else vm->dreg[dst - 8] = d;
        NEXT;
    }

    OP(R_LLOADU_I)
    {
        uint16_t regs = READ_SHORT();
        uint8_t dst = REG_POS1(regs);
        uint8_t h   = HND(regs);
        ENSURE_BOUND(h);

        int64_t idx; IDX_FROM_IMM(idx);
        double d = AS_NUMBER(LOAD_ELEM(h, idx));
        if (dst < 8) vm->ireg[dst] = (int64_t)d; else vm->dreg[dst - 8] = d;
        NEXT;
    }

    OP(R_LSTOREU_R)
    {
        uint16_t regs = READ_SHORT();
        uint8_t src = REG_POS1(regs);
        uint8_t ir  = REG_POS2(regs);
        uint8_t h   = HND(regs);
        ENSURE_BOUND(h);

        int64_t idx; IDX_FROM_IREG(ir, idx);
        double d = (src < 8) ? (double)vm->ireg[src] : vm->dreg[src - 8];
        STORE_ELEM(h, idx, NUMBER_VAL(d));
        NEXT;
    }

    OP(R_LSTOREU_I)
    {
        uint16_t regs = READ_SHORT();
        uint8_t src = REG_POS1(regs);
        uint8_t h   = HND(regs);
        ENSURE_BOUND(h);

        int64_t idx; IDX_FROM_IMM(idx);
        double d = (src < 8) ? (double)vm->ireg[src] : vm->dreg[src - 8];
        STORE_ELEM(h, idx, NUMBER_VAL(d));
        NEXT;
    }

    // Post-increment variants
    OP(R_LLOAD_PI)
    {
        uint16_t regs = READ_SHORT();
        uint8_t dst = REG_POS1(regs);
        uint8_t ir  = REG_POS2(regs);
        uint8_t h   = HND(regs);
        ENSURE_BOUND(h);

        int64_t idx = vm->ireg[ir];
        CHECK_BOUNDS(h, idx);
        Value v = LOAD_ELEM(h, idx);
        if (!IS_NUMBER(v)) { runtimeError(vm, "List element is not a number."); return INTERPRET_RUNTIME_ERROR; }
        double d = AS_NUMBER(v);
        if (dst < 8) vm->ireg[dst] = (int64_t)d; else vm->dreg[dst - 8] = d;
        vm->ireg[ir] = idx + 1;
        NEXT;
    }

    OP(R_LLOADU_PI)
    {
        uint16_t regs = READ_SHORT();
        uint8_t dst = REG_POS1(regs);
        uint8_t ir  = REG_POS2(regs);
        uint8_t h   = HND(regs);
        ENSURE_BOUND(h);

        int64_t idx = vm->ireg[ir];
        double d = AS_NUMBER(LOAD_ELEM(h, idx)); // UB if OOB/non-number
        if (dst < 8) vm->ireg[dst] = (int64_t)d; else vm->dreg[dst - 8] = d;
        vm->ireg[ir] = idx + 1;
        NEXT;
    }

    OP(R_LSTORE_PI)
    {
        uint16_t regs = READ_SHORT();
        uint8_t src = REG_POS1(regs);
        uint8_t ir  = REG_POS2(regs);
        uint8_t h   = HND(regs);
        ENSURE_BOUND(h);

        int64_t idx = vm->ireg[ir];
        CHECK_BOUNDS(h, idx);
        double d = (src < 8) ? (double)vm->ireg[src] : vm->dreg[src - 8];
        STORE_ELEM(h, idx, NUMBER_VAL(d));
        vm->ireg[ir] = idx + 1;
        NEXT;
    }

    OP(R_LSTOREU_PI)
    {
        uint16_t regs = READ_SHORT();
        uint8_t src = REG_POS1(regs);
        uint8_t ir  = REG_POS2(regs);
        uint8_t h   = HND(regs);
        ENSURE_BOUND(h);

        int64_t idx = vm->ireg[ir];
        double d = (src < 8) ? (double)vm->ireg[src] : vm->dreg[src - 8];
        STORE_ELEM(h, idx, NUMBER_VAL(d)); // UB if OOB
        vm->ireg[ir] = idx + 1;
        NEXT;
    }

#undef HND
#undef ENSURE_HANDLE
#undef ENSURE_BOUND
#undef H_LIST
#undef H_BASE
#undef H_LEN
#undef IDX_FROM_IREG
#undef IDX_FROM_IMM
#undef CHECK_BOUNDS
#undef LOAD_ELEM
#undef STORE_ELEM

    OP(R_PUSH)
    {
        uint16_t regs = READ_SHORT();
        uint8_t reg = REG_POS1(regs); // top nibble
        if (reg < 8)
            push(vm, NUMBER_VAL((double)vm->ireg[reg]));
        else
            push(vm, NUMBER_VAL(vm->dreg[reg - 8]));
        NEXT;
    }
    OP(R_POP)
    {
        uint16_t regs = READ_SHORT();
        uint8_t r = REG_POS1(regs);
        double x = AS_NUMBER(pop(vm));
        if (r < 8)
            vm->ireg[r] = (int64_t)x;
        else
            vm->dreg[r - 8] = x;
        NEXT;
    }
    OP(R_MOV)
    {
        uint16_t regs = READ_SHORT();
        uint8_t pos1 = REG_POS1(regs);
        uint8_t pos2 = REG_POS2(regs);

        if (pos1 < 8 && pos2 < 8) {
            vm->ireg[pos1] = vm->ireg[pos2];
        } else if (pos1 >= 8 && pos2 >= 8) {
            vm->dreg[pos1 - 8] = vm->dreg[pos2 - 8];
        } else if (pos1 < 8 && pos2 >= 8) {
            vm->ireg[pos1] = (int64_t)vm->dreg[pos2 - 8];
        } else {
            vm->dreg[pos1 - 8] = (double)vm->ireg[pos2];
        }
        NEXT;
    }

    OP(R_JZ)
    {
        uint16_t regs = READ_SHORT();
        uint8_t r = REG_POS1(regs);
        uint16_t off = READ_SHORT();

        bool isZero;
        if (r < 8) {
            isZero = (vm->ireg[r] == 0);
        } else {
            isZero = (vm->dreg[r - 8] == 0.0);
        }
        if (isZero) {
            frame->ip += off;
        }
        NEXT;
    }

    OP(R_JNZ)
    {
        uint16_t regs = READ_SHORT();
        uint8_t r = REG_POS1(regs);
        uint16_t off = READ_SHORT();

        bool notZero;
        if (r < 8) {
            notZero = (vm->ireg[r] != 0);
        } else {
            notZero = (vm->dreg[r - 8] != 0.0);
        }
        if (notZero) {
            frame->ip += off;
        }
        NEXT;
    }
    DEFAULT
    {
        printf("Unknown opcode %d\n", frame->ip[-1]);
        return INTERPRET_RUNTIME_ERROR;
    }

#undef OP
#undef DEFAULT
#undef NEXT

#undef READ_BYTE
#undef READ_OPCODE
#undef READ_CONSTANT
#undef READ_SHORT
#undef READ_STRING

#undef REG_POS1
#undef REG_POS2
#undef REG_POS3
#undef REG_POS4

#undef READ_I64
#undef READ_DBL

#undef BINARY_OP
#undef BINARY_OP_C
#undef BINARY_BIT_OP
#undef BINARY_BIT_OP_C
}

InterpretResult interpretCall(VM *vm, Obj *callable, int argCount)
{
    call(vm, callable, argCount);
    return run(vm);
}

InterpretResult interpret(VM *vm, const char *source)
{
    ObjFunction *function = compile(source, &vm->gc, &vm->strings);
    if (function == NULL) return INTERPRET_COMPILE_ERROR;

    push(vm, OBJ_VAL(function));
    call(vm, (Obj *) function, 0);

    return run(vm);
}
