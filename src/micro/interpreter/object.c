#include "./object.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "./gc.h"
#include "./memory.h"
#include "./obj_native.h"
#include "./value.h"

#define ALLOCATE_OBJ(gc, type, objectType) (type*)allocateObject(gc, sizeof(type), objectType)

static Obj *allocateObject(GC *gc, size_t size, ObjType type)
{
    Obj *object = (Obj *) reallocate(gc, NULL, 0, size);
    object->type = type;
    object->isMarked = false;

    object->next = gc->objects;
    gc->objects = object;

    if (debugLogGC) printf("%p allocate %zu for %d\n", (void *) object, size, type);

    return object;
}

ObjBoundMethod *newBoundMethod(GC *gc, Value receiver, Obj *method)
{
    ObjBoundMethod *bound = ALLOCATE_OBJ(gc, ObjBoundMethod, OBJ_BOUND_METHOD);
    bound->receiver = receiver;
    bound->method = method;
    return bound;
}

ObjClass *newClass(GC *gc, ObjString *name)
{
    ObjClass *klass = ALLOCATE_OBJ(gc, ObjClass, OBJ_CLASS);
    klass->name = name;
    initTable(&klass->methods, 0.75);
    return klass;
}

ObjClosure *newClosure(GC *gc, ObjFunction *function)
{
    ObjUpvalue **upvalues = ALLOCATE(gc, ObjUpvalue*, function->upvalueCount);
    for (int i = 0; i < function->upvalueCount; i++)
    {
        upvalues[i] = NULL;
    }

    ObjClosure *closure = ALLOCATE_OBJ(gc, ObjClosure, OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvalueCount = function->upvalueCount;
    return closure;
}

ObjFunction *newFunction(GC *gc)
{
    ObjFunction *function = ALLOCATE_OBJ(gc, ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->upvalueCount = 0;
    function->name = NULL;
    initChunk(&function->chunk);
    function->micro_i_slots = 0;
    function->micro_f_slots = 0;
    return function;
}

ObjInstance *newInstance(GC *gc, ObjClass *klass)
{
    ObjInstance *instance = ALLOCATE_OBJ(gc, ObjInstance, OBJ_INSTANCE);
    instance->klass = klass;
    initTable(&instance->fields, 0.75);
    return instance;
}

ObjList *newList(GC *gc)
{
    ObjList *list = ALLOCATE_OBJ(gc, ObjList, OBJ_LIST);
    initValueArray(&list->elements);
    return list;
}

ObjMap *newMap(GC *gc)
{
    ObjMap *map = ALLOCATE_OBJ(gc, ObjMap, OBJ_MAP);
    initTable(&map->table, 0.75);
    return map;
}

ObjNative *newNative(GC *gc, NativeFn function)
{
    ObjNative *native = ALLOCATE_OBJ(gc, ObjNative, OBJ_NATIVE);
    native->function = function;
    return native;
}

static ObjString *allocateString(GC *gc, int length, uint32_t hash)
{
    ObjString *string = (ObjString *) allocateObject(gc, sizeof(ObjString) + length + 1, OBJ_STRING);
    string->length = length;
    string->hash = hash;
    string->chars[0] = '\0';
    return string;
}

#define INIT_HASH 2166136261u

static uint32_t hashAnotherString(
    uint32_t hash, const char *key, int length)
{
    for (int i = 0; i < length; ++i)
    {
        hash ^= (uint8_t) key[i];
        hash *= 16777619;
    }
    return hash;
}

ObjString *concatStrings(GC *gc, Table *strings, const char *a, int aLen, uint32_t aHash, const char *b, int bLen)
{
    assert(aLen + bLen >= 0);

    uint32_t hash = hashAnotherString(aHash, b, bLen);
    Entry *entry = tableJoinedStringsEntry(gc, strings, a, aLen, b, bLen, hash);
    if (entry->key != NULL)
    {
        // Concatenated string already interned.
        return entry->key;
    }

    int length = aLen + bLen;
    ObjString *string = allocateString(gc, length, hash);
    memcpy(string->chars, a, aLen);
    memcpy(string->chars + aLen, b, bLen);
    string->chars[length] = '\0';

    tableSetEntry(strings, entry, string, NULL_VAL);
    return string;
}

ObjString *copyString(GC *gc, Table *strings, const char *chars, int length)
{
    uint32_t hash = hashAnotherString(INIT_HASH, chars, length);
    return concatStrings(gc, strings, chars, length, hash, "", 0);
}

ObjUpvalue *newUpvalue(GC *gc, Value *slot)
{
    ObjUpvalue *upvalue = ALLOCATE_OBJ(gc, ObjUpvalue, OBJ_UPVALUE);
    upvalue->closed = NULL_VAL;
    upvalue->location = slot;
    upvalue->next = NULL;
    return upvalue;
}

static void printFunction(ObjFunction *function)
{
    if (function->name == NULL)
    {
        printf("<script>");
        return;
    }
    printf("<fn %s>", function->name->chars);
}

void printObject(Value value)
{
    switch (OBJ_TYPE(value))
    {
        case OBJ_BOUND_METHOD:
        {
            printObject(OBJ_VAL(AS_BOUND_METHOD(value)->method));
            break;
        }
        case OBJ_CLASS:
            printf("%s", AS_CLASS(value)->name->chars);
            break;
        case OBJ_CLOSURE:
            printFunction(AS_CLOSURE(value)->function);
            break;
        case OBJ_FUNCTION:
            printFunction(AS_FUNCTION(value));
            break;
        case OBJ_INSTANCE:
            printf("%s instance", AS_INSTANCE(value)->klass->name->chars);
            break;
        case OBJ_LIST:
        {
            ObjList *list = AS_LIST(value);
            puts("[");
            if (list->elements.count > 0)
            {
                printValueShallow(list->elements.values[0]);
            }
            for (int i = 1; i < list->elements.count; ++i)
            {
                puts(", ");
                printValueShallow(list->elements.values[i]);
            }
            puts("]");
            break;
        }
        case OBJ_MAP:
        {
            ObjMap *map = AS_MAP(value);
            puts("{");
            bool first = true;
            for (int i = 0; i < map->table.capacity; ++i)
            {
                Entry *entry = &map->table.entries[i];
                if (entry->key == NULL)
                {
                    continue;
                }
                if (first)
                {
                    first = false;
                } else
                {
                    puts(", ");
                }
                printf("%.*s", entry->key->length, entry->key->chars);
                puts(": ");
                printValueShallow(entry->value);
            }
            puts("}");
            break;
        }
        case OBJ_NATIVE:
            printf("<native fn>");
            break;
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
        case OBJ_UPVALUE:
            printf("upvalue");
            break;
    }
}
