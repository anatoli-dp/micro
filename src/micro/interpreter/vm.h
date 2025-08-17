#pragma once

#include <stdio.h>

#include "./gc.h"
#include "./object.h"
#include "./table.h"
#include "./value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

#ifndef MS_MAX_LBIND
#define MS_MAX_LBIND 16
#endif

typedef struct {
    ObjList* list;   // bound list object
    Value*   elems;  // cached pointer to backing array (list->elements.values)
    uint32_t len;    // cached length (list->elements.count)
} MsListCache;

typedef struct
{
    ObjClosure *closure;
    ObjFunction *function;
    uint16_t *ip;

    // Value slots base for this frame (bytecode locals)
    Value *slots;

    // ---- micro-scratch allocator state ----
    uint32_t  ms_sp_mark;   // watermark for the VM micro scratch pool
    int64_t  *msi;          // int64 scratch base
    double   *msd;          // double scratch base
    uint16_t  msi_cap;      // # of int64 scratch slots
    uint16_t  msd_cap;      // # of double scratch slots

    // ---- up to 16 concurrently bound lists (handles h0..h15) ----
    MsListCache mh[MS_MAX_LBIND];  // mh[h] = {list, elems, len}
} CallFrame;

typedef struct
{
    ValueArray args;

    CallFrame frames[FRAMES_MAX];
    int frameCount;

    Value stack[STACK_MAX];
    Value *stackTop;
    Table globals;
    ValueArray globalSlots;
    Table strings;
    ObjString *initString;
    ObjUpvalue *openUpvalues;

    ObjClass *listClass;
    ObjClass *mapClass;
    ObjClass *stringClass;

    GC gc;

    // Microasm Register File
    double dreg[8]; // 8 double-precision floating-point registers: d0-d7
    int64_t ireg[8]; // 8 64-bit integer registers: i0-i7
    // Micro scratch bump-allocated pool
    uint8_t  *ms_pool;
    uint32_t  ms_sp;     // bump pointer (bytes)
    uint32_t  ms_size;   // pool size (bytes)
} VM;

typedef enum
{
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

void initVM(VM *vm);
void freeVM(VM *vm);
void argsVM(VM *vm, int argc, const char *argv[]);
void push(VM *vm, Value value);
Value pop(VM *vm);

InterpretResult interpretCall(VM *vm, Obj *callable, int argCount);
InterpretResult interpret(VM *vm, const char *source);

extern bool debugTraceExecution;

void runtimeError(VM *vm, const char *format, ...);
