#pragma once

#include "./vm.h"
#include "obj_native.h"

typedef struct {
    const char* name;
    NativeFn function;
    int arity;
} FastNative;

extern FastNative G_FAST_NATIVES[];

int getFastNativeCount();
