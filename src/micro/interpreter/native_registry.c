#include "./native_registry.h"

// Forward-declare the native C functions that we want to register.
extern bool native_sqrt(VM* vm, int argCount, Value* args);
extern bool native_sin(VM* vm, int argCount, Value* args);
extern bool native_cos(VM* vm, int argCount, Value* args);
extern bool native_abs(VM* vm, int argCount, Value* args);

// The central registry.
FastNative G_FAST_NATIVES[] = {
    {"sqrt", native_sqrt, 1},
    {"sin",  native_sin,  1},
    {"cos",  native_cos,  1},
    {"abs",  native_abs,  1},
    {NULL, NULL, 0} // Marks the end of the list
};

int getFastNativeCount() {
    int count = 0;
    while (G_FAST_NATIVES[count].name != NULL) {
        count++;
    }
    return count;
}