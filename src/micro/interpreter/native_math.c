#include <math.h>

#include "./native_registry.h"
#include "./object.h"
#include "./value.h"
#include "./vm.h"

// Helper to get a number argument and report an error if it's not a number
bool getNumberArg(VM* vm, int argCount, Value* args, int index, const char* name, double* num) {
    if (index >= argCount || !IS_NUMBER(args[index])) {
        runtimeError(vm, "Argument %d to %s() must be a number.", index + 1, name);
        return false;
    }
    *num = AS_NUMBER(args[index]);
    return true;
}

bool native_sqrt(VM* vm, int argCount, Value* args) {
    double num;
    if (!getNumberArg(vm, argCount, args, 0, "sqrt", &num)) return false;
    push(vm, NUMBER_VAL(sqrt(num)));
    return true;
}

bool native_sin(VM* vm, int argCount, Value* args) {
    double num;
    if (!getNumberArg(vm, argCount, args, 0, "sin", &num)) return false;
    push(vm, NUMBER_VAL(sin(num)));
    return true;
}

bool native_cos(VM* vm, int argCount, Value* args) {
    double num;
    if (!getNumberArg(vm, argCount, args, 0, "cos", &num)) return false;
    push(vm, NUMBER_VAL(cos(num)));
    return true;
}

bool native_abs(VM* vm, int argCount, Value* args) {
    double num;
    if (!getNumberArg(vm, argCount, args, 0, "abs", &num)) return false;
    push(vm, NUMBER_VAL(fabs(num)));
    return true;
}