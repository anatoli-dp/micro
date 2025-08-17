#pragma once

#include <stdio.h>

#include "../chunk.h"
#include "../object.h"
#include "../table.h"

ObjFunction* compile(const char* source, GC* gc, Table* strings);

extern bool debugPrintCode;
