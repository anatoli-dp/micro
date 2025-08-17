#set(CMAKE_C_FLAGS_RELEASE "-O3 -march=native -DNDEBUG")
#set(CMAKE_BUILD_TYPE Release)

cmake_minimum_required(VERSION 3.28)
project(micro C)

set(CMAKE_C_STANDARD 11)

add_executable(micro
        src/main.c

        src/micro/interpreter/native_registry.c
        src/micro/interpreter/native_registry.h
        src/micro/interpreter/native_math.c

        src/micro/natives/natives.h

        src/micro/interpreter/chunk.c
        src/micro/interpreter/chunk.h
        src/micro/interpreter/common.h
        src/micro/interpreter/compiler/compiler.c
        src/micro/interpreter/compiler/compiler.h
        src/micro/interpreter/debug.c
        src/micro/interpreter/debug.h
        src/micro/interpreter/gc.c
        src/micro/interpreter/gc.h
        src/micro/interpreter/list.h
        src/micro/interpreter/memory.c
        src/micro/interpreter/memory.h
        src/micro/interpreter/obj_native.h
        src/micro/interpreter/object.c
        src/micro/interpreter/object.h
        src/micro/interpreter/scanner.c
        src/micro/interpreter/scanner.h
        src/micro/interpreter/table.c
        src/micro/interpreter/table.h
        src/micro/interpreter/value.c
        src/micro/interpreter/value.h
        src/micro/interpreter/vm.c
        src/micro/interpreter/vm.h
)

target_link_libraries(micro m)