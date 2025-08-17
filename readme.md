# micro

**micro** is a lightweight, embeddable scripting language inspired by the [clox interpreter](https://craftinginterpreters.com/) (part of Bob Nystrom's *Crafting Interpreters*). Designed specifically for resource-constrained environments like microcontrollers and embedded devices, **micro** provides simplicity, efficiency, and customizability.

> **Note:** This project is no longer actively maintained. It is publicly shared here for educational purposes, reference, or as a starting point for your own projects. You're encouraged to fork, experiment, and adapt freely!

---

## About the Project

Originally created as an embedded scripting language for a custom game console, **micro** can serve general-purpose embedded scripting needs. Its compact footprint and embedded-focused optimizations make it suitable for small hardware environments.

---

## Key Features

- **Micro-scratch Memory Management:**

  - Custom bump-allocator designed specifically for embedded systems to efficiently manage temporary allocations.

- **VM Enhancements:**

  - Internally, micro includes a register file for an experimental "microasm" (assembly-like) mode, but regular scripts and code compiled by the standard compiler do not access these registers directly.

- **Built-in Optimized Data Types:**

  - Native support for `list` and `map` types with optimized internal caching for faster runtime access and lower overhead.

\$1

- **Optimized VM Dispatch (Diverges from clox):**
  - micro replaces the original switch/case instruction dispatch with a computed goto jump table (where supported by the C compiler). This results in a significantly faster VM and is a major divergence from the standard lox/clox implementation.

---

## Language Semantics & Usage Overview

### Basic Syntax

- Curly braces `{}` define code blocks.
- Variables are dynamically typed, declared with `var`.
- **Semicolons are optional:** Most statements do not require a trailing semicolon. Code can be written in a C-like or JavaScript-like style, with or without semicolons, including single-line or multiline statements and expressions.
- **Flexible formatting:** Function arguments, loop headers/conditions, and expressions can be split across multiple lines or grouped together as desired. This applies to all code constructs, making the syntax both forgiving and highly readable for your preferred style.

### Control Flow

- Supported loops:
  - `for` loops: `for (var i = 0; i < N; ++i)`
  - `while` loops: `while (condition)`
  - `do-while` loops: `do { ... } while (condition)`
- `break` and `continue` can be used in any loop to exit early or skip to the next iteration.
- **Labeled Loops:**
  - Any loop or block can be labeled (e.g., `outer: while (...) { ... }`).
  - `break label;` and `continue label;` allow direct control over which loop to break/continue, even skipping multiple nesting levels (not just the innermost loop).
  - Unlabeled `break`/`continue` always apply to the innermost loop.
- Labeled and unlabeled break/continue can be mixed freely within deeply nested or mixed loop types (`for`, `while`, `do-while`).
- Edge cases (such as breaking or continuing at the very start of a loop, or combining multiple labeled loops) are handled in an intuitive, C-like way.

#### Example Pattern:

```javascript
outer: for (var i = 0; i < 3; ++i) {
    for (var j = 0; j < 3; ++j) {
        if (j == 1) continue outer;
        if (i == 2) break outer;
    }
}
```

### Functions & Recursion

- Functions declared with `func name(params) { ... }`.
- Functions support recursion and can return values.

### Built-in Functions & Methods

- `println(...)`: Print to standard output.
- `clock()`: Return current system or VM clock (for timing).
- String `.size()` method for length.

\$1

### Ternary Operator

- **Ternary operator (**``**):**
  - C-style ternary expressions are supported: `condition ? expr1 : expr2`.
  - This allows inline conditional logic anywhere an expression is valid.

#### Example:

```javascript
var a = 10
var b = 20
var max = (a > b) ? a : b
println("Max value is:", max)
```

- Standard comparison operators (`==`, `<`, `<=`, etc.).

### Numeric Literals & Bitwise Operations

- **Binary literals:** Use `0b` prefix (e.g., `0b1010`), underscores allowed (`0b1111_0000`).
- **Hexadecimal literals:** Use `0x` prefix (e.g., `0xDEAD_BEEF`), underscores allowed.
- **Decimal literals:** Underscores can be used for digit grouping (e.g., `1_000_000`).
- **Bitwise operators:** `&` (and), `|` (or), `^` (xor), `~` (not), `<<` (left shift), `>>` (right shift)—fully supported and can be combined in expressions.
- **Large integer support:** At least up to 53 bits (e.g., `9007199254740991`).
- **Mixing:** Binary, hexadecimal, decimal, and bitwise operations can be freely mixed in complex expressions.

#### Example Patterns:

```javascript
var a = 0b1010_1010
var b = 0xDEAD_BEEF
var c = 1_000_000

var d = a & 0xFF
var e = b | 0b1111_0000
var f = ~a
var g = (a ^ b) >> 2
```

### String Manipulation

- Direct concatenation with `+=`.
- String objects support the `.size()` method.

### Maps (Hashmaps)

- Maps are built-in, high-performance hashmaps with flexible key/value support.
- You can create a map with curly braces:
  ```javascript
  var m = {"one": 1, 2: [someList], [1+2]: 3}
  ```
- **Key coercion:** All keys are converted to strings, so numbers and strings (or expressions that evaluate to them) are valid as keys.
- **Access patterns:**
    - Dot notation (`m.key`) for string keys (if the key is not a number)
    - Index notation (`m["key"]`, `m[key]`, `m[5]`) for any key (number, variable, or expression)

#### Example:
```javascript
var m = {"foo": 10, 3: [1,2,3], bar: 99}
println(m.foo)      // 10
println(m[3])       // [1,2,3]
println(m["bar"])   // 99

var key = "foo"
println(m[key])     // 10

m.baz = 42
println(m["baz"])   // 42
```


### Classes, Inheritance, and Closures

- **Class Declarations:** Use `class Name { ... }`. Methods are defined inside; constructors use `init()`.
- **Fields and Methods:** Fields assigned with `this.field = value`. Methods called as `obj.method()`.
- **Semicolons are optional** in classes and methods, including for field and method definitions.
- **Inheritance:**
  - `class Child < Parent { ... }` for subclassing.
  - Use `super.method()` and `super.init()` to access parent class functionality.
  - Fields and methods can be inherited and overridden.
- **Closures:**
  - Functions may be nested, returned, or stored in variables/fields, and capture variables from their defining environment (lexical closures).
  - Closures can reference both local variables and object fields (`this`).
  - Methods can return closures that reference the object or its fields.
- **Flexible, modern method syntax:** Supports nested blocks, multiline method bodies, and blank lines inside methods and closures.

#### Example Patterns:

```javascript
class Foo {
    init() { this.x = 10 }
    getX() { return this.x }
}
var f = Foo()
println(f.getX()) // 10

func makeAdder(n) { return func(x) { return x + n } }
var add5 = makeAdder(5)
println(add5(7)) // 12

class Child < Parent {
    speak() { return super.speak() + " child" }
}
```

### Labels, `jmp`, and `goto` Statements

- **Labels:** Any statement can be labeled (e.g., `mylabel: statement;`). Multiple labels can be stacked on a single statement.
- ``** (Raw Jump):**
  - Performs an unstructured, unconditional jump straight to a label in the same function or block.
  - **Raw**: Does not clean up or run scope/loop finalizers—can jump into or out of any code region, including skipping variable destruction and loop exits.
  - **Unsafe:** Can lead to undefined behavior if used to jump into the middle of scopes or loops; intended only for advanced, low-level control (e.g., state machines).
  - Cannot cross function boundaries.
- ``** (Safe Jump):**
  - Provides a structured jump that **cleans up scopes** before jumping.
  - Only allows jumping to:
    - A label in a higher (enclosing) scope (can jump out of nested blocks, but not into them).
    - Another label in the same scope.
  - Will run all necessary cleanup code for exited scopes ("trampoline" semantics).
  - Designed for safer, more C-like control flow—cannot be used to jump into a block, only out of or across at the same level.
- **Typical usage:**
  - `goto` for safe early exits or breaking out of nested blocks/loops (with cleanup).
  - `jmp` for raw, direct jumps where cleanup is either unnecessary or explicitly managed.

#### Example:

```javascript
// Using jmp (raw, unsafe)
label1:
var x = 1
jmp label2
x = 99 // skipped
label2:
x = x + 1

// Using goto (safe)
outer: {
  inner: {
    if (something) goto outer; // safe jump OUT of inner to outer
    // goto inner; // not allowed: cannot jump IN
  }
}
```

---



## Usage Examples

You can find various example scripts demonstrating language features in the `/tests` directory of this repository. Each script covers different aspects of micro's syntax, control flow, functions, recursion, built-in types, and more.

---

## Getting Started

Clone the repository:

```bash
git clone https://github.com/your-username/micro.git
cd micro
```

Compile the interpreter (example shown, adjust source files as needed):

```bash
gcc -o micro main.c [additional_source_files.c]
```

Run a test script provided in the `/tests` directory:

```bash
./micro tests/example.micro
```

See `main.c` for embedding examples and integration into other projects.

---

## Why Archived?

**micro** reached a stable state suitable for its original purpose but had architectural limitations. As a result, I've started a fresh rebuild to explore new architectural approaches. This repository remains available for reference and for anyone interested in continuing or adapting the project.

---

## License

Currently, **micro** has no formal license attached. An MIT license may be added later. For now, use at your own discretion and risk.

---

## Contributions

While the project is no longer actively maintained, contributions, improvements, and fixes are still welcome. I will occasionally review and merge beneficial pull requests.

---

## Acknowledgements

- Inspired by [clox](https://github.com/munificent/craftinginterpreters) from Bob Nystrom's *Crafting Interpreters*.
- Special thanks to anyone using, studying, or extending the **micro** scripting language.

---

## Advanced Uses: MicroASM Blocks

**micro** supports embedding raw, register-based *VM code* for maximum performance via the `microasm` block:

- **microasm blocks:**
  - Allow direct access to the VM’s internal registers (integer: `i0`–`i7`, float: `f0`–`f7`) using low-level *VM* instructions (not hardware CPU assembly).
  - **Not machine assembly:** These registers are unique to the micro VM, not your CPU. But they are much faster than the usual stack-based scripting due to lower VM overhead. \$1- **Bulk list/array binding and streaming:**
  - microasm lets you bind lists (arrays) directly to registers and process them in-place with special instructions like `lbind`, `lbindl`, `lload`, `lloadu`, `lloadp`, `lloadup`, `lstore`, `lstoreu`, `lstorep`, and `lstoreup`.
    - `lload` / `lstore`: Access/store at a specific index, **with bounds checking** (safe).
    - `lloadu` / `lstoreu`: Access/store with **no bounds checking** (faster, unsafe).
    - `lloadp` / `lstorep`: Like `lload`/`lstore`, but **auto-increments index** (with bounds check; safe for streaming).
    - `lloadup` / `lstoreup`: Like `lloadu`/`lstoreu`, but **auto-increments index** (no bounds check; fastest streaming).
  - Enables highly efficient “streaming” calculations across large buffers—ideal for game physics, simulation, DSP, or any large-scale data processing.
  - Allows you to write tight, high-performance “vectorized” loops in script.
  - This is the main intended use for microasm: hand-optimizing array and buffer operations that would be slow in regular script code.
  - **Example:**
    ```javascript
    func verletUpdate(posX, posY, velX, velY)
    microasm {
        lbindl i7 posX h0    // bind posX, i7 = len
        lbind  posY    h1    // bind posY
        lbind  velX    h2    // bind velX
        lbind  velY    h3    // bind velY

        // ...stream through and operate on lists in-place with lloadp/lstorep or lloadup/lstoreup...
    }
    ```

\$1 \$1

- **Inline and functional microasm blocks:**

  - microasm blocks can be used *inline* within scripts—not just as standalone functions.
  - Inline microasm can directly access local variables, upvalues, and globals, allowing fast register code to interleave with regular script.
  - You can jump into and out of microasm blocks (and across separate microasm blocks) using label/jump mechanics.
  - This enables advanced, hand-crafted control flows and optimizations, like building a loop or state machine using both script and microasm.
  - **Example:**
    ```javascript
    var total = 0
    var i = 0

    microasm { rload i0 i }    // Load script variable 'i' into register i0

    loop:
      microasm { rstore i0 i } // Write register i0 back to script variable 'i'
      total = total + i        // Script code uses the updated 'i'

      if (i < 10) {
        microasm { radd i0 i0 1 }  // Increment i0 in register
        goto loop                  // Loop again (can jump in/out of microasm)
      }
    ```

- **Interoperability:** microasm functions are called just like any script function and can return values.

- **For general scripting use:**

  - *microasm* is specifically meant to let you hand-optimize performance-critical “hot spots” anywhere in your script, not just for advanced users.
  - It’s the “no bars held” way to get speed when you really need it.

- **No Safety Checks:**

  - If it passes compile-time checks, it runs *as is*—with no extra runtime safety, scope, or memory cleanup guarantees.
  - Bad or buggy microasm (such as register misuse, invalid jumps, or breaking VM state) can cause segmentation faults or crashes, especially on embedded platforms.

- **Intended for:**

  - Squeezing out maximum performance, writing fast math/DSP loops, porting C/ASM-style logic, or experimenting with the VM’s limits.

> **Note:**\
> There are more advanced behaviors and nuances to microasm than can be listed here. For the deepest understanding, check out the `microasmBlock` function in `compiler.c`—the source is always the most definitive reference for edge cases, implementation details, and advanced usage.

---

> **FAQ:** Not every feature, VM trick, or corner-case is in this README.
> The best way to truly learn how `micro` works is to read the code—especially `compiler.c`, `vm.c`, and the tests directory.
> This project is meant for explorers, tinkerers, and learners!

---

Enjoy exploring **micro**, and happy hacking!



