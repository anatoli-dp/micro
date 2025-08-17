#include "./compiler.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../common.h"
#include "../debug.h"
#include "../gc.h"
#include "../memory.h"
#include "../scanner.h"
#include "../native_registry.h"

#ifndef MICRO_SCRATCH_MAX_I
#define MICRO_SCRATCH_MAX_I 256  // max integer scratch slots per function
#endif
#ifndef MICRO_SCRATCH_MAX_F
#define MICRO_SCRATCH_MAX_F 256  // max float   scratch slots per function
#endif

#define MS_MAX_LBIND 16

bool debugPrintCode = false;

// ---- micro-scratch debug summary ----
static void printMicroScratchSummary(ObjFunction *fn) {
    const char *name = (fn->name != NULL) ? fn->name->chars : "<script>";
    printf(";; micro-scratch: %-16s i=%u f=%u\n", name, (unsigned)fn->micro_i_slots, (unsigned)fn->micro_f_slots);
}

typedef struct
{
    Token name;
    int depth;
    bool isCaptured;
} Local;

typedef struct
{
    uint8_t index;
    bool isLocal;
} Upvalue;

typedef enum
{
    TYPE_FUNCTION,
    TYPE_INITIALIZER,
    TYPE_METHOD,
    TYPE_SCRIPT,
} FunctionType;

typedef struct BreakJump {
    int offset;
    struct BreakJump *next;
} BreakJump;

typedef struct ContinueJump {
    int offset;
    struct ContinueJump *next;
} ContinueJump;

typedef struct Label {
    Token name;
    int offset;
    int scopeDepth;
    struct Label *next;
} Label;

typedef struct JmpJump {
    Token name;
    int offset;
    int scopeDepth;
    struct JmpJump *next;
} JmpJump;

typedef struct SafeJump {
    Token name;           // target label (function-level label only)
    int   placeholder;    // index of the 16-bit word to patch (from emitJump)
    int   srcLocalLimit;  // number of locals declared up to the jump site
    int   srcScopeDepth;  // scope depth at the jump site
    struct SafeJump *next;
} SafeJump;

typedef struct Trampoline {
    int srcLocalLimit;      // localCount at the safe-jump site
    int srcScopeDepth;      // scope depth at the safe-jump site
    int targetScopeDepth;   // scope depth where the label is declared
    int targetOffset;       // bytecode index of target label
    int entryOffset;        // bytecode index where trampoline begins
    struct Trampoline *next;
} Trampoline;

typedef struct Loop
{
    int start;
    int exitJump;
    int continueTarget;
    BreakJump *breakJumps;
    ContinueJump *continueJumps;
    struct Loop *enclosing;
    Token *label; // Pointer to the label token if labeled, else NULL
    int scopeDepth; // Scope depth at which the loop begins
} Loop;

struct Parser;

typedef struct Compiler
{
    struct Compiler *enclosing;
    ObjFunction *function;
    FunctionType type;
    Local locals[UINT8_COUNT];
    int localCount;
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth;
    Loop *currentLoop;
    Label *labels;
    JmpJump *gotos;
    SafeJump *safeJumps;
    Trampoline *trampCache;
    struct Parser* parser;
} Compiler;

typedef struct ClassCompiler
{
    struct ClassCompiler *enclosing;
    bool hasSuperclass;
} ClassCompiler;

typedef struct Parser
{
    GC *gc;
    void (*prevMarkRoots)(GC *, void *);
    void *prevMarkRootsArg;
    void (*prevFixWeak)(void *);
    void *prevFixWeakArg;
    Table *strings;
    Scanner scanner;
    Compiler *currentCompiler;
    ClassCompiler *currentClass;
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
    Token *currentLabel; // Current label for labeled loops
    Table fastNatives;
} Parser;

// clang-format off
typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT, // =
  PREC_OR,         // or
  PREC_AND,        // and
  PREC_BIT_OR,     // |
  PREC_BIT_XOR,    // ^
  PREC_BIT_AND,    // &
  PREC_EQUALITY,   // == !=
  PREC_COMPARISON, // < > <= >=
  PREC_BIT_SHIFT,  // << >>
  PREC_TERM,       // + -
  PREC_FACTOR,     // * / %
  PREC_UNARY,      // ! -
  PREC_CALL,       // . ()
  PREC_PRIMARY,
} Precedence;
// clang-format on

typedef void (*ParseFn)(Parser *parser, bool canAssign);

typedef struct
{
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

static Chunk *currentChunk(Parser *parser)
{
    return &parser->currentCompiler->function->chunk;
}

static void errorAt(Parser *parser, Token *token, const char *message)
{
    if (parser->panicMode) return;
    parser->panicMode = true;
    printf("[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        printf(" at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing.
    } else {
        printf(" at '%.*s'", token->length, token->start);
    }

    printf(": %s\n", message);
    parser->hadError = true;
}

static void error(Parser *parser, const char *message)
{
    errorAt(parser, &parser->previous, message);
}

static void errorAtCurrent(Parser *parser, const char *message)
{
    errorAt(parser, &parser->current, message);
}

static void advance(Parser *parser)
{
    parser->previous = parser->current;

    for (;;)
    {
        parser->current = scanToken(&parser->scanner);
        if (parser->current.type != TOKEN_ERROR) break;
        errorAtCurrent(parser, parser->current.start);
    }
}

static void consume(Parser *parser, TokenType type, const char *message)
{
    if (parser->current.type == type)
    {
        advance(parser);
        return;
    }

    errorAtCurrent(parser, message);
}

static bool check(Parser *parser, TokenType type)
{
    return parser->current.type == type;
}

static bool match(Parser *parser, TokenType type)
{
    if (!check(parser, type)) return false;
    advance(parser);
    return true;
}

static bool isStatementStart(Parser *parser) {
    switch (parser->current.type) {
        case TOKEN_VAR:
        case TOKEN_FUNC:
        case TOKEN_CLASS:
        case TOKEN_FOR:
        case TOKEN_IF:
        case TOKEN_WHILE:
        case TOKEN_DO:
        case TOKEN_RETURN:
        case TOKEN_BREAK:
        case TOKEN_CONTINUE:
        case TOKEN_IDENTIFIER:
        case TOKEN_LEFT_BRACE:
        case TOKEN_PRINT:
        case TOKEN_MICROASM:
            return true;
        default:
            return false;
    }
}

static void consumeEndOfStatement(Parser *parser, const char *message)
{
    if (match(parser, TOKEN_SEMICOLON)) return;
    if (match(parser, TOKEN_NEWLINE)) return;
    if (check(parser, TOKEN_EOF)) return;
    if (check(parser, TOKEN_RIGHT_BRACE)) return;
    if (isStatementStart(parser)) return; // <-- THIS LINE MAKES IT SUPER ROBUST
    errorAtCurrent(parser, message);
}

static void skipNewlines(Parser *parser) {
    while (match(parser, TOKEN_NEWLINE));
}

static void emitByte(Parser *parser, uint16_t byte)
{
    writeChunk(parser->gc, currentChunk(parser), byte, parser->previous.line);
}

static void emitBytes(Parser *parser, uint16_t byte1, uint16_t byte2)
{
    emitByte(parser, byte1);
    emitByte(parser, byte2);
}

static void emitOpShort(Parser *parser, uint16_t op, uint16_t u16)
{
    emitByte(parser, op);
    emitByte(parser, u16);
}

static void emitLoop(Parser *parser, int loopStart)
{
    emitByte(parser, JMP_BACK);

    int offset = currentChunk(parser)->count - loopStart + 1;
    if (offset > UINT16_MAX) error(parser, "Loop body too large.");

    emitByte(parser, (uint16_t)offset);
}

static int emitJump(Parser *parser, uint16_t instruction)
{
    emitByte(parser, instruction);
    emitByte(parser, 0xffff); // Single 16-bit placeholder
    return currentChunk(parser)->count - 1;
}

static void emitReturn(Parser *parser)
{
    if (parser->currentCompiler->type == TYPE_INITIALIZER) {
        emitBytes(parser, LD_LOCAL, 0);
    } else {
        emitByte(parser, PUSH_NULL);
    }

    emitByte(parser, RET);
}

static uint16_t makeConstant(Parser *parser, Value value)
{
    int constant = findConstant(currentChunk(parser), value);
    if (constant == -1) constant = addConstant(parser->gc, currentChunk(parser), value);
    if (constant > UINT16_MAX)
    {
        error(parser, "Too many constants in one chunk.");
        return 0;
    }

    return (uint16_t) constant;
}

static void emitConstant(Parser *parser, Value value)
{
    emitOpShort(parser, PUSH_CONST, makeConstant(parser, value));
}

static void patchJump(Parser *parser, int offset)
{
    // -1 to adjust for the bytecode for the jump offset itself.
    int jump = currentChunk(parser)->count - offset - 1;

    if (jump > UINT16_MAX) error(parser, "Too much code to jump over.");

    currentChunk(parser)->code[offset] = (uint16_t)jump;
}

static void initCompiler(Parser *parser, Compiler *compiler, FunctionType type, const char *name, int nameLength)
{
    compiler->enclosing = parser->currentCompiler;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->currentLoop = NULL;
    compiler->labels = NULL;
    compiler->gotos = NULL;
    compiler->safeJumps = NULL;
    compiler->trampCache = NULL;
    compiler->function = newFunction(parser->gc);
    parser->currentCompiler = compiler;
    compiler->parser = parser;
    compiler->function->micro_i_slots = 0;
    compiler->function->micro_f_slots = 0;

    if (type != TYPE_SCRIPT) parser->currentCompiler->function->name = copyString(parser->gc, parser->strings, name, nameLength);

    Local *local = &parser->currentCompiler->locals[parser->currentCompiler->localCount++];
    local->depth = 0;
    local->isCaptured = false;

    if (type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

static void beginLoop(Parser *parser, Loop *loop)
{
    loop->enclosing = parser->currentCompiler->currentLoop;
    loop->start = currentChunk(parser)->count;
    loop->exitJump = -1;
    loop->continueTarget = -1;
    loop->breakJumps = NULL;
    loop->continueJumps = NULL;
    loop->label = NULL; // Initialize label to NULL
    loop->scopeDepth = parser->currentCompiler->scopeDepth; // Store current scope depth
    parser->currentCompiler->currentLoop = loop;
}

static void endLoop(Parser *parser)
{
    if (parser->currentCompiler->currentLoop != NULL) {
        // Patch all break jumps to jump to the current position
        BreakJump* breakJump = parser->currentCompiler->currentLoop->breakJumps;
        while (breakJump != NULL) {
            patchJump(parser, breakJump->offset);
            BreakJump* next = breakJump->next;
            free(breakJump);
            breakJump = next;
        }
        
        // Free any remaining continue jumps
        ContinueJump* continueJump = parser->currentCompiler->currentLoop->continueJumps;
        while (continueJump != NULL) {
            patchJump(parser, continueJump->offset);
            ContinueJump* next = continueJump->next;
            free(continueJump);
            continueJump = next;
        }
        
        parser->currentCompiler->currentLoop = parser->currentCompiler->currentLoop->enclosing;
    }
}

static void resolveJmpJumps(Parser *parser) {
    Compiler *compiler = parser->currentCompiler;

    JmpJump *jmpJump = compiler->gotos;
    while (jmpJump != NULL) {
        Label *label = compiler->labels;
        bool found = false;

        while (label != NULL) {
            if (label->name.length == jmpJump->name.length &&
                memcmp(label->name.start, jmpJump->name.start, label->name.length) == 0 &&
                label->scopeDepth <= jmpJump->scopeDepth) {
                currentChunk(parser)->code[jmpJump->offset] =
                    (uint16_t)(label->offset - jmpJump->offset - 1);
                found = true;
                break;
                }
            label = label->next;
        }

        if (!found) {
            parser->current = jmpJump->name;
            error(parser, "Undefined label.");
        }

        JmpJump *next = jmpJump->next;
        free(jmpJump);
        jmpJump = next;
    }

    compiler->gotos = NULL;
}

static int getOrMakeTrampoline(Parser *parser,
    int srcLocalLimit, int srcScopeDepth,
    int targetScopeDepth, int targetOffset)
{
    Compiler *c = parser->currentCompiler;

    // Lookup with full key (srcLimit + both depths + targetOffset)
    for (Trampoline *t = c->trampCache; t; t = t->next) {
        if (t->srcLocalLimit    == srcLocalLimit &&
            t->srcScopeDepth    == srcScopeDepth &&
            t->targetScopeDepth == targetScopeDepth &&
            t->targetOffset     == targetOffset) {
            return t->entryOffset;
        }
    }

    int trampStart = currentChunk(parser)->count;

    // IMPORTANT:
    // Use the *snapshot* of locals that were live at the jump site
    // (srcLocalLimit). Do NOT clamp to the current c->localCount,
    // because we may be emitting this trampoline after inner scopes
    // have been closed, which would hide the very locals we need to
    // clean up (e.g., loop indices).
    //
    // Only emit cleanup when jumping to a *shallower* scope.
    if (targetScopeDepth < srcScopeDepth && srcLocalLimit > 0) {
        for (int i = srcLocalLimit - 1; i >= 0; --i) {
            int d = c->locals[i].depth;
            if (d <= targetScopeDepth) break; // locals at/above target survive
            emitByte(parser, c->locals[i].isCaptured ? CLOSE_UPVAL : POP);
        }
    }

    // Final jump to the target label
    int here = currentChunk(parser)->count;
    if (targetOffset <= here) {
        int back = here - targetOffset + 2;
        emitByte(parser, JMP_BACK);
        emitByte(parser, (uint16_t)back);
    } else {
        int fwd = targetOffset - here - 2;
        emitByte(parser, JMP);
        emitByte(parser, (uint16_t)fwd);
    }

    // Cache the trampoline for reuse
    Trampoline *t = (Trampoline*)malloc(sizeof(Trampoline));
    t->srcLocalLimit    = srcLocalLimit;
    t->srcScopeDepth    = srcScopeDepth;
    t->targetScopeDepth = targetScopeDepth;
    t->targetOffset     = targetOffset;
    t->entryOffset      = trampStart;
    t->next = c->trampCache;
    c->trampCache = t;

    return trampStart;
}

static void resolveSafeJumps(Parser *parser) {
    Compiler *compiler = parser->currentCompiler;

    SafeJump *sj = compiler->safeJumps;
    while (sj != NULL) {
        // Find the target function-level label
        Label *label = compiler->labels;
        Label *target = NULL;
        while (label != NULL) {
            if (label->name.length == sj->name.length &&
                memcmp(label->name.start, sj->name.start, label->name.length) == 0) {
                target = label;
                break;
                }
            label = label->next;
        }

        if (!target) {
            parser->current = sj->name;
            error(parser, "Undefined label (safe jump).");
            SafeJump *next = sj->next; free(sj); sj = next; continue;
        }

        // Verify: cannot jump "into" a deeper scope
        if (target->scopeDepth > sj->srcScopeDepth) {
            parser->current = sj->name;
            error(parser, "Safe jump into deeper scope is not allowed.");
            SafeJump *next = sj->next; free(sj); sj = next; continue;
        }

        // Build (or reuse) a cleanup trampoline and patch the placeholder to it.
        int trampStart = getOrMakeTrampoline(parser, sj->srcLocalLimit, sj->srcScopeDepth, target->scopeDepth, target->offset);

        currentChunk(parser)->code[sj->placeholder] = (uint16_t)(trampStart - sj->placeholder - 1);

        SafeJump *next = sj->next;
        free(sj);
        sj = next;
    }

    compiler->safeJumps = NULL;

    // Now that both raw and safe jumps are resolved, free labels and tramp cache.
    Label *L = compiler->labels;
    while (L != NULL) { Label *next = L->next; free(L); L = next; }
    compiler->labels = NULL;

    Trampoline *tt = compiler->trampCache;
    while (tt) { Trampoline *next = tt->next; free(tt); tt = next; }
    compiler->trampCache = NULL;
}

static ObjFunction *endCompiler(Parser *parser)
{
    resolveJmpJumps(parser);
    emitReturn(parser);
    resolveSafeJumps(parser);

    ObjFunction *function = parser->currentCompiler->function;

    if (debugPrintCode && !parser->hadError) {
        printMicroScratchSummary(function);
        disassembleChunk(currentChunk(parser), function->name != NULL ? function->name->chars : "<script>");
    }

    parser->currentCompiler = parser->currentCompiler->enclosing;
    return function;
}

static void beginScope(Parser *parser)
{
    parser->currentCompiler->scopeDepth++;
}

static void resolveLabelsInScope(Parser *parser, int scopeDepth) {
    Compiler *compiler = parser->currentCompiler;

    JmpJump *jmpJump = compiler->gotos;
    JmpJump *prevJmp = NULL;

    while (jmpJump != NULL) {
        // Find a matching label that is not deeper than where the jump was emitted.
        Label *label = compiler->labels;
        bool found = false;

        while (label != NULL) {
            if (label->name.length == jmpJump->name.length &&
                memcmp(label->name.start, jmpJump->name.start, label->name.length) == 0 &&
                label->scopeDepth <= jmpJump->scopeDepth) {
                currentChunk(parser)->code[jmpJump->offset] =
                    (uint16_t)(label->offset - jmpJump->offset - 1);
                found = true;
                break;
                }
            label = label->next;
        }

        if (found) {
            // Remove this jump record from the list.
            JmpJump *next = jmpJump->next;
            if (prevJmp == NULL) {
                compiler->gotos = next;
            } else {
                prevJmp->next = next;
            }
            free(jmpJump);
            jmpJump = next;
        } else {
            // Keep it for later.
            prevJmp = jmpJump;
            jmpJump = jmpJump->next;
        }
    }

    // IMPORTANT: Do NOT remove labels here.
    //
    // Safe jumps (goto/jmp_safe/j*_safe) are resolved in resolveSafeJumps()
    // at function end, after we’ve emitted epilogue/trampolines. They still
    // need access to label metadata. Labels will be freed there.
}

static void endScope(Parser *parser)
{
    int scopeDepth = parser->currentCompiler->scopeDepth;
    parser->currentCompiler->scopeDepth--;

    // Resolve labels in the scope we're exiting
    resolveLabelsInScope(parser, scopeDepth);

    while (parser->currentCompiler->localCount > 0 && parser->currentCompiler->locals[parser->currentCompiler->localCount - 1].depth > parser->currentCompiler->scopeDepth)
    {
        if (parser->currentCompiler->locals[parser->currentCompiler->localCount - 1].isCaptured) {
            emitByte(parser, CLOSE_UPVAL);
        } else {
            emitByte(parser, POP);
        }
        parser->currentCompiler->localCount--;
    }
}

static bool parsePrecedence(Parser *parser, Precedence precedence, bool stopForSoleConst);
static ParseRule *getRule(TokenType type);
static void expression(Parser *parser);
static void declaration(Parser *parser);
static void function(Parser *parser, FunctionType type, const char *name, int nameLength);
static void statement(Parser *parser);
static bool isLabel(Parser *parser);
static void gotoStatement(Parser *parser);
static int countCleanupToDepth(Parser *parser, int targetDepth);
static void emitCleanupToDepth(Parser *parser, int targetDepth);
static void declareLabel(Parser *parser, Token name);

static uint16_t identifierConstant(Parser *parser, Token *name)
{
    return makeConstant(parser, OBJ_VAL(copyString(parser->gc, parser->strings, name->start, name->length)));
}

static bool identifiersEqual(Token *a, Token *b)
{
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Parser *parser, Compiler *compiler, Token *name)
{
    for (int i = compiler->localCount - 1; i >= 0; i--)
    {
        Local *local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name))
        {
            if (local->depth == -1) error(parser, "Can't read local variable in its own initializer.");
            return i;
        }
    }

    return -1;
}

static int addUpvalue(Parser *parser, Compiler *compiler, uint8_t index, bool isLocal)
{
    int upvalueCount = compiler->function->upvalueCount;

    for (int i = 0; i < upvalueCount; i++)
    {
        Upvalue *upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) return i;
    }

    if (upvalueCount == UINT8_COUNT)
    {
        error(parser, "Too many closure variables in function.");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

static int resolveUpvalue(Parser *parser, Compiler *compiler, Token *name)
{
    if (compiler->enclosing == NULL) return -1;

    int local = resolveLocal(parser, compiler->enclosing, name);
    if (local != -1)
    {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(parser, compiler, (uint8_t) local, true);
    }

    int upvalue = resolveUpvalue(parser, compiler->enclosing, name);
    if (upvalue != -1) return addUpvalue(parser, compiler, (uint8_t) upvalue, false);

    return -1;
}

static void addLocal(Parser *parser, Token name)
{
    if (parser->currentCompiler->localCount == UINT8_COUNT)
    {
        error(parser, "Too many local variables in function.");
        return;
    }

    Local *local = &parser->currentCompiler->locals[parser->currentCompiler->localCount++];
    local->name = name;
    local->depth = -1;
    local->isCaptured = false;
}

static void declareVariable(Parser *parser)
{
    if (parser->currentCompiler->scopeDepth == 0) return;

    Token *name = &parser->previous;
    for (int i = parser->currentCompiler->localCount - 1; i >= 0; i--)
    {
        Local *local = &parser->currentCompiler->locals[i];
        if (local->depth != -1 && local->depth < parser->currentCompiler->scopeDepth) break;
        if (identifiersEqual(name, &local->name)) error(parser, "Already a variable with this name in this scope.");
    }

    addLocal(parser, *name);
}

static uint16_t parseVariable(Parser *parser, const char *errorMessage)
{
    consume(parser, TOKEN_IDENTIFIER, errorMessage);

    declareVariable(parser);
    if (parser->currentCompiler->scopeDepth > 0) return 0;

    return identifierConstant(parser, &parser->previous);
}

static void markInitialized(Parser *parser)
{
    if (parser->currentCompiler->scopeDepth == 0) return;
    parser->currentCompiler->locals[parser->currentCompiler->localCount - 1].depth = parser->currentCompiler->scopeDepth;
}

static void defineVariable(Parser *parser, uint16_t global)
{
    if (parser->currentCompiler->scopeDepth > 0)
    {
        markInitialized(parser);
        return;
    }

    emitOpShort(parser, DEF_GLOBAL, global);
}

static uint8_t argumentList(Parser *parser)
{
    uint8_t argCount = 0;
    while (true)
    {
        skipNewlines(parser); // <-- allow newlines before argument

        if (check(parser, TOKEN_RIGHT_PAREN)) break;

        expression(parser);

        if (argCount == 255) error(parser, "Can't have more than 255 arguments.");
        argCount++;

        skipNewlines(parser); // <-- allow newlines after argument (before comma or ) )

        if (!match(parser, TOKEN_COMMA)) break;
    }
    consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

static void and_(Parser *parser, bool canAssign)
{
    (void) canAssign;

    int endJump = emitJump(parser, JMP_IF_FALSE);

    emitByte(parser, POP);
    parsePrecedence(parser, PREC_AND, false);

    patchJump(parser, endJump);
}

static double parseNumberLiteral(const char *start, int length);

static void binary(Parser *parser, bool canAssign)
{
    (void) canAssign;

    TokenType operatorType = parser->previous.type;
    ParseRule *rule = getRule(operatorType);
    if (!parsePrecedence(parser, (Precedence) (rule->precedence + 1), true))
    {
        TokenType prevType = parser->previous.type;
        ParseFn prefixRule = NULL;
        if (prevType == TOKEN_NUMBER) {
            double value = parseNumberLiteral(parser->previous.start, parser->previous.length);
            switch (operatorType)
            {
                case TOKEN_EQUAL_EQUAL: emitOpShort(parser, EQ_C, makeConstant(parser, NUMBER_VAL(value))); break;
                case TOKEN_BANG_EQUAL: emitOpShort(parser, NEQ_C, makeConstant(parser, NUMBER_VAL(value))); break;
                case TOKEN_GREATER_EQUAL: emitOpShort(parser, LT_C, makeConstant(parser, NUMBER_VAL(value))); emitByte(parser, NOT); break;
                case TOKEN_GREATER: emitOpShort(parser, GT_C, makeConstant(parser, NUMBER_VAL(value))); break;
                case TOKEN_LESS: emitOpShort(parser, LT_C, makeConstant(parser, NUMBER_VAL(value))); break;
                case TOKEN_PLUS: emitOpShort(parser, ADD_C, makeConstant(parser, NUMBER_VAL(value))); break;
                case TOKEN_MINUS: emitOpShort(parser, SUB_C, makeConstant(parser, NUMBER_VAL(value))); break;
                case TOKEN_STAR: emitOpShort(parser, MUL_C, makeConstant(parser, NUMBER_VAL(value))); break;
                case TOKEN_SLASH: emitOpShort(parser, DIV_C, makeConstant(parser, NUMBER_VAL(value))); break;
                case TOKEN_PERCENT: emitOpShort(parser, MOD_C, makeConstant(parser, NUMBER_VAL(value))); break;
                case TOKEN_AMPERSAND: emitOpShort(parser, BAND_C, makeConstant(parser, NUMBER_VAL(value))); break;
                case TOKEN_PIPE: emitOpShort(parser, BOR_C, makeConstant(parser, NUMBER_VAL(value))); break;
                case TOKEN_CARET: emitOpShort(parser, BXOR_C, makeConstant(parser, NUMBER_VAL(value))); break;
                case TOKEN_LESS_LESS: emitOpShort(parser, BSHL_C, makeConstant(parser, NUMBER_VAL(value))); break;
                case TOKEN_GREATER_GREATER: emitOpShort(parser, BSHR_C, makeConstant(parser, NUMBER_VAL(value))); break;
                default: prefixRule = getRule(prevType)->prefix;
            }
        } else if (prevType == TOKEN_STRING) {
            if (operatorType == TOKEN_PLUS) {
                ObjString *str = copyString(parser->gc, parser->strings, parser->previous.start + 1, parser->previous.length - 2);
                pushTemp(parser->gc, OBJ_VAL(str));
                emitOpShort(parser, ADD_C, makeConstant(parser, OBJ_VAL(str)));
                popTemp(parser->gc);
            } else if (operatorType == TOKEN_EQUAL_EQUAL) {
                ObjString *str = copyString(parser->gc, parser->strings, parser->previous.start + 1, parser->previous.length - 2);
                pushTemp(parser->gc, OBJ_VAL(str));
                emitOpShort(parser, EQ_C, makeConstant(parser, OBJ_VAL(str)));
                popTemp(parser->gc);
            } else if (operatorType == TOKEN_BANG_EQUAL) {
                ObjString *str = copyString(parser->gc, parser->strings, parser->previous.start + 1, parser->previous.length - 2);
                pushTemp(parser->gc, OBJ_VAL(str));
                emitOpShort(parser, NEQ_C, makeConstant(parser, OBJ_VAL(str)));
                popTemp(parser->gc);
            } else {
                prefixRule = getRule(prevType)->prefix;
            }
        } else {
            prefixRule = getRule(prevType)->prefix;
        }

        if (prefixRule != NULL) {
            prefixRule(parser, false);
        } else {
            return;
        }
    }

    switch (operatorType)
    {
        case TOKEN_BANG_EQUAL: emitByte(parser, NEQ); break;
        case TOKEN_EQUAL_EQUAL: emitByte(parser, EQ); break;
        case TOKEN_GREATER: emitByte(parser, GT); break;
        case TOKEN_GREATER_EQUAL: emitBytes(parser, LT, NOT); break;
        case TOKEN_LESS: emitByte(parser, LT); break;
        case TOKEN_LESS_EQUAL: emitBytes(parser, GT, NOT); break;
        case TOKEN_PLUS: emitByte(parser, ADD); break;
        case TOKEN_MINUS: emitByte(parser, SUB); break;
        case TOKEN_STAR: emitByte(parser, MUL); break;
        case TOKEN_SLASH: emitByte(parser, DIV); break;
        case TOKEN_PERCENT: emitByte(parser, MOD); break;
        case TOKEN_AMPERSAND: emitByte(parser, BAND); break;
        case TOKEN_PIPE: emitByte(parser, BOR); break;
        case TOKEN_CARET: emitByte(parser, BXOR); break;
        case TOKEN_LESS_LESS: emitByte(parser, BSHL); break;
        case TOKEN_GREATER_GREATER: emitByte(parser, BSHR); break;
        default: return; // Unreachable.
    }
}

static void ternary(Parser *parser, bool canAssign) {
    int thenJump = emitJump(parser, PJMP_IF_FALSE);
    expression(parser);
    int elseJump = emitJump(parser, JMP);
    patchJump(parser, thenJump);
    consume(parser, TOKEN_COLON, "Expect ':' after then branch of ternary operator.");
    expression(parser);
    patchJump(parser, elseJump);
}

static void call(Parser *parser, bool canAssign)
{
    (void) canAssign;

    uint8_t argCount = argumentList(parser);
    emitBytes(parser, CALL, argCount);
}

static bool isCompoundAssignment(Parser* parser) {
    switch (parser->current.type) {
        case TOKEN_PLUS_EQUAL:
        case TOKEN_MINUS_EQUAL:
        case TOKEN_STAR_EQUAL:
        case TOKEN_SLASH_EQUAL:
        case TOKEN_PERCENT_EQUAL:
        case TOKEN_AMPERSAND_EQUAL:
        case TOKEN_PIPE_EQUAL:
        case TOKEN_CARET_EQUAL:
        case TOKEN_LESS_LESS_EQUAL:
        case TOKEN_GREATER_GREATER_EQUAL:
            return true;
        default:
            return false;
    }
}

static uint16_t getBinaryOpForAssignment(Token* operatorToken) {
    switch (operatorToken->type) {
        case TOKEN_PLUS_EQUAL:              return ADD;
        case TOKEN_MINUS_EQUAL:             return SUB;
        case TOKEN_STAR_EQUAL:              return MUL;
        case TOKEN_SLASH_EQUAL:             return DIV;
        case TOKEN_PERCENT_EQUAL:           return MOD;
        case TOKEN_AMPERSAND_EQUAL:         return BAND;
        case TOKEN_PIPE_EQUAL:              return BOR;
        case TOKEN_CARET_EQUAL:             return BXOR;
        case TOKEN_LESS_LESS_EQUAL:         return BSHL;
        case TOKEN_GREATER_GREATER_EQUAL:   return BSHR;
        default:                            return (uint16_t)-1; // Unreachable
    }
}

static void dot(Parser *parser, bool canAssign)
{
    consume(parser, TOKEN_IDENTIFIER, "Expect property name after '.'.");
    uint16_t name = identifierConstant(parser, &parser->previous);

    if (canAssign && match(parser, TOKEN_EQUAL)) {
        expression(parser);
        emitOpShort(parser, ST_PROP, name);
    } else if (canAssign && match(parser, TOKEN_PLUS_PLUS)) {
        emitOpShort(parser, POST_INC_PROP, name);
    } else if (canAssign && match(parser, TOKEN_MINUS_MINUS)) {
        emitOpShort(parser, POST_DEC_PROP, name);
    } else if (match(parser, TOKEN_LEFT_PAREN)) {
        uint8_t argCount = argumentList(parser);
        emitOpShort(parser, INVOKE, name);
        emitByte(parser, argCount);
    } else {
        emitOpShort(parser, LD_PROP, name);
    }
}

static void lambda(Parser *parser, bool canAssign)
{
    (void) canAssign;
    function(parser, TYPE_FUNCTION, "()", 2);
}

static void list(Parser *parser, bool canAssign)
{
    (void) canAssign;

    emitByte(parser, MAKE_LIST);
    do
    {
        if (check(parser, TOKEN_RIGHT_SQUARE)) break;
        expression(parser);
        emitByte(parser, LIST_DATA);
    } while (match(parser, TOKEN_COMMA));
    consume(parser, TOKEN_RIGHT_SQUARE, "Expect ']' after list.");
}

static void literal(Parser *parser, bool canAssign)
{
    (void) canAssign;

    switch (parser->previous.type)
    {
        case TOKEN_FALSE:
            emitByte(parser, PUSH_FALSE);
            break;
        case TOKEN_NULL:
            emitByte(parser, PUSH_NULL);
            break;
        case TOKEN_TRUE:
            emitByte(parser, PUSH_TRUE);
            break;
        default:
            return; // Unreachable.
    }
}

static void grouping(Parser *parser, bool canAssign)
{
    (void) canAssign;

    expression(parser);
    consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void index_(Parser *parser, bool canAssign)
{
    expression(parser);
    consume(parser, TOKEN_RIGHT_SQUARE, "Expect ']' after expression.");

    if (canAssign && isCompoundAssignment(parser)) {
        Token operator = parser->current;
        advance(parser); // Consume operator

        emitByte(parser, DUP2);
        emitByte(parser, LD_INDEX);

        expression(parser);
        emitByte(parser, getBinaryOpForAssignment(&operator));

        emitByte(parser, ST_INDEX);
    } else if (canAssign && match(parser, TOKEN_EQUAL)) {
        expression(parser);
        emitByte(parser, ST_INDEX);
    } else if (canAssign && match(parser, TOKEN_PLUS_PLUS)) {
        emitByte(parser, POST_INC_INDEX);
    } else if (canAssign && match(parser, TOKEN_MINUS_MINUS)) {
        emitByte(parser, POST_DEC_INDEX);
    } else {
        emitByte(parser, LD_INDEX);
    }
}

static void map(Parser *parser, bool canAssign)
{
    (void) canAssign;

    emitByte(parser, MAKE_MAP);
    do
    {
        if (check(parser, TOKEN_RIGHT_BRACE)) break;

        // Parse the key
        if (match(parser, TOKEN_LEFT_SQUARE)) {
            // Evaluated key: m = {[myVar]: "value"}
            expression(parser);
            consume(parser, TOKEN_RIGHT_SQUARE, "Expect ']' after expression.");
        } else if (match(parser, TOKEN_STRING)) {
            // String literal key: m = {"key": "value"}
            Value s = OBJ_VAL(copyString(parser->gc, parser->strings, parser->previous.start + 1, parser->previous.length - 2));
            pushTemp(parser->gc, s);
            emitConstant(parser, s);
            popTemp(parser->gc);
        } else if (match(parser, TOKEN_NUMBER)) {
            // Number literal key, converted to string at compile time: m = {5: "value"}
            double number = parseNumberLiteral(parser->previous.start, parser->previous.length);
            char buffer[32];
            int length = snprintf(buffer, sizeof(buffer), "%g", number);
            Value s = OBJ_VAL(copyString(parser->gc, parser->strings, buffer, length));
            pushTemp(parser->gc, s);
            emitConstant(parser, s);
            popTemp(parser->gc);
        }
        else {
            // Identifier key: m = {key: "value"}
            consume(parser, TOKEN_IDENTIFIER, "Expect map key (identifier, string, number, or '[').");
            uint16_t constant = identifierConstant(parser, &parser->previous);
            emitOpShort(parser, PUSH_CONST, constant);
        }

        consume(parser, TOKEN_COLON, "Expect ':' after map key.");
        expression(parser);
        emitByte(parser, MAP_DATA);
    } while (match(parser, TOKEN_COMMA));
    consume(parser, TOKEN_RIGHT_BRACE, "Expect '}' after map.");
}

static double parseNumberLiteral(const char *start, int length) {
    double value;

    if (length >= 2 && start[0] == '0' && (start[1] == 'x' || start[1] == 'X')) {
        long long hex = 0;
        for (int i = 2; i < length; i++) {
            char c = start[i];
            if (c == '_') continue;
            if (c >= '0' && c <= '9') {
                hex = hex * 16 + (c - '0');
            } else if (c >= 'a' && c <= 'f') {
                hex = hex * 16 + (10 + (c - 'a'));
            } else if (c >= 'A' && c <= 'F') {
                hex = hex * 16 + (10 + (c - 'A'));
            } else {
                break;
            }
        }
        value = (double)hex;
    } else if (length >= 2 && start[0] == '0' && (start[1] == 'b' || start[1] == 'B')) {
        long long bin = 0;
        for (int i = 2; i < length; i++) {
            char c = start[i];
            if (c == '_') continue;
            if (c == '0' || c == '1') {
                bin = bin * 2 + (c - '0');
            } else {
                break;
            }
        }
        value = (double)bin;
    } else {
        char *clean = (char *)malloc(length + 1);
        int pos = 0;
        for (int i = 0; i < length; i++) {
            if (start[i] != '_') {
                clean[pos++] = start[i];
            }
        }
        clean[pos] = '\0';
        value = strtod(clean, NULL);
        free(clean);
    }
    return value;
}

static void number(Parser *parser, bool canAssign)
{
    (void) canAssign;
    double value = parseNumberLiteral(parser->previous.start, parser->previous.length);
    emitConstant(parser, NUMBER_VAL(value));
}

static void or_(Parser *parser, bool canAssign)
{
    (void) canAssign;

    // If the first operand is true, we can short-circuit
    int endJump = emitJump(parser, JMP_IF_TRUE);
    
    // Otherwise, pop the first operand and evaluate the second
    emitByte(parser, POP);
    parsePrecedence(parser, PREC_OR, false);
    
    patchJump(parser, endJump);
}

static void string(Parser *parser, bool canAssign)
{
    (void) canAssign;

    Value s = OBJ_VAL(copyString(parser->gc, parser->strings, parser->previous.start + 1, parser->previous.length - 2));
    pushTemp(parser->gc, s);
    emitConstant(parser, s);
    popTemp(parser->gc);
}

static void namedVariable(Parser *parser, Token name, bool canAssign)
{
    ObjString* nameString = copyString(parser->gc, parser->strings, name.start, name.length);
    pushTemp(parser->gc, OBJ_VAL(nameString));
    Value nativeId;

    // Check if it's a fast native call
    if (tableGet(&parser->currentCompiler->parser->fastNatives, nameString, &nativeId)) {
        if (match(parser, TOKEN_LEFT_PAREN)) {
            // It's a direct call, e.g., sin(x). Emit the fast path.
            uint8_t argCount = argumentList(parser);
            emitOpShort(parser, FAST_NATIVE_CALL, (uint16_t)AS_NUMBER(nativeId));
            emitByte(parser, argCount);
            popTemp(parser->gc);
            return;
        }
    }
    popTemp(parser->gc);


    uint8_t getOp, setOp;
    int arg = resolveLocal(parser, parser->currentCompiler, &name);

    if (arg != -1) {
        getOp = LD_LOCAL;
        setOp = ST_LOCAL;
    } else if ((arg = resolveUpvalue(parser, parser->currentCompiler, &name)) != -1) {
        getOp = LD_UPVAL;
        setOp = ST_UPVAL;
    } else {
        arg = identifierConstant(parser, &name);
        getOp = LD_GLOBAL;
        setOp = ST_GLOBAL;
    }

    if (canAssign && isCompoundAssignment(parser)) {
        Token operator = parser->current;
        advance(parser); // Consume operator

        if (getOp == LD_GLOBAL) emitOpShort(parser, getOp, (uint16_t)arg);
        else emitBytes(parser, (uint16_t)getOp, (uint8_t)arg);

        expression(parser);

        emitByte(parser, getBinaryOpForAssignment(&operator));
        if (setOp == ST_GLOBAL) emitOpShort(parser, setOp, (uint16_t)arg);
        else emitBytes(parser, (uint16_t)setOp, (uint8_t)arg);

    } else if (canAssign && match(parser, TOKEN_EQUAL)) {
        expression(parser);
        if (setOp == ST_GLOBAL) {
            emitOpShort(parser, setOp, (uint16_t) arg);
        } else {
            emitBytes(parser, (uint16_t) setOp, (uint8_t) arg);
        }
    } else if (canAssign && match(parser, TOKEN_PLUS_PLUS)) {
        uint16_t incOp = 0;
        if (getOp == LD_LOCAL) incOp = POST_INC_LOCAL;
        else if (getOp == LD_UPVAL) incOp = POST_INC_UPVAL;
        else incOp = POST_INC_GLOBAL;

        if (incOp == POST_INC_GLOBAL) {
            emitOpShort(parser, incOp, (uint16_t) arg);
        } else {
            emitBytes(parser, incOp, (uint8_t) arg);
        }
    } else if (canAssign && match(parser, TOKEN_MINUS_MINUS)) {
        uint16_t decOp = 0;
        if (getOp == LD_LOCAL) decOp = POST_DEC_LOCAL;
        else if (getOp == LD_UPVAL) decOp = POST_DEC_UPVAL;
        else decOp = POST_DEC_GLOBAL;

        if (decOp == POST_DEC_GLOBAL) {
            emitOpShort(parser, decOp, (uint16_t) arg);
        } else {
            emitBytes(parser, decOp, (uint8_t) arg);
        }
    } else {
        if (getOp == LD_GLOBAL) {
            emitOpShort(parser, getOp, (uint16_t) arg);
        } else {
            emitBytes(parser, (uint16_t) getOp, (uint8_t) arg);
        }
    }
}

static void prefixIncDec(Parser* parser, bool canAssign) {
    if (!canAssign) {
        error(parser, "Invalid assignment target.");
    }
    bool isInc = parser->previous.type == TOKEN_PLUS_PLUS;

    parsePrecedence(parser, PREC_CALL, false);

    Chunk* chunk = currentChunk(parser);
    int lastOpOffset = -1;
    uint16_t lastOpCode = 0;

    if (chunk->count >= 2) {
        uint16_t op = chunk->code[chunk->count - 2];
        if (op == LD_LOCAL || op == LD_UPVAL || op == LD_PROP || op == LD_GLOBAL || op == LD_GLOBAL_I) {
            lastOpCode = op;
            lastOpOffset = chunk->count - 2;
        }
    }
    if (lastOpOffset == -1 && chunk->count >= 1) {
        uint16_t op = chunk->code[chunk->count - 1];
        if (op == LD_INDEX) {
            lastOpCode = op;
            lastOpOffset = chunk->count - 1;
        }
    }

    if (lastOpOffset == -1) {
        error(parser, "Invalid assignment target for prefix operator.");
        return;
    }

    uint16_t newOp = 0;
    switch(lastOpCode) {
        case LD_LOCAL:      newOp = isInc ? PRE_INC_LOCAL    : PRE_DEC_LOCAL; break;
        case LD_GLOBAL:     newOp = isInc ? PRE_INC_GLOBAL   : PRE_DEC_GLOBAL; break;
        case LD_GLOBAL_I:   newOp = isInc ? PRE_INC_GLOBAL_I : PRE_DEC_GLOBAL_I; break;
        case LD_UPVAL:      newOp = isInc ? PRE_INC_UPVAL    : PRE_DEC_UPVAL; break;
        case LD_PROP:       newOp = isInc ? PRE_INC_PROP     : PRE_DEC_PROP; break;
        case LD_INDEX:      newOp = isInc ? PRE_INC_INDEX    : PRE_DEC_INDEX; break;
        default:
            error(parser, "Invalid assignment target for prefix operator.");
            return;
    }
    chunk->code[lastOpOffset] = newOp;
}

static void variable(Parser *parser, bool canAssign)
{
    namedVariable(parser, parser->previous, canAssign);
}

static Token syntheticToken(const char *text)
{
    Token token;
    token.start = text;
    token.length = (int) strlen(text);
    return token;
}

static bool tokenEqualsCI(Token *t, const char *kw) {
    int n = t->length;
    for (int i = 0; i < n && kw[i]; i++) {
        char a = t->start[i];
        char b = kw[i];
        if (a >= 'A' && a <= 'Z') a = (char)(a - 'A' + 'a');
        if (b >= 'A' && b <= 'Z') b = (char)(b - 'A' + 'a');
        if (a != b) return false;
        if (i == n - 1 && kw[i + 1] != '\0') return false;
        if (kw[i + 1] == '\0' && i != n - 1) return false;
    }
    return (int)strlen(kw) == n;
}

static bool parseRegisterToken(Token *t, uint8_t *outIndex) {
    // Accept i0..i7 (int), d0..d7 or f0..f7 (double)
    if (t->length != 2) return false;
    char k = t->start[0];
    char d = t->start[1];
    if (d < '0' || d > '7') return false;

    if (k == 'i' || k == 'I') {
        *outIndex = (uint8_t)(d - '0');             // 0..7
        return true;
    }
    if (k == 'd' || k == 'D' || k == 'f' || k == 'F') {
        *outIndex = (uint8_t)(8 + (d - '0'));       // 8..15
        return true;
    }
    return false;
}

static uint16_t packRegs(uint8_t p1, uint8_t p2, uint8_t p3, uint8_t p4) {
    return (uint16_t)(((p1 & 0xF) << 12) | ((p2 & 0xF) << 8) | ((p3 & 0xF) << 4) | (p4 & 0xF));
}

// Emit a value on the stack for an identifier (local/upvalue/global)
static void emitLoadIdentifierValue(Parser *parser, Token nameTok) {
    namedVariable(parser, nameTok, false); // pushes the variable's value
}

static int emitRegJump(Parser *parser, uint16_t op, uint8_t reg) {
    // word0: op
    // word1: packed reg nibble
    // word2: 16-bit offset placeholder (0xFFFF)
    emitOpShort(parser, op, packRegs(reg, 0, 0, 0));
    int patchIndex = currentChunk(parser)->count;
    emitByte(parser, 0xFFFF);
    return patchIndex;
}

static void emitImmI64(Parser *parser, int64_t v) {
    uint16_t w[4];
    memcpy(w, &v, 8);
    emitByte(parser, w[0]);
    emitByte(parser, w[1]);
    emitByte(parser, w[2]);
    emitByte(parser, w[3]);
}

static void emitImmF64(Parser *parser, double v) {
    uint16_t w[4];
    memcpy(w, &v, 8);
    emitByte(parser, w[0]);
    emitByte(parser, w[1]);
    emitByte(parser, w[2]);
    emitByte(parser, w[3]);
}

static void micro_emitImmPayload(Parser *parser, bool dstIsInt, double v) {
    if (dstIsInt) {
        int64_t iv = (int64_t)v;
        uint16_t w[4]; memcpy(w, &iv, 8);
        emitByte(parser, w[0]); emitByte(parser, w[1]); emitByte(parser, w[2]); emitByte(parser, w[3]);
    } else {
        double dv = v;
        uint16_t w[4]; memcpy(w, &dv, 8);
        emitByte(parser, w[0]); emitByte(parser, w[1]); emitByte(parser, w[2]); emitByte(parser, w[3]);
    }
}

// Parse either a register or a number literal (with optional leading '-')
// Returns true and fills outReg or outNum accordingly.
// Leaves parser at the token after the operand.
static bool micro_parseRegOrNumber(Parser *parser, uint8_t *outReg, bool *isImm, double *outNum) {
    // try register first
    if (match(parser, TOKEN_IDENTIFIER)) {
        uint8_t r = 0xFF;
        if (parseRegisterToken(&parser->previous, &r)) {
            *isImm = false; *outReg = r; return true;
        }
        // Not a register; put back an error
        error(parser, "Expect register or number.");
        return false;
    }
    // optional '-' for negative
    bool neg = false;
    if (match(parser, TOKEN_MINUS)) neg = true;
    if (match(parser, TOKEN_NUMBER)) {
        double v = parseNumberLiteral(parser->previous.start, parser->previous.length);
        if (neg) v = -v;
        *isImm = true; *outNum = v; return true;
    }
    error(parser, "Expect register or number.");
    return false;
}

static void recordSafeJump(Parser *parser, Token labelName, int placeholderWordIndex) {
    SafeJump *sj = (SafeJump*)malloc(sizeof(SafeJump));
    sj->name          = labelName;
    sj->placeholder   = placeholderWordIndex;
    sj->srcLocalLimit = parser->currentCompiler->localCount;
    sj->srcScopeDepth = parser->currentCompiler->scopeDepth;
    sj->next          = parser->currentCompiler->safeJumps;
    parser->currentCompiler->safeJumps = sj;
}

static int countCleanupToDepth(Parser *parser, int targetDepth) {
    Compiler *c = parser->currentCompiler;
    int n = 0;
    for (int i = c->localCount - 1; i >= 0 && c->locals[i].depth > targetDepth; --i)
    {
        // one instruction per local (POP or CLOSE_UPVAL)
        n++;
    }
    return n;
}

// Called by the compiler to unwind codegen to a shallower scope
static void emitCleanupToDepth(Parser *parser, int targetDepth) {
    Compiler *c = parser->currentCompiler;
    for (int i = c->localCount - 1; i >= 0 && c->locals[i].depth > targetDepth; --i) {
        emitByte(parser, c->locals[i].isCaptured ? CLOSE_UPVAL : POP);
    }
}

// Accept optional handle token "h0".."h15" (case-insensitive). Returns true if consumed.
static bool tryParseListHandle(Parser* p, uint8_t* outH) {
    if (!check(p, TOKEN_IDENTIFIER)) return false;

    Token t = p->current; // don't advance yet
    if (t.length < 2) return false;

    char c = t.start[0];
    if (c != 'h' && c != 'H') return false;

    // Parse decimal digits after 'h'
    int v = 0;
    for (int i = 1; i < t.length; ++i) {
        unsigned char ch = (unsigned char)t.start[i];
        if (ch < '0' || ch > '9') return false; // not purely h<digits> => not a handle token
        v = v * 10 + (int)(ch - '0');
        if (v > 255) break;
    }

    if (v < 0 || v >= MS_MAX_LBIND) {
        // Consume it to avoid infinite loops, but report an error.
        advance(p);
        error(p, "List handle out of range.");  // keep messages simple (your error() takes const char*)
        // Fallback to default 0 so subsequent code can keep going.
        if (outH) *outH = 0;
        return true; // token consumed
    }

    // OK: consume and set
    advance(p);
    if (outH) *outH = (uint8_t)v;
    return true;
}

static void microasmBlock(Parser *parser, bool restrictToLocals)
{
    // ---- Per-block structures ----
    typedef struct MicroLabel {
        Token name;
        int offset;
        struct MicroLabel *next;
    } MicroLabel;

    // Forward fixups for block-local targets (".label").
    typedef struct MicroLocalFix {
        Token name;        // identifier token (without the dot)
        int patchIndex;    // index of the 16-bit placeholder word to patch
        struct MicroLocalFix *next;
    } MicroLocalFix;

    MicroLabel    *mlabels     = NULL;    // block-local labels in this microasm block
    MicroLocalFix *localFixups = NULL;    // pending forward jumps to block-local labels

    uint16_t blockMaxI = 0;
    uint16_t blockMaxF = 0;

    // Find a block-local label by name (token equality).
    #define FIND_LOCAL_LABEL(tokPtr, outVar)                                                    \
        do {                                                                                    \
            MicroLabel *tmp__ = mlabels;                                                        \
            (outVar) = NULL;                                                                    \
            while (tmp__ != NULL) {                                                             \
                if (tmp__->name.length == (tokPtr)->length &&                                   \
                    memcmp(tmp__->name.start, (tokPtr)->start, tmp__->name.length) == 0) {      \
                    (outVar) = tmp__;                                                           \
                    break;                                                                      \
                }                                                                               \
                tmp__ = tmp__->next;                                                            \
            }                                                                                   \
        } while (0)

    // ---- Parse until '}' or EOF ----
    while (!check(parser, TOKEN_RIGHT_BRACE) && !check(parser, TOKEN_EOF)) {
        skipNewlines(parser);
        if (check(parser, TOKEN_RIGHT_BRACE) || check(parser, TOKEN_EOF)) break;

        // Block-local label line? (e.g., "foo:")
        if (isLabel(parser)) {
            Token labelName = parser->current;      // identifier
            advance(parser);                        // consume ident
            consume(parser, TOKEN_COLON, "Expect ':' after label name.");

            // For GENERAL microasm only (restrictToLocals == false), also publish this
            // label to the function-level label table so that script-level jmp/goto
            // can target it.
            if (!restrictToLocals) {
                // declareLabel() already checks for duplicates in the current scope
                declareLabel(parser, labelName);
            }

            // Keep a block-local label so ".name" jumps inside this microasm block
            // can be resolved via MicroLocalFix.
            MicroLabel *existing = NULL;
            FIND_LOCAL_LABEL(&labelName, existing);
            if (existing != NULL) {
                error(parser, "Label already defined in this microasm block.");
            } else {
                MicroLabel *node = (MicroLabel*)malloc(sizeof(MicroLabel));
                node->name   = labelName;
                node->offset = currentChunk(parser)->count;
                node->next   = mlabels;
                mlabels      = node;
            }

            skipNewlines(parser);
            continue;
        }

        // Support `return <reg>;` inside microasm (function bodies only)
        if (match(parser, TOKEN_RETURN)) {
            consume(parser, TOKEN_IDENTIFIER, "Expect register after 'return' in microasm.");
            uint8_t regIdx = 0xFF;
            if (!parseRegisterToken(&parser->previous, &regIdx)) {
                error(parser, "Invalid register in microasm return.");
            } else {
                if (parser->currentCompiler->type == TYPE_SCRIPT) {
                    error(parser, "Can't return from top-level code.");
                } else {
                    emitOpShort(parser, R_RET_R, packRegs(regIdx, 0, 0, 0));
                }
            }
            consumeEndOfStatement(parser, "Expect ';' or newline after microasm return.");
            continue;
        }

        // Raw 'jmp' keyword (works with ".label" or "Name")
        if (match(parser, TOKEN_JMP)) {
            bool isLocal = false;

            // Accept "jmp .label" (block-local) OR "jmp Name" (function-level)
            if (match(parser, TOKEN_DOT)) {
                isLocal = true;
                consume(parser, TOKEN_IDENTIFIER, "Expect label after '.'.");
            } else {
                consume(parser, TOKEN_IDENTIFIER, "Expect label name.");
            }
            Token tgt = parser->previous;

            if (isLocal) {
                // Block-local jmp
                MicroLabel *lbl = NULL;
                FIND_LOCAL_LABEL(&tgt, lbl);

                if (lbl && lbl->offset <= currentChunk(parser)->count) {
                    // Backward within block: JMP_BACK
                    int back = currentChunk(parser)->count - lbl->offset + 2;
                    emitByte(parser, JMP_BACK);
                    emitByte(parser, (uint16_t)back);
                } else {
                    // Forward within block: emit placeholder + record local fixup
                    int patch = emitJump(parser, JMP);
                    MicroLocalFix *m = (MicroLocalFix*)malloc(sizeof(MicroLocalFix));
                    m->name = tgt; m->patchIndex = patch; m->next = localFixups; localFixups = m;
                }
            } else {
                // Function-level jmp (only allowed in general microasm)
                if (restrictToLocals) {
                    error(parser, "Function microasm can only jump to block-local labels (use '.name').");
                    consumeEndOfStatement(parser, "Expect ';' or newline.");
                    continue;
                }

                // If already defined and behind -> JMP_BACK, else record via compiler->gotos
                Label *fl = parser->currentCompiler->labels;
                Label *found = NULL;
                while (fl) {
                    if (fl->name.length == tgt.length &&
                        memcmp(fl->name.start, tgt.start, tgt.length) == 0 &&
                        fl->scopeDepth <= parser->currentCompiler->scopeDepth) { found = fl; break; }
                    fl = fl->next;
                }
                if (found && found->offset <= currentChunk(parser)->count) {
                    int back = currentChunk(parser)->count - found->offset + 2;
                    emitByte(parser, JMP_BACK);
                    emitByte(parser, (uint16_t)back);
                } else {
                    int jumpOffset = emitJump(parser, JMP);
                    JmpJump *node = (JmpJump*)malloc(sizeof(JmpJump));
                    node->name = tgt;
                    node->offset = jumpOffset;                 // placeholder word index
                    node->scopeDepth = parser->currentCompiler->scopeDepth;
                    node->next = parser->currentCompiler->gotos;
                    parser->currentCompiler->gotos = node;
                }
            }

            consumeEndOfStatement(parser, "Expect ';' or newline.");
            continue;
        }

        // Otherwise: mnemonic as identifier
        consume(parser, TOKEN_IDENTIFIER, "Expect microasm instruction.");
        Token op = parser->previous;

        // -------------------------
        // Unconditional SAFE jump: jmp_safe .label | Name
        //  - Block-local ".label": behaves like raw jmp within the block (no scope change),
        //    supports forward jumps via MicroLocalFix.
        //  - Function-level Name (general microasm only): emits a JMP placeholder and records
        //    a SafeJump so resolveSafeJumps() will route to a deduplicated trampoline that
        //    performs scope cleanup and then jumps to the function-level label.
        // -------------------------
        if (tokenEqualsCI(&op, "jmp_safe")) {
            bool isLocal = false;

            if (match(parser, TOKEN_DOT)) {
                isLocal = true;
                consume(parser, TOKEN_IDENTIFIER, "Expect label after '.'.");
            } else {
                consume(parser, TOKEN_IDENTIFIER, "Expect label name.");
            }
            Token tgt = parser->previous;

            if (isLocal) {
                // ---- block-local safe jump (no cleanup needed) ----
                MicroLabel *lbl = NULL;
                FIND_LOCAL_LABEL(&tgt, lbl);

                if (lbl && lbl->offset <= currentChunk(parser)->count) {
                    // backward within the block
                    int back = currentChunk(parser)->count - lbl->offset + 2;
                    emitByte(parser, JMP_BACK);
                    emitByte(parser, (uint16_t)back);
                } else {
                    // forward within the block -> placeholder + local fixup
                    int patch = emitJump(parser, JMP);
                    MicroLocalFix *m = (MicroLocalFix*)malloc(sizeof(MicroLocalFix));
                    m->name = tgt; m->patchIndex = patch; m->next = localFixups; localFixups = m;
                }
            } else {
                // ---- function-level safe jump (only allowed in general microasm) ----
                if (restrictToLocals) {
                    error(parser, "Function microasm can only jump to block-local labels (use '.name').");
                    consumeEndOfStatement(parser, "Expect ';' or newline after jmp_safe.");
                    continue;
                }

                // Unconditional safe jump uses a trampoline (forward or backward).
                int placeholder = emitJump(parser, JMP);
                recordSafeJump(parser, tgt, placeholder);
            }

            consumeEndOfStatement(parser, "Expect ';' or newline after jmp_safe.");
            continue;
        }

        // -------------------------
        // Conditional reg jumps: jz / jnz
        // (raw - existing behavior)
        // -------------------------
        if (tokenEqualsCI(&op, "jnz") || tokenEqualsCI(&op, "jz")) {
            consume(parser, TOKEN_IDENTIFIER, "Expect register.");
            uint8_t r = 0xFF;
            if (!parseRegisterToken(&parser->previous, &r)) {
                error(parser, "Invalid register.");
                consumeEndOfStatement(parser, "Expect ';' or newline after j* instruction.");
                continue;
            }

            bool isLocal = false;
            if (match(parser, TOKEN_DOT)) {
                isLocal = true;
                consume(parser, TOKEN_IDENTIFIER, "Expect block-local label name.");
            } else {
                consume(parser, TOKEN_IDENTIFIER, "Expect label name.");
            }
            Token tgt = parser->previous;

            if (isLocal) {
                // ---- block-local target ----
                MicroLabel *lbl = NULL;
                FIND_LOCAL_LABEL(&tgt, lbl);

                if (lbl && lbl->offset <= currentChunk(parser)->count) {
                    // invert + JMP_BACK trick
                    uint16_t skip = 2;
                    if (tokenEqualsCI(&op, "jz")) {
                        emitOpShort(parser, R_JNZ, packRegs(r, 0, 0, 0));
                        emitByte(parser, skip);
                    } else {
                        emitOpShort(parser, R_JZ, packRegs(r, 0, 0, 0));
                        emitByte(parser, skip);
                    }
                    int back = currentChunk(parser)->count - lbl->offset + 2;
                    emitByte(parser, JMP_BACK);
                    emitByte(parser, (uint16_t)back);
                } else {
                    // Forward within block -> emit placeholder and record fixup
                    int patchIndex;
                    if (tokenEqualsCI(&op, "jz")) {
                        patchIndex = emitRegJump(parser, R_JZ, r);
                    } else {
                        patchIndex = emitRegJump(parser, R_JNZ, r);
                    }
                    MicroLocalFix *m = (MicroLocalFix*)malloc(sizeof(MicroLocalFix));
                    m->name = tgt; m->patchIndex = patchIndex; m->next = localFixups; localFixups = m;
                }
            } else {
                // ---- function-level target (only allowed in general microasm) ----
                if (restrictToLocals) {
                    error(parser, "Function microasm can only jump to block-local labels (use '.name').");
                    consumeEndOfStatement(parser, "Expect ';' or newline after j* instruction.");
                    continue;
                }

                // If label already defined and behind us -> invert + JMP_BACK.
                Label *fl = parser->currentCompiler->labels;
                Label *found = NULL;
                while (fl) {
                    if (fl->name.length == tgt.length &&
                        memcmp(fl->name.start, tgt.start, tgt.length) == 0 &&
                        fl->scopeDepth <= parser->currentCompiler->scopeDepth) { found = fl; break; }
                    fl = fl->next;
                }

                if (found && found->offset <= currentChunk(parser)->count) {
                    uint16_t skip = 2;
                    if (tokenEqualsCI(&op, "jz")) {
                        emitOpShort(parser, R_JNZ, packRegs(r, 0, 0, 0));
                        emitByte(parser, skip);
                    } else {
                        emitOpShort(parser, R_JZ, packRegs(r, 0, 0, 0));
                        emitByte(parser, skip);
                    }
                    int back = currentChunk(parser)->count - found->offset + 2;
                    emitByte(parser, JMP_BACK);
                    emitByte(parser, (uint16_t)back);
                } else {
                    // Forward function-level conditional: use compiler->gotos with reg-conditional placeholder
                    int patchIndex;
                    if (tokenEqualsCI(&op, "jz")) {
                        patchIndex = emitRegJump(parser, R_JZ, r);
                    } else {
                        patchIndex = emitRegJump(parser, R_JNZ, r);
                    }
                    JmpJump *node = (JmpJump*)malloc(sizeof(JmpJump));
                    node->name = tgt;
                    node->offset = patchIndex;                 // placeholder word index
                    node->scopeDepth = parser->currentCompiler->scopeDepth;
                    node->next = parser->currentCompiler->gotos;
                    parser->currentCompiler->gotos = node;
                }
            }

            consumeEndOfStatement(parser, "Expect ';' or newline after j* instruction.");
            continue;
        }

        // -------------------------
        // Block-local (".label"): same as raw (no scope cleanup needed).
        // Function-level (general microasm only):
        //   - Backward: invert + (cleanup to label scope) + JMP_BACK
        //   - Forward: error for now (needs trampoline infra)
        // -------------------------
        if (tokenEqualsCI(&op, "jz_safe") || tokenEqualsCI(&op, "jnz_safe")) {
            consume(parser, TOKEN_IDENTIFIER, "Expect register.");
            uint8_t r = 0xFF;
            if (!parseRegisterToken(&parser->previous, &r)) {
                error(parser, "Invalid register.");
                consumeEndOfStatement(parser, "Expect ';' or newline after j*_safe.");
                continue;
            }

            bool isLocal = false;
            if (match(parser, TOKEN_DOT)) {
                isLocal = true;
                consume(parser, TOKEN_IDENTIFIER, "Expect block-local label name.");
            } else {
                consume(parser, TOKEN_IDENTIFIER, "Expect label name.");
            }
            Token tgt = parser->previous;

            if (isLocal) {
                // SAFE within block-local: identical to raw jz/jnz behavior (no scope change).
                MicroLabel *lbl = NULL;
                FIND_LOCAL_LABEL(&tgt, lbl);

                if (lbl && lbl->offset <= currentChunk(parser)->count) {
                    uint16_t skip = 2;
                    if (tokenEqualsCI(&op, "jz_safe")) {
                        emitOpShort(parser, R_JNZ, packRegs(r, 0, 0, 0)); emitByte(parser, skip);
                    } else {
                        emitOpShort(parser, R_JZ,  packRegs(r, 0, 0, 0)); emitByte(parser, skip);
                    }
                    int back = currentChunk(parser)->count - lbl->offset + 2;
                    emitByte(parser, JMP_BACK); emitByte(parser, (uint16_t)back);
                } else {
                    int patchIndex;
                    if (tokenEqualsCI(&op, "jz_safe")) {
                        patchIndex = emitRegJump(parser, R_JZ, r);
                    } else {
                        patchIndex = emitRegJump(parser, R_JNZ, r);
                    }
                    MicroLocalFix *m = (MicroLocalFix*)malloc(sizeof(MicroLocalFix));
                    m->name = tgt; m->patchIndex = patchIndex; m->next = localFixups; localFixups = m;
                }
            } else {
                // Function-level SAFE (only allowed in general microasm)
                if (restrictToLocals) {
                    error(parser, "Function microasm can only jump to block-local labels (use '.name').");
                    consumeEndOfStatement(parser, "Expect ';' or newline after j*_safe.");
                    continue;
                }

                Label *fl = parser->currentCompiler->labels;
                Label *found = NULL;
                while (fl) {
                    if (fl->name.length == tgt.length &&
                        memcmp(fl->name.start, tgt.start, tgt.length) == 0 &&
                        fl->scopeDepth <= parser->currentCompiler->scopeDepth) { found = fl; break; }
                    fl = fl->next;
                }

                if (found && found->offset <= currentChunk(parser)->count) {
                    // Compute cleanup size (number of POP/CLOSE_UPVAL)
                    int cleanupN = countCleanupToDepth(parser, found->scopeDepth);
                    uint16_t skip = (uint16_t)(cleanupN + 2); // cleanup + JMP_BACK pair

                    // Invert the condition to SKIP the cleanup+JMP when not taking the branch
                    if (tokenEqualsCI(&op, "jz_safe")) {
                        emitOpShort(parser, R_JNZ, packRegs(r, 0, 0, 0)); emitByte(parser, skip);
                    } else {
                        emitOpShort(parser, R_JZ,  packRegs(r, 0, 0, 0)); emitByte(parser, skip);
                    }

                    // Emit runtime cleanup down to label scope, then a backward jump
                    emitCleanupToDepth(parser, found->scopeDepth);
                    int back = currentChunk(parser)->count - found->offset + 2;
                    emitByte(parser, JMP_BACK);
                    emitByte(parser, (uint16_t)back);
                } else {
                    // ---- function-level SAFE (only allowed in general microasm) ----
                    if (restrictToLocals) {
                        error(parser, "Function microasm can only jump to block-local labels (use '.name').");
                        consumeEndOfStatement(parser, "Expect ';' or newline after j*_safe.");
                        continue;
                    }

                    // For safe conditionals, always branch to a trampoline (works for forward and backward).
                    int patchIndex;
                    if (tokenEqualsCI(&op, "jz_safe")) {
                        patchIndex = emitRegJump(parser, R_JZ, r);
                    } else {
                        patchIndex = emitRegJump(parser, R_JNZ, r);
                    }
                    // Record a SafeJump so resolveSafeJumps() can build/dedupe a trampoline and patch this.
                    recordSafeJump(parser, tgt, patchIndex);
                }
            }

            consumeEndOfStatement(parser, "Expect ';' or newline after j*_safe.");
            continue;
        }

        // -------------------------
        // rload <dst-reg> <src>
        // -------------------------
        if (tokenEqualsCI(&op, "rload")) {
            consume(parser, TOKEN_IDENTIFIER, "Expect destination register.");
            uint8_t dst = 0xFF;
            if (!parseRegisterToken(&parser->previous, &dst)) {
                error(parser, "Invalid destination register.");
            } else {
                if (match(parser, TOKEN_IDENTIFIER)) {
                    Token srcName = parser->previous;

                    int local = resolveLocal(parser, parser->currentCompiler, &srcName);
                    if (local != -1) {
                        emitOpShort(parser, R_LD_LOCAL_R, packRegs(dst, 0, 0, 0));
                        emitByte(parser, (uint16_t)local);
                    } else {
                        if (restrictToLocals) {
                            error(parser, "Function microasm block can only access locals.");
                        } else {
                            int up = resolveUpvalue(parser, parser->currentCompiler, &srcName);
                            if (up != -1) {
                                emitOpShort(parser, R_LD_UPVAL_R, packRegs(dst, 0, 0, 0));
                                emitByte(parser, (uint16_t)up);
                            } else {
                                uint16_t nameConst = identifierConstant(parser, &srcName);
                                emitOpShort(parser, R_LD_GLOBAL_R, packRegs(dst, 0, 0, 0));
                                emitByte(parser, nameConst);
                            }
                        }
                    }
                } else {
                    bool neg = false;
                    if (match(parser, TOKEN_MINUS)) neg = true;
                    consume(parser, TOKEN_NUMBER, "Expect immediate or identifier.");
                    double v = parseNumberLiteral(parser->previous.start, parser->previous.length);
                    if (neg) v = -v;

                    if (dst < 8) {
                        emitOpShort(parser, R_ILOAD_IMM, packRegs(dst, 0, 0, 0));
                        int64_t iv = (int64_t)v; uint16_t w[4]; memcpy(w, &iv, 8);
                        emitByte(parser, w[0]); emitByte(parser, w[1]); emitByte(parser, w[2]); emitByte(parser, w[3]);
                    } else {
                        emitOpShort(parser, R_FLOAD_IMM, packRegs(dst, 0, 0, 0));
                        double dv = v; uint16_t w[4]; memcpy(w, &dv, 8);
                        emitByte(parser, w[0]); emitByte(parser, w[1]); emitByte(parser, w[2]); emitByte(parser, w[3]);
                    }
                }
            }
            consumeEndOfStatement(parser, "Expect ';' or newline after rload.");
            continue;
        }

        // rstore <src-reg> <identifier>
        if (tokenEqualsCI(&op, "rstore")) {
            consume(parser, TOKEN_IDENTIFIER, "Expect source register.");
            uint8_t src = 0xFF;
            if (!parseRegisterToken(&parser->previous, &src)) {
                error(parser, "Invalid source register.");
                consumeEndOfStatement(parser, "Expect ';' or newline after rstore.");
                continue;
            }
            consume(parser, TOKEN_IDENTIFIER, "Expect identifier to store into.");
            Token dstName = parser->previous;

            int local = resolveLocal(parser, parser->currentCompiler, &dstName);
            if (local != -1) {
                emitOpShort(parser, R_ST_LOCAL_R, packRegs(src, 0, 0, 0));
                emitByte(parser, (uint16_t)local);
            } else {
                if (restrictToLocals) {
                    error(parser, "Function microasm block can only access locals.");
                } else {
                    int up = resolveUpvalue(parser, parser->currentCompiler, &dstName);
                    if (up != -1) {
                        emitOpShort(parser, R_ST_UPVAL_R, packRegs(src, 0, 0, 0));
                        emitByte(parser, (uint16_t)up);
                    } else {
                        uint16_t nameConst = identifierConstant(parser, &dstName);
                        emitOpShort(parser, R_ST_GLOBAL_R, packRegs(src, 0, 0, 0));
                        emitByte(parser, nameConst);
                    }
                }
            }
            consumeEndOfStatement(parser, "Expect ';' or newline after rstore.");
            continue;
        }

        // rmov <dst-reg> <src-reg>
        if (tokenEqualsCI(&op, "rmov")) {
            consume(parser, TOKEN_IDENTIFIER, "Expect destination register.");
            uint8_t dst = 0xFF;
            if (!parseRegisterToken(&parser->previous, &dst)) { error(parser, "Invalid destination register."); }
            consume(parser, TOKEN_IDENTIFIER, "Expect source register.");
            uint8_t src = 0xFF;
            if (!parseRegisterToken(&parser->previous, &src)) { error(parser, "Invalid source register."); }
            emitOpShort(parser, R_MOV, packRegs(dst, src, 0, 0));
            consumeEndOfStatement(parser, "Expect ';' or newline after rmov.");
            continue;
        }

        // rpush <reg>
        if (tokenEqualsCI(&op, "rpush")) {
            consume(parser, TOKEN_IDENTIFIER, "Expect register.");
            uint8_t r = 0xFF;
            if (!parseRegisterToken(&parser->previous, &r)) {
                error(parser, "Invalid register.");
            } else {
                emitOpShort(parser, R_PUSH, packRegs(r, 0, 0, 0));
            }
            consumeEndOfStatement(parser, "Expect ';' or newline after rpush.");
            continue;
        }

        // rpop <reg>
        if (tokenEqualsCI(&op, "rpop")) {
            consume(parser, TOKEN_IDENTIFIER, "Expect register.");
            uint8_t r = 0xFF;
            if (!parseRegisterToken(&parser->previous, &r)) {
                error(parser, "Invalid register.");
            } else {
                emitOpShort(parser, R_POP, packRegs(r, 0, 0, 0));
            }
            consumeEndOfStatement(parser, "Expect ';' or newline after rpop.");
            continue;
        }

        // rsst <src-reg> <index>
        if (tokenEqualsCI(&op, "rsst")) {
            consume(parser, TOKEN_IDENTIFIER, "Expect source register.");
            uint8_t src = 0xFF;
            if (!parseRegisterToken(&parser->previous, &src)) {
                error(parser, "Invalid source register for rsst.");
                consumeEndOfStatement(parser, "Expect ';' or newline after rsst.");
                continue;
            }
            bool neg = false;
            if (match(parser, TOKEN_MINUS)) { neg = true; }
            consume(parser, TOKEN_NUMBER, "Expect scratch index.");
            double v = parseNumberLiteral(parser->previous.start, parser->previous.length);
            if (neg) error(parser, "Scratch index must be non-negative.");
            if (v < 0) error(parser, "Scratch index must be non-negative.");
            uint32_t idx = (uint32_t)v;
            if ((double)idx != v) error(parser, "Scratch index must be a whole number.");

            if (src < 8) {
                if (idx >= MICRO_SCRATCH_MAX_I) error(parser, "Integer scratch index exceeds limit.");
                if ((uint16_t)(idx + 1) > blockMaxI) blockMaxI = (uint16_t)(idx + 1);
            } else {
                if (idx >= MICRO_SCRATCH_MAX_F) error(parser, "Float scratch index exceeds limit.");
                if ((uint16_t)(idx + 1) > blockMaxF) blockMaxF = (uint16_t)(idx + 1);
            }
            emitOpShort(parser, R_SST, packRegs(src, 0, 0, 0));
            emitByte(parser, (uint16_t)idx);

            consumeEndOfStatement(parser, "Expect ';' or newline after rsst.");
            continue;
        }

        // rsld <dst-reg> <index>
        if (tokenEqualsCI(&op, "rsld")) {
            consume(parser, TOKEN_IDENTIFIER, "Expect destination register.");
            uint8_t dst = 0xFF;
            if (!parseRegisterToken(&parser->previous, &dst)) {
                error(parser, "Invalid destination register for rsld.");
                consumeEndOfStatement(parser, "Expect ';' or newline after rsld.");
                continue;
            }
            bool neg = false;
            if (match(parser, TOKEN_MINUS)) { neg = true; }
            consume(parser, TOKEN_NUMBER, "Expect scratch index.");
            double v = parseNumberLiteral(parser->previous.start, parser->previous.length);
            if (neg) error(parser, "Scratch index must be non-negative.");
            if (v < 0) error(parser, "Scratch index must be non-negative.");
            uint32_t idx = (uint32_t)v;
            if ((double)idx != v) error(parser, "Scratch index must be a whole number.");

            if (dst < 8) {
                if (idx >= MICRO_SCRATCH_MAX_I) error(parser, "Integer scratch index exceeds limit.");
                if ((uint16_t)(idx + 1) > blockMaxI) blockMaxI = (uint16_t)(idx + 1);
            } else {
                if (idx >= MICRO_SCRATCH_MAX_F) error(parser, "Float scratch index exceeds limit.");
                if ((uint16_t)(idx + 1) > blockMaxF) blockMaxF = (uint16_t)(idx + 1);
            }
            emitOpShort(parser, R_SLD, packRegs(dst, 0, 0, 0));
            emitByte(parser, (uint16_t)idx);

            consumeEndOfStatement(parser, "Expect ';' or newline after rsld.");
            continue;
        }

        // --------------------------------------------------------------------
        // radd / rsub / rmul / rdiv / rmod
        // rand / rbor / rbxor
        // rshl / rshr / rsar
        // rrol / rror
        // rbtst / rbset / rbclr / rbtgl
        // rbnot  (unary):  rbnot <dst> <src>
        //
        // Syntax (binary):
        //   <mnemonic> <dst> <srcA> <srcB>
        // where srcA/srcB is either a register in same bank as <dst> (for reg)
        // or an immediate number (parsed as int64 for int ops, double for float ops).
        // --------------------------------------------------------------------

        // ---- unary: rbnot <dst> <src> ----
        if (tokenEqualsCI(&op, "rbnot")) {
            // dst
            consume(parser, TOKEN_IDENTIFIER, "Expect destination register.");
            uint8_t dst = 0xFF;
            if (!parseRegisterToken(&parser->previous, &dst)) {
                error(parser, "Invalid destination register.");
                consumeEndOfStatement(parser, "Expect ';' or newline after rbnot.");
                continue;
            }
            // src
            consume(parser, TOKEN_IDENTIFIER, "Expect source register.");
            uint8_t src = 0xFF;
            if (!parseRegisterToken(&parser->previous, &src)) {
                error(parser, "Invalid source register.");
                consumeEndOfStatement(parser, "Expect ';' or newline after rbnot.");
                continue;
            }
            if (dst >= 8 || src >= 8) {
                error(parser, "rbnot requires integer registers (i0..i7).");
                consumeEndOfStatement(parser, "Expect ';' or newline after rbnot.");
                continue;
            }
            emitOpShort(parser, R_IBNOT, packRegs(dst, src, 0, 0));
            consumeEndOfStatement(parser, "Expect ';' or newline after rbnot.");
            continue;
        }

        {
            // ---- classify mnemonic ----
            enum {
                AK_NONE = 0,
                // arithmetic (dst decides I/F)
                AK_ADD, AK_SUB, AK_MUL, AK_DIV, AK_MOD,
                // bitwise (INT only)
                BK_AND, BK_OR, BK_XOR,
                // shifts / rotates (INT only)
                SK_SHL, SK_SHR, SK_SAR, RK_ROL, RK_ROR,
                // bit twiddles (INT only)
                TK_BTST, TK_BSET, TK_BCLR, TK_BTGL
            } kind = AK_NONE;

            if      (tokenEqualsCI(&op, "radd"))  kind = AK_ADD;
            else if (tokenEqualsCI(&op, "rsub"))  kind = AK_SUB;
            else if (tokenEqualsCI(&op, "rmul"))  kind = AK_MUL;
            else if (tokenEqualsCI(&op, "rdiv"))  kind = AK_DIV;
            else if (tokenEqualsCI(&op, "rmod"))  kind = AK_MOD;

            else if (tokenEqualsCI(&op, "rand"))  kind = BK_AND;
            else if (tokenEqualsCI(&op, "rbor"))  kind = BK_OR;   // use rbor (avoid clash with rror)
            else if (tokenEqualsCI(&op, "rbxor")) kind = BK_XOR;

            else if (tokenEqualsCI(&op, "rshl"))  kind = SK_SHL;
            else if (tokenEqualsCI(&op, "rshr"))  kind = SK_SHR;
            else if (tokenEqualsCI(&op, "rsar"))  kind = SK_SAR;

            else if (tokenEqualsCI(&op, "rrol"))  kind = RK_ROL;
            else if (tokenEqualsCI(&op, "rror"))  kind = RK_ROR;

            else if (tokenEqualsCI(&op, "rbtst")) kind = TK_BTST;
            else if (tokenEqualsCI(&op, "rbset")) kind = TK_BSET;
            else if (tokenEqualsCI(&op, "rbclr")) kind = TK_BCLR;
            else if (tokenEqualsCI(&op, "rbtgl")) kind = TK_BTGL;

            if (kind != AK_NONE) {
                // --- dst ---
                consume(parser, TOKEN_IDENTIFIER, "Expect destination register.");
                uint8_t dst = 0xFF;
                if (!parseRegisterToken(&parser->previous, &dst)) {
                    error(parser, "Invalid destination register.");
                }

                bool dstIsInt = (dst < 8);
                bool isArithmetic = (kind >= AK_ADD && kind <= AK_MOD);
                bool isIntOnly =
                    (kind == BK_AND || kind == BK_OR || kind == BK_XOR ||
                     kind == SK_SHL || kind == SK_SHR || kind == SK_SAR ||
                     kind == RK_ROL || kind == RK_ROR ||
                     kind == TK_BTST || kind == TK_BSET || kind == TK_BCLR || kind == TK_BTGL);

                if (isIntOnly && !dstIsInt) {
                    error(parser, "This microasm operation requires an integer destination register (i0..i7).");
                    consumeEndOfStatement(parser, "Expect ';' or newline after instruction.");
                    continue;
                }

                // --- src1 ---
                uint8_t aReg = 0; bool aImm = false; double aNum = 0.0;
                if (!micro_parseRegOrNumber(parser, &aReg, &aImm, &aNum)) {
                    consumeEndOfStatement(parser, "Expect ';' or newline after instruction.");
                    continue;
                }
                // --- src2 ---
                uint8_t bReg = 0; bool bImm = false; double bNum = 0.0;
                if (!micro_parseRegOrNumber(parser, &bReg, &bImm, &bNum)) {
                    consumeEndOfStatement(parser, "Expect ';' or newline after instruction.");
                    continue;
                }

                // Bank checks for register operands
                if (!aImm) {
                    if (isArithmetic) {
                        if (((aReg < 8) != dstIsInt)) error(parser, "Register bank mismatch for src1.");
                    } else {
                        if (aReg >= 8) error(parser, "Integer register required for src1.");
                    }
                }
                if (!bImm) {
                    if (isArithmetic) {
                        if (((bReg < 8) != dstIsInt)) error(parser, "Register bank mismatch for src2.");
                    } else {
                        if (bReg >= 8) error(parser, "Integer register required for src2.");
                    }
                }

                // Pick opcode family + variant
                uint16_t opcode = 0;

                if (isArithmetic) {
                    if (dstIsInt) {
                        switch (kind) {
                            case AK_ADD: opcode = (!aImm && !bImm) ? R_IADD_RR :
                                                   (!aImm &&  bImm) ? R_IADD_RI :
                                                   ( aImm && !bImm) ? R_IADD_IR : R_IADD_II; break;
                            case AK_SUB: opcode = (!aImm && !bImm) ? R_ISUB_RR :
                                                   (!aImm &&  bImm) ? R_ISUB_RI :
                                                   ( aImm && !bImm) ? R_ISUB_IR : R_ISUB_II; break;
                            case AK_MUL: opcode = (!aImm && !bImm) ? R_IMUL_RR :
                                                   (!aImm &&  bImm) ? R_IMUL_RI :
                                                   ( aImm && !bImm) ? R_IMUL_IR : R_IMUL_II; break;
                            case AK_DIV: opcode = (!aImm && !bImm) ? R_IDIV_RR :
                                                   (!aImm &&  bImm) ? R_IDIV_RI :
                                                   ( aImm && !bImm) ? R_IDIV_IR : R_IDIV_II; break;
                            case AK_MOD: opcode = (!aImm && !bImm) ? R_IMOD_RR :
                                                   (!aImm &&  bImm) ? R_IMOD_RI :
                                                   ( aImm && !bImm) ? R_IMOD_IR : R_IMOD_II; break;
                            default: break;
                        }
                    } else {
                        switch (kind) {
                            case AK_ADD: opcode = (!aImm && !bImm) ? R_FADD_RR :
                                                   (!aImm &&  bImm) ? R_FADD_RI :
                                                   ( aImm && !bImm) ? R_FADD_IR : R_FADD_II; break;
                            case AK_SUB: opcode = (!aImm && !bImm) ? R_FSUB_RR :
                                                   (!aImm &&  bImm) ? R_FSUB_RI :
                                                   ( aImm && !bImm) ? R_FSUB_IR : R_FSUB_II; break;
                            case AK_MUL: opcode = (!aImm && !bImm) ? R_FMUL_RR :
                                                   (!aImm &&  bImm) ? R_FMUL_RI :
                                                   ( aImm && !bImm) ? R_FMUL_IR : R_FMUL_II; break;
                            case AK_DIV: opcode = (!aImm && !bImm) ? R_FDIV_RR :
                                                   (!aImm &&  bImm) ? R_FDIV_RI :
                                                   ( aImm && !bImm) ? R_FDIV_IR : R_FDIV_II; break;
                            case AK_MOD: opcode = (!aImm && !bImm) ? R_FMOD_RR :
                                                   (!aImm &&  bImm) ? R_FMOD_RI :
                                                   ( aImm && !bImm) ? R_FMOD_IR : R_FMOD_II; break;
                            default: break;
                        }
                    }
                } else {
                    // INT-only groups
                    switch (kind) {
                        case BK_AND: opcode = (!aImm && !bImm) ? R_IAND_RR :
                                              (!aImm &&  bImm) ? R_IAND_RI :
                                              ( aImm && !bImm) ? R_IAND_IR : R_IAND_II; break;
                        case BK_OR:  opcode = (!aImm && !bImm) ? R_IOR_RR  :
                                              (!aImm &&  bImm) ? R_IOR_RI  :
                                              ( aImm && !bImm) ? R_IOR_IR  : R_IOR_II;  break;
                        case BK_XOR: opcode = (!aImm && !bImm) ? R_IXOR_RR :
                                              (!aImm &&  bImm) ? R_IXOR_RI :
                                              ( aImm && !bImm) ? R_IXOR_IR : R_IXOR_II; break;

                        case SK_SHL: opcode = (!aImm && !bImm) ? R_ISHL_RR :
                                              (!aImm &&  bImm) ? R_ISHL_RI :
                                              ( aImm && !bImm) ? R_ISHL_IR : R_ISHL_II; break;
                        case SK_SHR: opcode = (!aImm && !bImm) ? R_ISHR_RR :
                                              (!aImm &&  bImm) ? R_ISHR_RI :
                                              ( aImm && !bImm) ? R_ISHR_IR : R_ISHR_II; break;
                        case SK_SAR: opcode = (!aImm && !bImm) ? R_ISAR_RR :
                                              (!aImm &&  bImm) ? R_ISAR_RI :
                                              ( aImm && !bImm) ? R_ISAR_IR : R_ISAR_II; break;

                        case RK_ROL: opcode = (!aImm && !bImm) ? R_IROL_RR :
                                              (!aImm &&  bImm) ? R_IROL_RI :
                                              ( aImm && !bImm) ? R_IROL_IR : R_IROL_II; break;
                        case RK_ROR: opcode = (!aImm && !bImm) ? R_IROR_RR :
                                              (!aImm &&  bImm) ? R_IROR_RI :
                                              ( aImm && !bImm) ? R_IROR_IR : R_IROR_II; break;

                        case TK_BTST: opcode = (!aImm && !bImm) ? R_IBTST_RR :
                                               (!aImm &&  bImm) ? R_IBTST_RI :
                                               ( aImm && !bImm) ? R_IBTST_IR : R_IBTST_II; break;
                        case TK_BSET: opcode = (!aImm && !bImm) ? R_IBSET_RR :
                                               (!aImm &&  bImm) ? R_IBSET_RI :
                                               ( aImm && !bImm) ? R_IBSET_IR : R_IBSET_II; break;
                        case TK_BCLR: opcode = (!aImm && !bImm) ? R_IBCLR_RR :
                                               (!aImm &&  bImm) ? R_IBCLR_RI :
                                               ( aImm && !bImm) ? R_IBCLR_IR : R_IBCLR_II; break;
                        case TK_BTGL: opcode = (!aImm && !bImm) ? R_IBTGL_RR :
                                               (!aImm &&  bImm) ? R_IBTGL_RI :
                                               ( aImm && !bImm) ? R_IBTGL_IR : R_IBTGL_II; break;
                        default: break;
                    }
                }

                if (opcode == 0) {
                    error(parser, "Internal: unhandled microasm opcode selection.");
                    consumeEndOfStatement(parser, "Expect ';' or newline after instruction.");
                    continue;
                }

                // Pack registers: dst in pos1; for RI use a in pos2; for IR use b in pos3.
                uint8_t aNib = aImm ? 0 : aReg;
                uint8_t bNib = bImm ? 0 : bReg;
                emitOpShort(parser, opcode, packRegs(dst, aNib, bNib, 0));

                // Append immediates in operand order (A then B).
                if (aImm) {
                    if (isArithmetic && !dstIsInt) emitImmF64(parser, (double)aNum);
                    else                            emitImmI64(parser, (int64_t)aNum);
                }
                if (bImm) {
                    if (isArithmetic && !dstIsInt) emitImmF64(parser, (double)bNum);
                    else                            emitImmI64(parser, (int64_t)bNum);
                }

                consumeEndOfStatement(parser, "Expect ';' or newline after instruction.");
                continue;
            }
        }

        // ---- lbind / lbindl ----
        if (tokenEqualsCI(&op, "lbind") || tokenEqualsCI(&op, "lbindl")) {
            bool withLen = tokenEqualsCI(&op, "lbindl");
            uint8_t lenDst = 0xF;

            if (withLen) {
                consume(parser, TOKEN_IDENTIFIER, "Expect integer register for length.");
                if (!parseRegisterToken(&parser->previous, &lenDst) || lenDst >= 8) {
                    error(parser, "Length destination must be i0..i7.");
                }
            }

            consume(parser, TOKEN_IDENTIFIER, "Expect list identifier to bind.");
            Token name = parser->previous;

            // Optional: handle token h0..h7 (default h0)
            uint8_t h = 0;
            (void)tryParseListHandle(parser, &h);

            int local = resolveLocal(parser, parser->currentCompiler, &name);
            if (local != -1) {
                emitOpShort(parser, R_LBIND_LOCAL, packRegs(lenDst, 0, h, 0));
                emitByte(parser, (uint16_t)local);
            } else {
                if (restrictToLocals) {
                    error(parser, "Function microasm can only access locals.");
                } else {
                    int up = resolveUpvalue(parser, parser->currentCompiler, &name);
                    if (up != -1) {
                        emitOpShort(parser, R_LBIND_UPVAL, packRegs(lenDst, 0, h, 0));
                        emitByte(parser, (uint16_t)up);
                    } else {
                        uint16_t nameConst = identifierConstant(parser, &name);
                        emitOpShort(parser, R_LBIND_GLOBAL, packRegs(lenDst, 0, h, 0));
                        emitByte(parser, nameConst);
                    }
                }
            }
            consumeEndOfStatement(parser, "Expect ';' or newline after lbind.");
            continue;
        }

        // ---- llen <iDst> [hN]? ----
        if (tokenEqualsCI(&op, "llen")) {
            consume(parser, TOKEN_IDENTIFIER, "Expect integer destination register.");
            uint8_t dst = 0xFF;
            if (!parseRegisterToken(&parser->previous, &dst) || dst >= 8) {
                error(parser, "Destination must be i0..i7.");
            } else {
                uint8_t h = 0;
                (void)tryParseListHandle(parser, &h);
                emitOpShort(parser, R_LLEN, packRegs(dst, 0, h, 0));
            }
            consumeEndOfStatement(parser, "Expect ';' or newline after llen.");
            continue;
        }


        // ---- lload / lloadu ----
        // Forms:
        //   lload  <dstReg> <iRegIndex> [hN]?
        //   lload  <dstReg> <immIndex>  [hN]?
        //   lloadu <dstReg> <iRegIndex> [hN]?
        //   lloadu <dstReg> <immIndex>  [hN]?
        if (tokenEqualsCI(&op, "lload") || tokenEqualsCI(&op, "lloadu")) {
            bool unsafe = tokenEqualsCI(&op, "lloadu");

            consume(parser, TOKEN_IDENTIFIER, "Expect destination register.");
            uint8_t dst = 0xFF;
            if (!parseRegisterToken(&parser->previous, &dst)) {
                error(parser, "Invalid destination register.");
                consumeEndOfStatement(parser, "Expect ';' or newline after lload.");
                continue;
            }

            if (match(parser, TOKEN_IDENTIFIER)) {
                // Index from i-register
                uint8_t ir = 0xFF;
                if (!parseRegisterToken(&parser->previous, &ir) || ir >= 8) {
                    error(parser, "Index register must be i0..i7.");
                } else {
                    uint8_t h = 0;
                    (void)tryParseListHandle(parser, &h);
                    emitOpShort(parser, unsafe ? R_LLOADU_R : R_LLOAD_R, packRegs(dst, ir, h, 0));
                }
            } else {
                // Immediate integer index
                bool neg = false; if (match(parser, TOKEN_MINUS)) neg = true;
                consume(parser, TOKEN_NUMBER, "Expect integer index.");
                double dv = parseNumberLiteral(parser->previous.start, parser->previous.length);
                if (neg) dv = -dv;
                int64_t idx = (int64_t)dv;
                if ((double)idx != dv) error(parser, "Index must be an integer.");

                uint8_t h = 0;
                (void)tryParseListHandle(parser, &h);

                emitOpShort(parser, unsafe ? R_LLOADU_I : R_LLOAD_I, packRegs(dst, 0, h, 0));
                uint16_t w[4]; memcpy(w, &idx, 8);
                emitByte(parser, w[0]); emitByte(parser, w[1]); emitByte(parser, w[2]); emitByte(parser, w[3]);
            }

            consumeEndOfStatement(parser, "Expect ';' or newline after lload.");
            continue;
        }

        // ---- lstore / lstoreu ----
        // Forms:
        //   lstore  <srcReg> <iRegIndex> [hN]?
        //   lstore  <srcReg> <immIndex>  [hN]?
        //   lstoreu <srcReg> <iRegIndex> [hN]?
        //   lstoreu <srcReg> <immIndex>  [hN]?
        if (tokenEqualsCI(&op, "lstore") || tokenEqualsCI(&op, "lstoreu")) {
            bool unsafe = tokenEqualsCI(&op, "lstoreu");

            consume(parser, TOKEN_IDENTIFIER, "Expect source register.");
            uint8_t src = 0xFF;
            if (!parseRegisterToken(&parser->previous, &src)) {
                error(parser, "Invalid source register.");
                consumeEndOfStatement(parser, "Expect ';' or newline after lstore.");
                continue;
            }

            if (match(parser, TOKEN_IDENTIFIER)) {
                // Index from i-register
                uint8_t ir = 0xFF;
                if (!parseRegisterToken(&parser->previous, &ir) || ir >= 8) {
                    error(parser, "Index register must be i0..i7.");
                } else {
                    uint8_t h = 0;
                    (void)tryParseListHandle(parser, &h);
                    emitOpShort(parser, unsafe ? R_LSTOREU_R : R_LSTORE_R, packRegs(src, ir, h, 0));
                }
            } else {
                // Immediate integer index
                bool neg = false; if (match(parser, TOKEN_MINUS)) neg = true;
                consume(parser, TOKEN_NUMBER, "Expect integer index.");
                double dv = parseNumberLiteral(parser->previous.start, parser->previous.length);
                if (neg) dv = -dv;
                int64_t idx = (int64_t)dv;
                if ((double)idx != dv) error(parser, "Index must be an integer.");

                uint8_t h = 0;
                (void)tryParseListHandle(parser, &h);

                emitOpShort(parser, unsafe ? R_LSTOREU_I : R_LSTORE_I, packRegs(src, 0, h, 0));
                uint16_t w[4]; memcpy(w, &idx, 8);
                emitByte(parser, w[0]); emitByte(parser, w[1]); emitByte(parser, w[2]); emitByte(parser, w[3]);
            }

            consumeEndOfStatement(parser, "Expect ';' or newline after lstore.");
            continue;
        }

        // ---- lloadp / lloadup ----
        if (tokenEqualsCI(&op, "lloadp") || tokenEqualsCI(&op, "lloadup")) {
            bool unsafe = tokenEqualsCI(&op, "lloadup");

            consume(parser, TOKEN_IDENTIFIER, "Expect destination register.");
            uint8_t dst = 0xFF;
            if (!parseRegisterToken(&parser->previous, &dst)) {
                error(parser, "Invalid destination register.");
                consumeEndOfStatement(parser, "Expect ';' or newline after lloadp.");
                continue;
            }

            consume(parser, TOKEN_IDENTIFIER, "Expect index i-register.");
            uint8_t ir = 0xFF;
            if (!parseRegisterToken(&parser->previous, &ir) || ir >= 8) {
                error(parser, "Index register must be i0..i7.");
            } else {
                uint8_t h = 0; (void)tryParseListHandle(parser, &h);
                emitOpShort(parser, unsafe ? R_LLOADU_PI : R_LLOAD_PI, packRegs(dst, ir, h, 0));
            }
            consumeEndOfStatement(parser, "Expect ';' or newline after lloadp.");
            continue;
        }

        // ---- lstorep / lstoreup ----
        if (tokenEqualsCI(&op, "lstorep") || tokenEqualsCI(&op, "lstoreup")) {
            bool unsafe = tokenEqualsCI(&op, "lstoreup");

            consume(parser, TOKEN_IDENTIFIER, "Expect source register.");
            uint8_t src = 0xFF;
            if (!parseRegisterToken(&parser->previous, &src)) {
                error(parser, "Invalid source register.");
                consumeEndOfStatement(parser, "Expect ';' or newline after lstorep.");
                continue;
            }

            consume(parser, TOKEN_IDENTIFIER, "Expect index i-register.");
            uint8_t ir = 0xFF;
            if (!parseRegisterToken(&parser->previous, &ir) || ir >= 8) {
                error(parser, "Index register must be i0..i7.");
            } else {
                uint8_t h = 0; (void)tryParseListHandle(parser, &h);
                emitOpShort(parser, unsafe ? R_LSTOREU_PI : R_LSTORE_PI, packRegs(src, ir, h, 0));
            }
            consumeEndOfStatement(parser, "Expect ';' or newline after lstorep.");
            continue;
        }

        // Unknown mnemonic
        error(parser, "Unknown microasm instruction.");
        consumeEndOfStatement(parser, "Expect ';' or newline after instruction.");
    }

    // ---- Resolve forward jumps to block-local labels (".name") ----
    {
        MicroLocalFix *p = localFixups;
        while (p != NULL) {
            MicroLocalFix *next = p->next;
            MicroLabel *lbl = NULL;
            FIND_LOCAL_LABEL(&p->name, lbl);
            if (!lbl) {
                parser->current = p->name; // point to offending token
                error(parser, "Undefined block-local label.");
            } else {
                currentChunk(parser)->code[p->patchIndex] =
                    (uint16_t)(lbl->offset - p->patchIndex - 1);
            }
            free(p);
            p = next;
        }
        localFixups = NULL;
    }

    // ---- Merge block maxima into function's requirements ----
    if (blockMaxI > parser->currentCompiler->function->micro_i_slots)
        parser->currentCompiler->function->micro_i_slots = blockMaxI;
    if (blockMaxF > parser->currentCompiler->function->micro_f_slots)
        parser->currentCompiler->function->micro_f_slots = blockMaxF;

    consume(parser, TOKEN_RIGHT_BRACE, "Expect '}' after microasm block.");

    #undef FIND_LOCAL_LABEL
}

static void super_(Parser *parser, bool canAssign)
{
    (void) canAssign;

    if (parser->currentClass == NULL) {
        error(parser, "Can't use 'super' outside of a class.");
    } else if (!parser->currentClass->hasSuperclass) {
        error(parser, "Can't use 'super' in a class with no superclass.");
    }

    consume(parser, TOKEN_DOT, "Expect '.' after 'super'.");
    consume(parser, TOKEN_IDENTIFIER, "Expect superclass method name.");
    uint16_t name = identifierConstant(parser, &parser->previous);

    namedVariable(parser, syntheticToken("this"), false);
    if (match(parser, TOKEN_LEFT_PAREN)) {
        uint8_t argCount = argumentList(parser);
        namedVariable(parser, syntheticToken("super"), false);
        emitOpShort(parser, INVOKE_SUPER, name);
        emitByte(parser, argCount);
    } else {
        namedVariable(parser, syntheticToken("super"), false);
        emitOpShort(parser, LD_SUPER, name);
    }
}

static void this_(Parser *parser, bool canAssign)
{
    (void) canAssign;

    if (parser->currentClass == NULL)
    {
        error(parser, "Can't use 'this' outside of a class.");
        return;
    }

    variable(parser, false);
}

static void unary(Parser *parser, bool canAssign)
{
    (void) canAssign;

    TokenType operatorType = parser->previous.type;

    // Compile the operand.
    parsePrecedence(parser, PREC_UNARY, false);

    // Emit the operator instruction.
    switch (operatorType)
    {
        case TOKEN_BANG: emitByte(parser, NOT); break;
        case TOKEN_MINUS: emitByte(parser, NEG); break;
        case TOKEN_TILDE: emitByte(parser, BNOT); break;
        default: return; // Unreachable.
    }
}

// clang-format off
ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]      = { grouping, call,   PREC_CALL },
  [TOKEN_RIGHT_PAREN]     = { NULL,     NULL,   PREC_NONE },
  [TOKEN_LEFT_BRACE]      = { map,      NULL,   PREC_NONE },
  [TOKEN_RIGHT_BRACE]     = { NULL,     NULL,   PREC_NONE },
  [TOKEN_LEFT_SQUARE]     = { list,     index_, PREC_CALL },
  [TOKEN_RIGHT_SQUARE]    = { NULL,     NULL,   PREC_NONE },
  [TOKEN_QUESTION]        = { NULL,     ternary,PREC_ASSIGNMENT },
  [TOKEN_COLON]           = { NULL,     NULL,   PREC_NONE },
  [TOKEN_COMMA]           = { NULL,     NULL,   PREC_NONE },
  [TOKEN_DOT]             = { NULL,     dot,    PREC_CALL },
  [TOKEN_MINUS]           = { unary,    binary, PREC_TERM },
  [TOKEN_MINUS_MINUS]     = { prefixIncDec,   NULL,   PREC_UNARY },
  [TOKEN_PERCENT]         = { NULL,     binary, PREC_FACTOR },
  [TOKEN_PLUS]            = { NULL,     binary, PREC_TERM },
  [TOKEN_PLUS_PLUS]       = { prefixIncDec,   NULL,   PREC_UNARY },
  [TOKEN_SEMICOLON]       = { NULL,     NULL,   PREC_NONE },
  [TOKEN_SLASH]           = { NULL,     binary, PREC_FACTOR },
  [TOKEN_STAR]            = { NULL,     binary, PREC_FACTOR },
  [TOKEN_BANG]            = { unary,    NULL,   PREC_NONE },
  [TOKEN_BANG_EQUAL]      = { NULL,     binary, PREC_EQUALITY },
  [TOKEN_EQUAL]           = { NULL,     NULL,   PREC_NONE },
  [TOKEN_EQUAL_EQUAL]     = { NULL,     binary, PREC_EQUALITY },
  [TOKEN_GREATER]         = { NULL,     binary, PREC_COMPARISON },
  [TOKEN_GREATER_EQUAL]   = { NULL,     binary, PREC_COMPARISON },
  [TOKEN_LESS]            = { NULL,     binary, PREC_COMPARISON },
  [TOKEN_LESS_EQUAL]      = { NULL,     binary, PREC_COMPARISON },
  [TOKEN_IDENTIFIER]      = { variable, NULL,   PREC_NONE },
  [TOKEN_STRING]          = { string,   NULL,   PREC_NONE },
  [TOKEN_NUMBER]          = { number,   NULL,   PREC_NONE },
  [TOKEN_AND]             = { NULL,     and_,   PREC_AND },
  [TOKEN_CLASS]           = { NULL,     NULL,   PREC_NONE },
  [TOKEN_ELSE]            = { NULL,     NULL,   PREC_NONE },
  [TOKEN_FALSE]           = { literal,  NULL,   PREC_NONE },
  [TOKEN_FOR]             = { NULL,     NULL,   PREC_NONE },
  [TOKEN_FUNC]            = { lambda,   NULL,   PREC_NONE },
  [TOKEN_IF]              = { NULL,     NULL,   PREC_NONE },
  [TOKEN_NULL]            = { literal,  NULL,   PREC_NONE },
  [TOKEN_OR]              = { NULL,     or_,    PREC_OR },
  [TOKEN_PRINT]           = { NULL,     NULL,   PREC_NONE },
  [TOKEN_RETURN]          = { NULL,     NULL,   PREC_NONE },
  [TOKEN_SUPER]           = { super_,   NULL,   PREC_NONE },
  [TOKEN_THIS]            = { this_,    NULL,   PREC_NONE },
  [TOKEN_TRUE]            = { literal,  NULL,   PREC_NONE },
  [TOKEN_VAR]             = { NULL,     NULL,   PREC_NONE },
  [TOKEN_WHILE]           = { NULL,     NULL,   PREC_NONE },
  [TOKEN_ERROR]           = { NULL,     NULL,   PREC_NONE },
  [TOKEN_EOF]             = { NULL,     NULL,   PREC_NONE },
  [TOKEN_AMPERSAND]       = { NULL,     binary, PREC_BIT_AND },
  [TOKEN_PIPE]            = { NULL,     binary, PREC_BIT_OR },
  [TOKEN_CARET]           = { NULL,     binary, PREC_BIT_XOR },
  [TOKEN_LESS_LESS]       = { NULL,     binary, PREC_BIT_SHIFT },
  [TOKEN_GREATER_GREATER] = { NULL,     binary, PREC_BIT_SHIFT },
  [TOKEN_TILDE]           = { unary,    NULL,   PREC_UNARY },
};
// clang-format on

static bool parsePrecedence(Parser *parser, Precedence precedence, bool stopForSoleConst)
{
    advance(parser);
    ParseFn prefixRule = getRule(parser->previous.type)->prefix;
    if (prefixRule == NULL)
    {
        error(parser, "Expect expression.");
        return false;
    }

    bool soleConst = false;
    bool canAssign = precedence <= PREC_ASSIGNMENT;
    if (!stopForSoleConst || precedence <= getRule(parser->current.type)->precedence) {
        prefixRule(parser, canAssign);
    } else {
        soleConst = true;
    }

    while (precedence <= getRule(parser->current.type)->precedence)
    {
        advance(parser);
        ParseFn infixRule = getRule(parser->previous.type)->infix;
        infixRule(parser, canAssign);
    }

    if (canAssign && match(parser, TOKEN_EQUAL))
    {
        error(parser, "Invalid assignment target.");
    }

    return !soleConst;
}

static ParseRule *getRule(TokenType type)
{
    return &rules[type];
}

static void expression(Parser *parser)
{
    parsePrecedence(parser, PREC_ASSIGNMENT, false);
}

static void block(Parser *parser)
{
    while (!check(parser, TOKEN_RIGHT_BRACE) && !check(parser, TOKEN_EOF))
    {
        skipNewlines(parser);
        if (check(parser, TOKEN_RIGHT_BRACE) || check(parser, TOKEN_EOF)) break;
        declaration(parser);
    }

    consume(parser, TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(Parser *parser, FunctionType type, const char *name, int nameLength)
{
    Compiler compiler;
    initCompiler(parser, &compiler, type, name, nameLength);
    beginScope(parser);

    consume(parser, TOKEN_LEFT_PAREN, nameLength == 2 && !memcmp(name, "()", 2) ? "Expect '(' after 'fun'." : "Expect '(' after function name.");
    do
    {
        if (check(parser, TOKEN_RIGHT_PAREN)) break;
        parser->currentCompiler->function->arity++;
        if (parser->currentCompiler->function->arity > 255) errorAtCurrent(parser, "Can't have more than 255 parameters.");
        uint16_t constant = parseVariable(parser, "Expect parameter name.");
        defineVariable(parser, constant);
    } while (match(parser, TOKEN_COMMA));

    consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");

    // --- NEW: allow optional 'microasm' keyword before '{'
    skipNewlines(parser);
    bool isMicroasm = false;

    // Prefer token form if scanner was patched:
    if (match(parser, TOKEN_MICROASM)) {
        isMicroasm = true;
    } else {
        // Fallback: accept identifier "microasm" case-insensitively
        if (check(parser, TOKEN_IDENTIFIER)) {
            Token t = parser->current;
            // tokenEqualsCI() is the helper we added; if you didn't add it yet,
            // temporarily compare case-sensitively to "microasm".
            if (tokenEqualsCI(&t, "microasm")) {
                advance(parser);
                isMicroasm = true;
            }
        }
    }

    skipNewlines(parser);
    consume(parser, TOKEN_LEFT_BRACE, "Expect '{' before function body.");

    if (isMicroasm) {
        microasmBlock(parser, /*restrictToLocals=*/true);   // function microasm: locals only
    } else {
        block(parser);
    }

    ObjFunction *function = endCompiler(parser);
    pushTemp(parser->gc, OBJ_VAL(function));
    emitOpShort(parser, function->upvalueCount ? MAKE_CLOSURE : PUSH_CONST,
                makeConstant(parser, OBJ_VAL(function)));
    popTemp(parser->gc);

    for (int i = 0; i < function->upvalueCount; i++) {
        emitByte(parser, compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(parser, compiler.upvalues[i].index);
    }
}

static void method(Parser *parser)
{
    consume(parser, TOKEN_IDENTIFIER, "Expect method name.");
    uint16_t constant = identifierConstant(parser, &parser->previous);

    FunctionType type = TYPE_METHOD;
    if (parser->previous.length == 4 && memcmp(parser->previous.start, "init", 4) == 0) type = TYPE_INITIALIZER;
    function(parser, type, parser->previous.start, parser->previous.length);
    emitOpShort(parser, DEF_METHOD, constant);
}

static void classDeclaration(Parser *parser)
{
    consume(parser, TOKEN_IDENTIFIER, "Expect class name.");
    Token className = parser->previous;
    uint16_t nameConstant = identifierConstant(parser, &parser->previous);
    declareVariable(parser);

    emitOpShort(parser, MAKE_CLASS, nameConstant);
    defineVariable(parser, nameConstant);

    ClassCompiler classCompiler;
    classCompiler.hasSuperclass = false;
    classCompiler.enclosing = parser->currentClass;
    parser->currentClass = &classCompiler;

    if (match(parser, TOKEN_LESS))
    {
        consume(parser, TOKEN_IDENTIFIER, "Expect superclass name.");
        variable(parser, false);

        if (identifiersEqual(&className, &parser->previous)) error(parser, "A class can't inherit from itself.");

        beginScope(parser);
        addLocal(parser, syntheticToken("super"));
        defineVariable(parser, 0);

        namedVariable(parser, className, false);
        emitByte(parser, CLASS_INHERIT);
        classCompiler.hasSuperclass = true;
    }

    namedVariable(parser, className, false);
    skipNewlines(parser);
    consume(parser, TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    while (!check(parser, TOKEN_RIGHT_BRACE) && !check(parser, TOKEN_EOF))
    {
        skipNewlines(parser);
        if (check(parser, TOKEN_RIGHT_BRACE) || check(parser, TOKEN_EOF)) break;
        method(parser);
    }
    consume(parser, TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    emitByte(parser, POP);

    if (classCompiler.hasSuperclass) endScope(parser);

    parser->currentClass = parser->currentClass->enclosing;
}

static void funDeclaration(Parser *parser)
{
    uint16_t global = parseVariable(parser, "Expect function name.");
    markInitialized(parser);
    function(parser, TYPE_FUNCTION, parser->previous.start, parser->previous.length);
    defineVariable(parser, global);
}

static void varDeclaration(Parser *parser)
{
    uint16_t global = parseVariable(parser, "Expect variable name.");

    if (match(parser, TOKEN_EQUAL)) {
        expression(parser);
    } else {
        emitByte(parser, PUSH_NULL);
    }
    consumeEndOfStatement(parser, "Expect ';' or newline after variable declaration.");

    defineVariable(parser, global);
}

static void expressionStatement(Parser *parser)
{
    expression(parser);
    consumeEndOfStatement(parser, "Expect ';' or newline after expression.");
    emitByte(parser, POP);
}

static void skipClauseEnds(Parser *parser) {
    while (match(parser, TOKEN_SEMICOLON) || match(parser, TOKEN_NEWLINE));
}

static bool matchClauseEnd(Parser *parser) {
    return match(parser, TOKEN_SEMICOLON) || match(parser, TOKEN_NEWLINE);
}

static void forStatement(Parser *parser)
{
    beginScope(parser);
    consume(parser, TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    skipNewlines(parser); // allow blank lines after '('

    // --- Initializer ---
    if (matchClauseEnd(parser)) {
        // No initializer.
    } else if (match(parser, TOKEN_VAR)) {
        varDeclaration(parser);
        skipClauseEnds(parser); // allow trailing ;/newline after var decl
    } else {
        expressionStatement(parser);
        skipClauseEnds(parser); // allow trailing ;/newline after expr
    }

    Loop loop;
    beginLoop(parser, &loop);
    if (parser->currentLabel != NULL) {
        loop.label = parser->currentLabel;
    }
    int conditionStart = currentChunk(parser)->count;
    loop.continueTarget = conditionStart;

    // --- Condition ---
    if (!check(parser, TOKEN_RIGHT_PAREN)) {
        expression(parser);
        skipClauseEnds(parser); // allow ;/newline after condition
        // Jump out of the loop if the condition is false.
        loop.exitJump = emitJump(parser, PJMP_IF_FALSE);
    } else {
        loop.exitJump = -1;
    }

    // --- Increment ---
    if (!check(parser, TOKEN_RIGHT_PAREN)) {
        int bodyJump = emitJump(parser, JMP);
        int incrementStart = currentChunk(parser)->count;
        loop.continueTarget = incrementStart;
        expression(parser);
        emitByte(parser, POP);
        skipClauseEnds(parser); // allow ;/newline after increment
        consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");
        emitLoop(parser, loop.start);
        loop.start = incrementStart;
        patchJump(parser, bodyJump);
    } else {
        consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");
    }

    skipNewlines(parser);
    statement(parser);
    emitLoop(parser, loop.start);

    if (loop.exitJump != -1) patchJump(parser, loop.exitJump);

    endLoop(parser);
    endScope(parser);
}

static void ifStatement(Parser *parser)
{
    consume(parser, TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    skipNewlines(parser);

    expression(parser);

    skipNewlines(parser);
    consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    skipNewlines(parser);

    int thenJump = emitJump(parser, PJMP_IF_FALSE);
    statement(parser);

    if (match(parser, TOKEN_ELSE)) {
        skipNewlines(parser);

        int elseJump = emitJump(parser, JMP);
        patchJump(parser, thenJump);
        statement(parser);
        patchJump(parser, elseJump);
    } else {
        patchJump(parser, thenJump);
    }
}

static void printStatement(Parser *parser)
{
    expression(parser);
    consumeEndOfStatement(parser, "Expect ';' or newline after value.");
    emitByte(parser, DBG_PRINT);
}

static void returnStatement(Parser *parser)
{
    if (parser->currentCompiler->type == TYPE_SCRIPT) error(parser, "Can't return from top-level code.");

    if (match(parser, TOKEN_SEMICOLON)) {
        emitReturn(parser);
    } else {
        if (parser->currentCompiler->type == TYPE_INITIALIZER) error(parser, "Can't return a value from an initializer.");

        expression(parser);
        consumeEndOfStatement(parser, "Expect ';' or newline after return value.");
        emitByte(parser, RET);
    }
}

static void whileStatement(Parser *parser)
{
    Loop loop;
    beginLoop(parser, &loop);

    // Associate label with loop if present
    if (parser->currentLabel != NULL) {
        loop.label = parser->currentLabel;
    }

    // For while loops, continue jumps to the condition check
    loop.continueTarget = loop.start;

    consume(parser, TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    skipNewlines(parser);

    expression(parser);

    skipNewlines(parser);
    consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    skipNewlines(parser);

    loop.exitJump = emitJump(parser, PJMP_IF_FALSE);
    statement(parser);
    emitLoop(parser, loop.start);

    patchJump(parser, loop.exitJump);

    endLoop(parser);
}


static void doWhileStatement(Parser *parser)
{
    // Create a new loop context
    Loop loop;
    beginLoop(parser, &loop);

    if (parser->currentLabel != NULL) {
        loop.label = parser->currentLabel;
    }

    skipNewlines(parser);
    statement(parser);

    int conditionStart = currentChunk(parser)->count;
    loop.continueTarget = conditionStart;

    ContinueJump* jump = loop.continueJumps;
    while (jump != NULL) {
        patchJump(parser, jump->offset);
        ContinueJump* next = jump->next;
        free(jump);
        jump = next;
    }
    loop.continueJumps = NULL;

    skipNewlines(parser);

    consume(parser, TOKEN_WHILE, "Expect 'while' after do block.");
    skipNewlines(parser);

    consume(parser, TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    skipNewlines(parser);

    expression(parser);
    skipNewlines(parser);

    consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");
    skipNewlines(parser);

    consumeEndOfStatement(parser, "Expect ';' or newline after do-while condition.");

    loop.exitJump = emitJump(parser, PJMP_IF_FALSE);
    emitLoop(parser, loop.start);
    patchJump(parser, loop.exitJump);
    endLoop(parser);
}


static void synchronize(Parser *parser)
{
    parser->panicMode = false;

    while (parser->current.type != TOKEN_EOF)
    {
        if (parser->previous.type == TOKEN_SEMICOLON) return;
        switch (parser->current.type)
        {
            case TOKEN_CLASS:
            case TOKEN_DO:
            case TOKEN_FUNC:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN: return;

            default: ; // Do nothing.
        }

        advance(parser);
    }
}

static void declaration(Parser *parser)
{
    if (match(parser, TOKEN_CLASS)) {
        classDeclaration(parser);
    } else if (match(parser, TOKEN_FUNC)) {
        funDeclaration(parser);
    } else if (match(parser, TOKEN_VAR)) {
        varDeclaration(parser);
    } else {
        statement(parser);
    }

    if (parser->panicMode) synchronize(parser);
}

// Helper function to find a loop by label
static Loop* findLoopByLabel(Parser *parser, Token labelToken) {
    Loop* loop = parser->currentCompiler->currentLoop;
    while (loop != NULL) {
        if (loop->label != NULL && 
            loop->label->length == labelToken.length &&
            memcmp(loop->label->start, labelToken.start, labelToken.length) == 0) {
            return loop;
        }
        loop = loop->enclosing;
    }
    return NULL; // No matching loop found
}

static void breakStatement(Parser *parser)
{
    if (parser->currentCompiler->currentLoop == NULL) {
        error(parser, "Cannot use 'break' outside of a loop.");
        return;
    }
    
    Loop* targetLoop = parser->currentCompiler->currentLoop; // Default to innermost loop
    
    // Check if there's a label after 'break'
    if (check(parser, TOKEN_IDENTIFIER)) {
        advance(parser); // Consume the identifier
        Token labelToken = parser->previous;
        
        // Find the loop with this label
        targetLoop = findLoopByLabel(parser, labelToken);
        
        if (targetLoop == NULL) {
            error(parser, "No loop with that label found.");
            return;
        }
    }
    
    consumeEndOfStatement(parser, "Expect ';' or newline after 'break'.");
    
    // Discard any locals created inside the loop
    // For labeled breaks, we need to pop locals up to the target loop's scope depth
    for (int i = parser->currentCompiler->localCount - 1; 
         i >= 0 && parser->currentCompiler->locals[i].depth > targetLoop->scopeDepth; 
         i--) {
        emitByte(parser, POP);
    }
    
    // Emit a jump to the end of the loop
    int breakJump = emitJump(parser, JMP);
    
    // Add this jump to the target loop's break jumps list
    BreakJump* jump = (BreakJump*)malloc(sizeof(BreakJump));
    jump->offset = breakJump;
    jump->next = targetLoop->breakJumps;
    targetLoop->breakJumps = jump;
}

static void continueStatement(Parser *parser)
{
    if (parser->currentCompiler->currentLoop == NULL) {
        error(parser, "Cannot use 'continue' outside of a loop.");
        return;
    }
    
    Loop* targetLoop = parser->currentCompiler->currentLoop; // Default to innermost loop
    
    // Check if there's a label after 'continue'
    if (check(parser, TOKEN_IDENTIFIER)) {
        advance(parser); // Consume the identifier
        Token labelToken = parser->previous;
        
        // Store the current loop before looking for the labeled loop
        Loop* originalLoop = targetLoop;
        
        // Find the loop with this label
        targetLoop = findLoopByLabel(parser, labelToken);
        
        if (targetLoop == NULL) {
            error(parser, "No loop with that label found.");
            return;
        }
    }
    
    consumeEndOfStatement(parser, "Expect ';' or newline after 'continue'.");
    
    // Discard any locals created inside the loop
    // For labeled continues, we need to pop locals up to the target loop's scope depth
    for (int i = parser->currentCompiler->localCount - 1; 
         i >= 0 && parser->currentCompiler->locals[i].depth > targetLoop->scopeDepth; 
         i--) {
        emitByte(parser, POP);
    }
    
    // CRITICAL: For do-while loops, always jump to the condition check (continueTarget)
    // This ensures that labeled continue statements correctly jump to the condition
    // of the target loop, not to the start of the loop body
    if (targetLoop->continueTarget != -1) {
        emitLoop(parser, targetLoop->continueTarget);
    } else {
        // For do-while loops where continueTarget is not yet known,
        // emit a jump and record it to be patched later
        int continueJump = emitJump(parser, JMP);
        
        // Add this jump to the target loop's continue jumps list
        ContinueJump* jump = (ContinueJump*)malloc(sizeof(ContinueJump));
        jump->offset = continueJump;
        jump->next = targetLoop->continueJumps;
        targetLoop->continueJumps = jump;
    }
}

static bool isLabel(Parser *parser) {
    if (parser->current.type != TOKEN_IDENTIFIER) return false;
    Scanner look = parser->scanner;           // copy-by-value
    Token next = scanToken(&look);            // peek
    return next.type == TOKEN_COLON;
}

static void declareLabel(Parser *parser, Token name) {
    // Check for duplicate label in the current scope
    Label *label = parser->currentCompiler->labels;
    while (label != NULL) {
        if (label->scopeDepth == parser->currentCompiler->scopeDepth &&
            label->name.length == name.length &&
            memcmp(label->name.start, name.start, name.length) == 0) {
            error(parser, "Label already defined in this scope.");
            return;
        }
        label = label->next;
    }
    
    // Create a new label
    label = (Label*)malloc(sizeof(Label));
    label->name = name;
    label->offset = currentChunk(parser)->count;
    label->scopeDepth = parser->currentCompiler->scopeDepth;
    label->next = parser->currentCompiler->labels;
    parser->currentCompiler->labels = label;
}

static void labelStatement(Parser *parser) {
    // Consume the label name
    Token labelName = parser->current;
    advance(parser);
    
    // Consume the colon
    consume(parser, TOKEN_COLON, "Expect ':' after label name.");
    
    // Declare the label
    declareLabel(parser, labelName);
    
    // Store the label for potential loop association
    // Allocate memory for the label token
    Token *labelToken = (Token*)malloc(sizeof(Token));
    *labelToken = labelName;
    parser->currentLabel = labelToken;

    // Skip new lines following the label if any
    skipNewlines(parser);
    
    // Parse the statement that follows the label
    statement(parser);
    
    // Reset the current label after the statement is parsed
    // This ensures the label is only associated with the immediately following statement
    parser->currentLabel = NULL;
}

static void jmpStatement(Parser *parser) {
    // The jmp keyword has already been consumed by the match function
    // No need to consume it again
    
    // Consume the label name
    consume(parser, TOKEN_IDENTIFIER, "Expect label name after 'jmp'.");
    Token labelName = parser->previous;
    
    // Consume the semicolon
    consumeEndOfStatement(parser, "Expect ';' or newline after jmp statement.");
    
    // Check if the label has already been defined
    Label *label = parser->currentCompiler->labels;
    bool found = false;
    
    while (label != NULL) {
        // Check if the label name matches and is in scope
        if (label->name.length == labelName.length &&
            memcmp(label->name.start, labelName.start, labelName.length) == 0 &&
            label->scopeDepth <= parser->currentCompiler->scopeDepth) {
            found = true;
            break;
        }
        label = label->next;
    }
    
    if (found) {
        // If the label is already defined, emit a backward jump
        // Calculate the offset for the backward jump
        int offset = currentChunk(parser)->count - label->offset + 2; // +2 for the JMP_BACK and offset bytes
        
        // Emit the backward jump
        emitByte(parser, JMP_BACK);
        emitByte(parser, (uint16_t)offset);
    } else {
        // If the label is not yet defined, emit a forward jump with a placeholder
        int jumpOffset = emitJump(parser, JMP);
        
        // Record the jmp statement for later resolution
        JmpJump *jmpJump = (JmpJump*)malloc(sizeof(JmpJump));
        jmpJump->name = labelName;
        jmpJump->offset = jumpOffset;
        jmpJump->scopeDepth = parser->currentCompiler->scopeDepth;
        jmpJump->next = parser->currentCompiler->gotos;
        parser->currentCompiler->gotos = jmpJump;
    }
}

static void gotoStatement(Parser *parser)
{
    consume(parser, TOKEN_IDENTIFIER, "Expect label name after 'goto'.");
    Token labelName = parser->previous;

    consumeEndOfStatement(parser, "Expect ';' or newline after goto.");

    int currDepth = parser->currentCompiler->scopeDepth;
    Chunk *chunk  = currentChunk(parser);

    // Find a same-name label that's not in deeper scope.
    Label *found = NULL;
    for (Label *it = parser->currentCompiler->labels; it; it = it->next) {
        if (it->name.length == labelName.length &&
            memcmp(it->name.start, labelName.start, labelName.length) == 0 &&
            it->scopeDepth <= currDepth) {
            found = it;
            break;
            }
    }

    if (found && found->offset <= chunk->count) {
        // Backward goto — label already defined
        // Guard: only clean up if jumping to a *shallower* scope.
        if (found->scopeDepth < currDepth) {
            emitCleanupToDepth(parser, found->scopeDepth);
        } else {
            // Same depth: do not emit cleanup (avoids popping locals).
        }

        int back = chunk->count - found->offset + 2; // PC after [op][arg]
        emitByte(parser, JMP_BACK);
        emitByte(parser, (uint16_t)back);
    } else {
        // Forward goto — record for resolution (will use trampoline)
        int placeholder = emitJump(parser, JMP);
        recordSafeJump(parser, labelName, placeholder);
    }
}

static void statement(Parser *parser)
{
    if (isLabel(parser)) {
        labelStatement(parser);
    } else if (match(parser, TOKEN_PRINT)) {
        printStatement(parser);
    } else if (match(parser, TOKEN_FOR)) {
        forStatement(parser);
    } else if (match(parser, TOKEN_IF)) {
        ifStatement(parser);
    } else if (match(parser, TOKEN_RETURN)) {
        returnStatement(parser);
    } else if (match(parser, TOKEN_WHILE)) {
        whileStatement(parser);
    } else if (match(parser, TOKEN_DO)) {
        doWhileStatement(parser);
    } else if (match(parser, TOKEN_BREAK)) {
        breakStatement(parser);
    } else if (match(parser, TOKEN_CONTINUE)) {
        continueStatement(parser);
    } else if (match(parser, TOKEN_JMP)) {
        jmpStatement(parser);
    } else if (match(parser, TOKEN_GOTO)) {
        gotoStatement(parser);
    } else if (match(parser, TOKEN_LEFT_BRACE)) {
        beginScope(parser);
        block(parser);
        endScope(parser);
    } else if (match(parser, TOKEN_MICROASM)) {
        skipNewlines(parser);
        consume(parser, TOKEN_LEFT_BRACE, "Expect '{' after microasm.");
        microasmBlock(parser, /*restrictToLocals=*/false);  // general microasm: allow upvalues & globals
    } else {
        expressionStatement(parser);
    }
}

static void compilerMarkRoots(struct GC *gc, void *arg)
{
    Parser *parser = (Parser *) arg;
    Compiler *compiler = parser->currentCompiler;
    while (compiler != NULL)
    {
        markObject(gc, (Obj *) compiler->function);
        compiler = compiler->enclosing;
    }

    if (parser->prevMarkRoots) parser->prevMarkRoots(gc, parser->prevMarkRootsArg);
}

static void compilerFixWeak(void *arg)
{
    Parser *parser = (Parser *) arg;
    tableRemoveWhite(parser->strings);

    if (parser->prevFixWeak) parser->prevFixWeak(parser->prevFixWeakArg);
}

static void setupGC(Parser *parser, GC *gc, Table *strings)
{
    parser->gc = gc;
    parser->prevMarkRoots = gc->markRoots;
    parser->prevMarkRootsArg = gc->markRootsArg;
    parser->prevFixWeak = gc->fixWeak;
    parser->prevFixWeakArg = gc->fixWeakArg;

    gc->markRoots = compilerMarkRoots;
    gc->markRootsArg = parser;
    if (gc->fixWeak != (void (*)(void *)) tableRemoveWhite || gc->fixWeakArg != strings)
    {
        gc->fixWeak = compilerFixWeak;
        gc->fixWeakArg = parser;
    }
}

static void restoreGC(Parser *parser)
{
    GC *gc = parser->gc;
    gc->markRoots = parser->prevMarkRoots;
    gc->markRootsArg = parser->prevMarkRootsArg;
    gc->fixWeak = parser->prevFixWeak;
    gc->fixWeakArg = parser->prevFixWeakArg;
}

ObjFunction *compile(const char *source, GC *gc, Table *strings)
{
    Parser parser;
    setupGC(&parser, gc, strings);
    parser.strings = strings;
    parser.currentCompiler = NULL;
    parser.currentClass = NULL;
    parser.hadError = false;
    parser.panicMode = false;
    parser.currentLabel = NULL; // Initialize currentLabel to NULL

    initScanner(&parser.scanner, source);

    initTable(&parser.fastNatives, 0.75);
    int nativeCount = getFastNativeCount();
    for (int i = 0; i < nativeCount; i++) {
        ObjString* name = copyString(parser.gc, parser.strings, G_FAST_NATIVES[i].name, strlen(G_FAST_NATIVES[i].name));
        pushTemp(parser.gc, OBJ_VAL(name));
        tableSet(parser.gc, &parser.fastNatives, name, NUMBER_VAL(i));
        popTemp(parser.gc);
    }

    Compiler compiler;
    initCompiler(&parser, &compiler, TYPE_SCRIPT, NULL, 0);
    advance(&parser);

    for (;;) {
        // Eat any trailing trivia (newlines, comments already skipped by the scanner)
        skipNewlines(&parser);

        // If we’re at EOF now, stop before attempting to parse a declaration/expression.
        if (check(&parser, TOKEN_EOF)) break;

        declaration(&parser);
    }

    ObjFunction *function = endCompiler(&parser);
    restoreGC(&parser);
    return parser.hadError ? NULL : function;
}
