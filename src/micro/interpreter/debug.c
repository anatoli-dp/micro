#include "./debug.h"

#include <stdio.h>
#include <string.h>

#include "./object.h"
#include "./value.h"
#include "./native_registry.h"

// ---- microasm helpers ----
#define REG_POS1(x)   (((x) >> 12) & 0xF)
#define REG_POS2(x)   (((x) >> 8)  & 0xF)
#define REG_POS3(x)   (((x) >> 4)  & 0xF)
#define REG_POS4(x)   ((x) & 0xF)

#ifndef MS_MAX_LBIND
#define MS_MAX_LBIND 16
#endif

static inline long long read_i64_payload(const uint16_t* w) {
    long long v; memcpy(&v, w, 8); return v;
}
static inline double read_f64_payload(const uint16_t* w) {
    double v; memcpy(&v, w, 8); return v;
}

static void printHandle(uint8_t h) {
    if (h >= MS_MAX_LBIND) printf("h%u(!)", (unsigned)h);
    else                   printf("h%u", (unsigned)h);
}


static void printReg(uint8_t r) {
    if (r < 8) printf("i%u", r);
    else       printf("f%u", r - 8);
}

static int reg1Instruction(const char *name, Chunk *chunk, int offset) {
    uint16_t regs = chunk->code[offset + 1];
    uint8_t r = REG_POS1(regs);
    printf("%-16s ", name);
    printReg(r);
    printf("\n");
    return offset + 2;
}

static int reg2Instruction(const char *name, Chunk *chunk, int offset) {
    uint16_t regs = chunk->code[offset + 1];
    uint8_t dst = REG_POS1(regs);
    uint8_t src = REG_POS2(regs);
    printf("%-16s ", name);
    printReg(dst);
    printf(", ");
    printReg(src);
    printf("\n");
    return offset + 2;
}

static int reg3Instruction(const char *name, Chunk *chunk, int offset) {
    uint16_t regs = chunk->code[offset + 1];
    uint8_t dst = REG_POS1(regs);
    uint8_t a   = REG_POS2(regs);
    uint8_t b   = REG_POS3(regs);
    printf("%-16s ", name);
    printReg(dst);
    printf(", ");
    printReg(a);
    printf(", ");
    printReg(b);
    printf("\n");
    return offset + 2;
}

static int reg3MaybeImmInstruction(const char *name, Chunk *chunk, int offset) {
    uint16_t regs = chunk->code[offset + 1];
    uint8_t dst   = REG_POS1(regs);
    uint8_t aNib  = REG_POS2(regs);
    uint8_t bNib  = REG_POS3(regs);
    uint8_t flags = REG_POS4(regs);

    int cursor = offset + 2; // start after [op][regs]
    printf("%-16s ", name);
    printReg(dst);
    printf(", ");

    // srcA
    if (flags & 0x1) {
        // read 8-byte imm typed by dst
        if (dst < 8) {
            long long iv = 0;
            unsigned char buf[8];
            uint16_t *w = &chunk->code[cursor];
            memcpy(buf + 0, &w[0], 2);
            memcpy(buf + 2, &w[1], 2);
            memcpy(buf + 4, &w[2], 2);
            memcpy(buf + 6, &w[3], 2);
            memcpy(&iv, buf, 8);
            printf("#%lld", iv);
        } else {
            double dv = 0.0;
            unsigned char buf[8];
            uint16_t *w = &chunk->code[cursor];
            memcpy(buf + 0, &w[0], 2);
            memcpy(buf + 2, &w[1], 2);
            memcpy(buf + 4, &w[2], 2);
            memcpy(buf + 6, &w[3], 2);
            memcpy(&dv, buf, 8);
            printf("#%.17g", dv);
        }
        cursor += 4;
    } else {
        printReg(aNib);
    }
    printf(", ");

    // srcB
    if (flags & 0x2) {
        if (dst < 8) {
            long long iv = 0;
            unsigned char buf[8];
            uint16_t *w = &chunk->code[cursor];
            memcpy(buf + 0, &w[0], 2);
            memcpy(buf + 2, &w[1], 2);
            memcpy(buf + 4, &w[2], 2);
            memcpy(buf + 6, &w[3], 2);
            memcpy(&iv, buf, 8);
            printf("#%lld", iv);
        } else {
            double dv = 0.0;
            unsigned char buf[8];
            uint16_t *w = &chunk->code[cursor];
            memcpy(buf + 0, &w[0], 2);
            memcpy(buf + 2, &w[1], 2);
            memcpy(buf + 4, &w[2], 2);
            memcpy(buf + 6, &w[3], 2);
            memcpy(&dv, buf, 8);
            printf("#%.17g", dv);
        }
        cursor += 4;
    } else {
        printReg(bNib);
    }
    printf("\n");

    return cursor;
}

static int regLoadInstruction(Chunk *chunk, int offset) {
    uint16_t regs = chunk->code[offset + 1];
    uint8_t dst = REG_POS1(regs);

    // 8‑byte payload follows in four 16‑bit words
    // We'll decode as int64 for i* and double for f*
    if (dst < 8) {
        long long iv = 0;
        // memcpy is safest, but reading via a local buffer is fine here
        unsigned char buf[8];
        // chunk->code is uint16_t[]; grab 4 words in native order
        uint16_t *w = &chunk->code[offset + 2];
        memcpy(buf + 0, &w[0], 2);
        memcpy(buf + 2, &w[1], 2);
        memcpy(buf + 4, &w[2], 2);
        memcpy(buf + 6, &w[3], 2);
        memcpy(&iv, buf, 8);

        printf("%-16s ", "R_LOAD");
        printReg(dst);
        printf(", %lld\n", iv);
    } else {
        double dv = 0.0;
        unsigned char buf[8];
        uint16_t *w = &chunk->code[offset + 2];
        memcpy(buf + 0, &w[0], 2);
        memcpy(buf + 2, &w[1], 2);
        memcpy(buf + 4, &w[2], 2);
        memcpy(buf + 6, &w[3], 2);
        memcpy(&dv, buf, 8);

        printf("%-16s ", "R_LOAD");
        printReg(dst);
        printf(", %.17g\n", dv);
    }
    return offset + 6; // op + regs + 4 words payload
}

static int regILoadInstruction(Chunk *chunk, int offset) {
    uint16_t regs = chunk->code[offset + 1];
    uint8_t dst = REG_POS1(regs);

    long long iv = 0;
    unsigned char buf[8];
    uint16_t *w = &chunk->code[offset + 2];
    memcpy(buf + 0, &w[0], 2);
    memcpy(buf + 2, &w[1], 2);
    memcpy(buf + 4, &w[2], 2);
    memcpy(buf + 6, &w[3], 2);
    memcpy(&iv, buf, 8);

    printf("%-16s ", "R_ILOAD_IMM");
    printReg(dst);
    printf(", %lld\n", iv);
    return offset + 6; // op + regs + 4 words payload
}

static int regFLoadInstruction(Chunk *chunk, int offset) {
    uint16_t regs = chunk->code[offset + 1];
    uint8_t dst = REG_POS1(regs);

    double dv = 0.0;
    unsigned char buf[8];
    uint16_t *w = &chunk->code[offset + 2];
    memcpy(buf + 0, &w[0], 2);
    memcpy(buf + 2, &w[1], 2);
    memcpy(buf + 4, &w[2], 2);
    memcpy(buf + 6, &w[3], 2);
    memcpy(&dv, buf, 8);

    printf("%-16s ", "R_FLOAD_IMM");
    printReg(dst);
    printf(", %.17g\n", dv);
    return offset + 6; // op + regs + 4 words payload
}


// ---------- Typed ALU helpers (I/F × RR/RI/IR/II) ----------

static int aluRR(const char *name, Chunk *chunk, int offset) {
    // Layout: [op][regs]
    uint16_t regs = chunk->code[offset + 1];
    uint8_t dst = REG_POS1(regs);
    uint8_t a   = REG_POS2(regs);
    uint8_t b   = REG_POS3(regs);

    printf("%-16s ", name);
    printReg(dst);
    printf(", ");
    printReg(a);
    printf(", ");
    printReg(b);
    printf("\n");
    return offset + 2;
}

static int aluRI(const char *name, bool isInt, Chunk *chunk, int offset) {
    // Layout: [op][regs][imm8]
    uint16_t regs = chunk->code[offset + 1];
    uint8_t dst = REG_POS1(regs);
    uint8_t a   = REG_POS2(regs); // reg
    int cursor = offset + 2;

    printf("%-16s ", name);
    printReg(dst);
    printf(", ");
    printReg(a);
    printf(", ");

    if (isInt) {
        long long iv = 0;
        unsigned char buf[8];
        uint16_t *w = &chunk->code[cursor];
        memcpy(buf + 0, &w[0], 2);
        memcpy(buf + 2, &w[1], 2);
        memcpy(buf + 4, &w[2], 2);
        memcpy(buf + 6, &w[3], 2);
        memcpy(&iv, buf, 8);
        printf("#%lld\n", iv);
    } else {
        double dv = 0.0;
        unsigned char buf[8];
        uint16_t *w = &chunk->code[cursor];
        memcpy(buf + 0, &w[0], 2);
        memcpy(buf + 2, &w[1], 2);
        memcpy(buf + 4, &w[2], 2);
        memcpy(buf + 6, &w[3], 2);
        memcpy(&dv, buf, 8);
        printf("#%.17g\n", dv);
    }
    return cursor + 4;
}

static int aluIR(const char *name, bool isInt, Chunk *chunk, int offset) {
    // Layout: [op][regs][imm8]
    uint16_t regs = chunk->code[offset + 1];
    uint8_t dst = REG_POS1(regs);
    uint8_t b   = REG_POS3(regs); // reg
    int cursor = offset + 2;

    printf("%-16s ", name);
    printReg(dst);
    printf(", ");

    // immediate first
    if (isInt) {
        long long iv = 0;
        unsigned char buf[8];
        uint16_t *w = &chunk->code[cursor];
        memcpy(buf + 0, &w[0], 2);
        memcpy(buf + 2, &w[1], 2);
        memcpy(buf + 4, &w[2], 2);
        memcpy(buf + 6, &w[3], 2);
        memcpy(&iv, buf, 8);
        printf("#%lld", iv);
    } else {
        double dv = 0.0;
        unsigned char buf[8];
        uint16_t *w = &chunk->code[cursor];
        memcpy(buf + 0, &w[0], 2);
        memcpy(buf + 2, &w[1], 2);
        memcpy(buf + 4, &w[2], 2);
        memcpy(buf + 6, &w[3], 2);
        memcpy(&dv, buf, 8);
        printf("#%.17g", dv);
    }
    cursor += 4;

    printf(", ");
    printReg(b);
    printf("\n");
    return cursor;
}

static int aluII(const char *name, bool isInt, Chunk *chunk, int offset) {
    // Layout: [op][regs][imm8 A][imm8 B]
    uint16_t regs = chunk->code[offset + 1];
    uint8_t dst = REG_POS1(regs);
    int cursor = offset + 2;

    printf("%-16s ", name);
    printReg(dst);
    printf(", ");

    if (isInt) {
        long long ia = 0, ib = 0;
        { unsigned char buf[8]; uint16_t *w = &chunk->code[cursor];
          memcpy(buf + 0, &w[0], 2); memcpy(buf + 2, &w[1], 2);
          memcpy(buf + 4, &w[2], 2); memcpy(buf + 6, &w[3], 2);
          memcpy(&ia, buf, 8); }
        cursor += 4;
        { unsigned char buf[8]; uint16_t *w = &chunk->code[cursor];
          memcpy(buf + 0, &w[0], 2); memcpy(buf + 2, &w[1], 2);
          memcpy(buf + 4, &w[2], 2); memcpy(buf + 6, &w[3], 2);
          memcpy(&ib, buf, 8); }
        cursor += 4;
        printf("#%lld, #%lld\n", ia, ib);
    } else {
        double da = 0.0, db = 0.0;
        { unsigned char buf[8]; uint16_t *w = &chunk->code[cursor];
          memcpy(buf + 0, &w[0], 2); memcpy(buf + 2, &w[1], 2);
          memcpy(buf + 4, &w[2], 2); memcpy(buf + 6, &w[3], 2);
          memcpy(&da, buf, 8); }
        cursor += 4;
        { unsigned char buf[8]; uint16_t *w = &chunk->code[cursor];
          memcpy(buf + 0, &w[0], 2); memcpy(buf + 2, &w[1], 2);
          memcpy(buf + 4, &w[2], 2); memcpy(buf + 6, &w[3], 2);
          memcpy(&db, buf, 8); }
        cursor += 4;
        printf("#%.17g, #%.17g\n", da, db);
    }
    return cursor;
}

static int regJumpInstruction(const char *name, Chunk *chunk, int offset) {
    // Layout: [op][regs][u16 offset]
    uint16_t regs = chunk->code[offset + 1];
    uint8_t r = REG_POS1(regs);
    uint16_t off = chunk->code[offset + 2];
    int target = offset + 3 + off; // after reading 3 words, VM adds 'off' words

    printf("%-16s %4d ", name, offset);
    printReg(r);
    printf(" -> %d\n", target);
    return offset + 3;
}

// [op][regs][u16 slot] or [op][regs][u16 index]
static int regShortInstruction(const char *name, Chunk *chunk, int offset) {
    uint16_t regs = chunk->code[offset + 1];
    uint8_t r = REG_POS1(regs);
    uint16_t s = chunk->code[offset + 2];
    printf("%-16s ", name);
    printReg(r);
    printf(", %u\n", (unsigned)s);
    return offset + 3;
}

// [op][regs][constIndex] -- prints name if it is a string const
static int regConstNameInstruction(const char *name, Chunk *chunk, int offset) {
    uint16_t regs = chunk->code[offset + 1];
    uint8_t r = REG_POS1(regs);
    uint16_t ci = chunk->code[offset + 2];
    printf("%-16s ", name);
    printReg(r);
    printf(", ");
    if (ci < chunk->constants.count && IS_STRING(chunk->constants.values[ci])) {
        ObjString *s = AS_STRING(chunk->constants.values[ci]);
        printf("'%.*s'\n", s->length, s->chars);
    } else {
        printf("#const[%u]\n", (unsigned)ci);
    }
    return offset + 3;
}

static int regConstInstruction(const char *name, Chunk *chunk, int offset) {
    uint16_t regs = chunk->code[offset + 1];
    uint8_t r = REG_POS1(regs);
    uint16_t c = chunk->code[offset + 2];
    printf("%-16s ", name);
    printReg(r);
    printf(", '");
    printValue(chunk->constants.values[c]);
    printf("'\n");
    return offset + 3;
}

static int regIndexInstruction(const char *name, Chunk *chunk, int offset) {
    uint16_t regs = chunk->code[offset + 1];
    uint8_t r = REG_POS1(regs);
    uint16_t idx = chunk->code[offset + 2];
    printf("%-16s ", name);
    printReg(r);
    printf(", %u\n", (unsigned)idx);
    return offset + 3;
}

// ---- list binding disasm helpers ----
static int listBindLocalInstruction(const char *name, Chunk *chunk, int offset) {
    uint16_t regs = chunk->code[offset + 1];
    uint8_t lenDst = REG_POS1(regs);
    uint8_t h      = REG_POS3(regs);
    uint16_t slot  = chunk->code[offset + 2];

    printf("%-16s ", name);
    if (lenDst < 8) printf("i%u, ", lenDst); else printf("-, ");
    printHandle(h);
    printf(", local %u\n", (unsigned)slot);
    return offset + 3;
}

static int listBindUpvalInstruction(const char *name, Chunk *chunk, int offset) {
    uint16_t regs = chunk->code[offset + 1];
    uint8_t lenDst = REG_POS1(regs);
    uint8_t h      = REG_POS3(regs);
    uint16_t idx   = chunk->code[offset + 2];

    printf("%-16s ", name);
    if (lenDst < 8) printf("i%u, ", lenDst); else printf("-, ");
    printHandle(h);
    printf(", upval %u\n", (unsigned)idx);
    return offset + 3;
}

static int listBindGlobalInstruction(const char *name, Chunk *chunk, int offset) {
    uint16_t regs = chunk->code[offset + 1];
    uint8_t lenDst = REG_POS1(regs);
    uint8_t h      = REG_POS3(regs);
    uint16_t ci    = chunk->code[offset + 2];

    printf("%-16s ", name);
    if (lenDst < 8) printf("i%u, ", lenDst); else printf("-, ");
    printHandle(h);
    printf(", ");

    if (ci < chunk->constants.count && IS_STRING(chunk->constants.values[ci])) {
        ObjString *s = AS_STRING(chunk->constants.values[ci]);
        printf("global '%.*s'\n", s->length, s->chars);
    } else {
        printf("global #const[%u]\n", (unsigned)ci);
    }
    return offset + 3;
}

static int listLoadRWithHandle(const char* name, Chunk* c, int off) {
    uint16_t regs = c->code[off + 1];
    uint8_t dst = REG_POS1(regs), ir = REG_POS2(regs), h = REG_POS3(regs);
    printf("%-16s ", name); printReg(dst); printf(", i%u, ", ir); printHandle(h); printf("\n");
    return off + 2;
}

static int listLoadIWithHandle(const char* name, Chunk* c, int off) {
    uint16_t regs = c->code[off + 1];
    uint8_t dst = REG_POS1(regs), h = REG_POS3(regs);
    long long iv=0; unsigned char buf[8]; uint16_t* w=&c->code[off+2];
    memcpy(buf,&w[0],2); memcpy(buf+2,&w[1],2); memcpy(buf+4,&w[2],2); memcpy(buf+6,&w[3],2);
    memcpy(&iv,buf,8);
    printf("%-16s ", name); printReg(dst); printf(", #%lld, ", iv); printHandle(h); printf("\n");
    return off + 6;
}

static int listStoreRWithHandle(const char* name, Chunk* c, int off) {
    uint16_t regs = c->code[off + 1];
    uint8_t src = REG_POS1(regs), ir = REG_POS2(regs), h = REG_POS3(regs);
    printf("%-16s ", name); printReg(src); printf(", i%u, ", ir); printHandle(h); printf("\n");
    return off + 2;
}

static int listStoreIWithHandle(const char* name, Chunk* c, int off) {
    uint16_t regs = c->code[off + 1];
    uint8_t src = REG_POS1(regs), h = REG_POS3(regs);
    long long iv=0; unsigned char buf[8]; uint16_t* w=&c->code[off+2];
    memcpy(buf,&w[0],2); memcpy(buf+2,&w[1],2); memcpy(buf+4,&w[2],2); memcpy(buf+6,&w[3],2);
    memcpy(&iv,buf,8);
    printf("%-16s ", name); printReg(src); printf(", #%lld, ", iv); printHandle(h); printf("\n");
    return off + 6;
}

static int listLenWithHandle(Chunk* c, int off) {
    uint16_t regs = c->code[off + 1];
    uint8_t dst = REG_POS1(regs), h = REG_POS3(regs);
    printf("%-16s i%u, ", "R_LLEN", dst); printHandle(h); printf("\n");
    return off + 2;
}

void disassembleChunk(Chunk *chunk, const char *name)
{
    printf("== %s ==\n", name);

    for (int offset = 0; offset < chunk->count;)
    {
        offset = disassembleInstruction(chunk, offset);
    }
}

static int constantInstruction(const char *name, Chunk *chunk, int offset)
{
    uint16_t constant = chunk->code[offset + 1];
    printf("%-16s %4d '", name, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 2;
}

static int invokeInstruction(const char *name, Chunk *chunk, int offset)
{
    uint16_t constant = chunk->code[offset + 1];
    uint8_t argCount = (uint8_t)chunk->code[offset + 2];
    printf("%-16s (%d args) %4d '", name, argCount, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 3;
}

static int simpleInstruction(const char *name, int offset)
{
    printf("%s\n", name);
    return offset + 1;
}

static int byteInstruction(const char *name, Chunk *chunk, int offset)
{
    uint8_t slot = (uint8_t)chunk->code[offset + 1];
    printf("%-16s %4d\n", name, slot);
    return offset + 2;
}

static int shortInstruction(const char *name, Chunk *chunk, int offset)
{
    uint16_t slot = chunk->code[offset + 1];
    printf("%-16s %4d\n", name, slot);
    return offset + 2;
}

static int jumpInstruction(const char *name, int sign, Chunk *chunk, int offset)
{
    uint16_t jump = chunk->code[offset + 1];
    printf("%-16s %4d -> %d\n", name, offset, offset + 2 + sign * jump);
    return offset + 2;
}

int disassembleInstruction(Chunk *chunk, int offset)
{
    printf("%04d ", offset);
    if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1])
    {
        printf("   | ");
    } else
    {
        printf("%4d ", chunk->lines[offset]);
    }

    uint16_t instruction = chunk->code[offset];
    switch (instruction)
    {
        case PUSH_CONST: return constantInstruction("PUSH_CONST", chunk, offset);
        case PUSH_NULL: return simpleInstruction("PUSH_NULL", offset);
        case PUSH_TRUE: return simpleInstruction("PUSH_TRUE", offset);
        case PUSH_FALSE: return simpleInstruction("PUSH_FALSE", offset);
        case DUP: return simpleInstruction("DUP", offset);
        case DUP2: return simpleInstruction("DUP2", offset);
        case POP: return simpleInstruction("POP", offset);
        case LD_LOCAL: return byteInstruction("LD_LOCAL", chunk, offset);
        case ST_LOCAL: return byteInstruction("ST_LOCAL", chunk, offset);
        case LD_GLOBAL: return constantInstruction("LD_GLOBAL", chunk, offset);
        case LD_GLOBAL_I: return shortInstruction("LD_GLOBAL_I", chunk, offset);
        case DEF_GLOBAL: return constantInstruction("DEF_GLOBAL", chunk, offset);
        case ST_GLOBAL: return constantInstruction("ST_GLOBAL", chunk, offset);
        case ST_GLOBAL_I: return shortInstruction("ST_GLOBAL_I", chunk, offset);
        case LD_UPVAL: return byteInstruction("LD_UPVAL", chunk, offset);
        case ST_UPVAL: return byteInstruction("ST_UPVAL", chunk, offset);
        case LD_PROP: return constantInstruction("LD_PROP", chunk, offset);
        case ST_PROP: return constantInstruction("ST_PROP", chunk, offset);
        case LD_INDEX: return simpleInstruction("LD_INDEX", offset);
        case ST_INDEX: return simpleInstruction("ST_INDEX", offset);
        case LD_SUPER: return constantInstruction("LD_SUPER", chunk, offset);
        case EQ: return simpleInstruction("EQ", offset);
        case NEQ: return simpleInstruction("NEQ", offset);
        case EQ_C: return constantInstruction("EQ_C", chunk, offset);
        case NEQ_C: return constantInstruction("NEQ_C", chunk, offset);
        case GT: return simpleInstruction("GT", offset);
        case GT_C: return constantInstruction("GT_C", chunk, offset);
        case LT: return simpleInstruction("LT", offset);
        case LT_C: return constantInstruction("LT_C", chunk, offset);
        case ADD: return simpleInstruction("ADD", offset);
        case ADD_C: return constantInstruction("ADD_C", chunk, offset);
        case SUB: return simpleInstruction("SUB", offset);
        case SUB_C: return constantInstruction("SUB_C", chunk, offset);
        case MUL: return simpleInstruction("MUL", offset);
        case MUL_C: return constantInstruction("MUL_C", chunk, offset);
        case DIV: return simpleInstruction("DIV", offset);
        case DIV_C: return constantInstruction("DIV_C", chunk, offset);
        case MOD: return simpleInstruction("MOD", offset);
        case MOD_C: return constantInstruction("MOD_C", chunk, offset);
        case NOT: return simpleInstruction("NOT", offset);
        case NEG: return simpleInstruction("NEG", offset);
        case BAND: return simpleInstruction("BAND", offset);
        case BAND_C: return constantInstruction("BAND_C", chunk, offset);
        case BOR: return simpleInstruction("BOR", offset);
        case BOR_C: return constantInstruction("BOR_C", chunk, offset);
        case BXOR: return simpleInstruction("BXOR", offset);
        case BXOR_C: return constantInstruction("BXOR_C", chunk, offset);
        case BNOT: return simpleInstruction("BNOT", offset);
        case BSHL: return simpleInstruction("BSHL", offset);
        case BSHL_C: return constantInstruction("BSHL_C", chunk, offset);
        case BSHR: return simpleInstruction("BSHR", offset);
        case BSHR_C: return constantInstruction("BSHR_C", chunk, offset);
        case DBG_PRINT: return simpleInstruction("DBG_PRINT", offset);
        case JMP: return jumpInstruction("JMP", 1, chunk, offset);
        case JMP_IF_FALSE: return jumpInstruction("JMP_IF_FALSE", 1, chunk, offset);
        case PJMP_IF_FALSE: return jumpInstruction("PJMP_IF_FALSE", 1, chunk, offset);
        case JMP_IF_TRUE: return jumpInstruction("JMP_IF_TRUE", 1, chunk, offset);
        case PJMP_IF_TRUE: return jumpInstruction("PJMP_IF_TRUE", 1, chunk, offset);
        case JMP_BACK: return jumpInstruction("JMP_BACK", -1, chunk, offset);
        case CALL: return byteInstruction("CALL", chunk, offset);
        case FAST_NATIVE_CALL: {
            uint16_t nativeId = chunk->code[offset + 1];
            uint8_t argCount = (uint8_t)chunk->code[offset + 2];
            printf("%-16s (%d args) %4d '%s'\n", "FAST_NATIVE_CALL", argCount, nativeId, G_FAST_NATIVES[nativeId].name);
            return offset + 3;
        }
        case INVOKE: return invokeInstruction("INVOKE", chunk, offset);
        case INVOKE_SUPER: return invokeInstruction("INVOKE_SUPER", chunk, offset);
        case MAKE_CLOSURE:
        {
            offset++;
            uint16_t constant = chunk->code[offset++];
            printf("%-16s %4d ", "MAKE_CLOSURE", constant);
            printValue(chunk->constants.values[constant]);
            printf("\n");

            ObjFunction *function = AS_FUNCTION(chunk->constants.values[constant]);
            for (int j = 0; j < function->upvalueCount; j++)
            {
                int isLocal = (uint8_t)chunk->code[offset++];
                int index = (uint8_t)chunk->code[offset++];
                printf("%04d      |                     %s %d\n", offset - 2, isLocal ? "local" : "upvalue", index);
            }

            return offset;
        }
        case CLOSE_UPVAL: return simpleInstruction("CLOSE_UPVAL", offset);
        case MAKE_LIST: return simpleInstruction("MAKE_LIST", offset);
        case LIST_DATA: return simpleInstruction("LIST_DATA", offset);
        case MAKE_MAP: return simpleInstruction("MAKE_MAP", offset);
        case MAP_DATA: return simpleInstruction("MAP_DATA", offset);
        case RET: return simpleInstruction("RET", offset);
        case MAKE_CLASS: return constantInstruction("MAKE_CLASS", chunk, offset);
        case CLASS_INHERIT: return simpleInstruction("CLASS_INHERIT", offset);
        case DEF_METHOD: return constantInstruction("DEF_METHOD", chunk, offset);

            // Prefix/Postfix
        case PRE_INC_LOCAL: return byteInstruction("PRE_INC_LOCAL", chunk, offset);
        case PRE_DEC_LOCAL: return byteInstruction("PRE_DEC_LOCAL", chunk, offset);
        case POST_INC_LOCAL: return byteInstruction("POST_INC_LOCAL", chunk, offset);
        case POST_DEC_LOCAL: return byteInstruction("POST_DEC_LOCAL", chunk, offset);
        case PRE_INC_GLOBAL: return constantInstruction("PRE_INC_GLOBAL", chunk, offset);
        case PRE_DEC_GLOBAL: return constantInstruction("PRE_DEC_GLOBAL", chunk, offset);
        case POST_INC_GLOBAL: return constantInstruction("POST_INC_GLOBAL", chunk, offset);
        case POST_DEC_GLOBAL: return constantInstruction("POST_DEC_GLOBAL", chunk, offset);
        case PRE_INC_GLOBAL_I: return shortInstruction("PRE_INC_GLOBAL_I", chunk, offset);
        case PRE_DEC_GLOBAL_I: return shortInstruction("PRE_DEC_GLOBAL_I", chunk, offset);
        case POST_INC_GLOBAL_I: return shortInstruction("POST_INC_GLOBAL_I", chunk, offset);
        case POST_DEC_GLOBAL_I: return shortInstruction("POST_DEC_GLOBAL_I", chunk, offset);
        case PRE_INC_UPVAL: return byteInstruction("PRE_INC_UPVALUE", chunk, offset);
        case PRE_DEC_UPVAL: return byteInstruction("PRE_DEC_UPVALUE", chunk, offset);
        case POST_INC_UPVAL: return byteInstruction("POST_INC_UPVAL", chunk, offset);
        case POST_DEC_UPVAL: return byteInstruction("POST_DEC_UPVAL", chunk, offset);
        case PRE_INC_PROP: return constantInstruction("PRE_INC_PROP", chunk, offset);
        case PRE_DEC_PROP: return constantInstruction("PRE_DEC_PROP", chunk, offset);
        case POST_INC_PROP: return constantInstruction("POST_INC_PROP", chunk, offset);
        case POST_DEC_PROP: return constantInstruction("POST_DEC_PROP", chunk, offset);
        case PRE_INC_INDEX: return simpleInstruction("PRE_INC_INDEX", offset);
        case PRE_DEC_INDEX: return simpleInstruction("PRE_DEC_INDEX", offset);
        case POST_INC_INDEX: return simpleInstruction("POST_INC_INDEX", offset);
        case POST_DEC_INDEX: return simpleInstruction("POST_DEC_INDEX", offset);

            // microasm: typed immediate loads
        case R_ILOAD_IMM: return regILoadInstruction(chunk, offset);
        case R_FLOAD_IMM: return regFLoadInstruction(chunk, offset);

            // ---- microasm: fast reg <-> local/global and ret ----
        case R_LD_LOCAL_R:  return regShortInstruction("R_LD_LOCAL_R",  chunk, offset);
        case R_ST_LOCAL_R:  return regShortInstruction("R_ST_LOCAL_R",  chunk, offset);
        case R_LD_UPVAL_R:  return regShortInstruction("R_LD_UPVAL_R",  chunk, offset);
        case R_ST_UPVAL_R:  return regShortInstruction("R_ST_UPVAL_R",  chunk, offset);
        case R_LD_GLOBAL_R: return regConstInstruction("R_LD_GLOBAL_R", chunk, offset);
        case R_ST_GLOBAL_R: return regConstInstruction("R_ST_GLOBAL_R", chunk, offset);
        case R_RET_R:       return reg1Instruction("R_RET_R",           chunk, offset);

            // microasm: scratch typed
        case R_SST:  return regIndexInstruction("R_SST",  chunk, offset);
        case R_SLD:  return regIndexInstruction("R_SLD",  chunk, offset);

            // ---- microasm: typed ALU (ints) ----
        case R_IADD_RR: return aluRR("R_IADD_RR", chunk, offset);
        case R_IADD_RI: return aluRI("R_IADD_RI", true,  chunk, offset);
        case R_IADD_IR: return aluIR("R_IADD_IR", true,  chunk, offset);
        case R_IADD_II: return aluII("R_IADD_II", true,  chunk, offset);

        case R_ISUB_RR: return aluRR("R_ISUB_RR", chunk, offset);
        case R_ISUB_RI: return aluRI("R_ISUB_RI", true,  chunk, offset);
        case R_ISUB_IR: return aluIR("R_ISUB_IR", true,  chunk, offset);
        case R_ISUB_II: return aluII("R_ISUB_II", true,  chunk, offset);

        case R_IMUL_RR: return aluRR("R_IMUL_RR", chunk, offset);
        case R_IMUL_RI: return aluRI("R_IMUL_RI", true,  chunk, offset);
        case R_IMUL_IR: return aluIR("R_IMUL_IR", true,  chunk, offset);
        case R_IMUL_II: return aluII("R_IMUL_II", true,  chunk, offset);

        case R_IDIV_RR: return aluRR("R_IDIV_RR", chunk, offset);
        case R_IDIV_RI: return aluRI("R_IDIV_RI", true,  chunk, offset);
        case R_IDIV_IR: return aluIR("R_IDIV_IR", true,  chunk, offset);
        case R_IDIV_II: return aluII("R_IDIV_II", true,  chunk, offset);

        case R_IMOD_RR: return aluRR("R_IMOD_RR", chunk, offset);
        case R_IMOD_RI: return aluRI("R_IMOD_RI", true,  chunk, offset);
        case R_IMOD_IR: return aluIR("R_IMOD_IR", true,  chunk, offset);
        case R_IMOD_II: return aluII("R_IMOD_II", true,  chunk, offset);

            // ---- microasm: typed ALU (floats) ----
        case R_FADD_RR: return aluRR("R_FADD_RR", chunk, offset);
        case R_FADD_RI: return aluRI("R_FADD_RI", false, chunk, offset);
        case R_FADD_IR: return aluIR("R_FADD_IR", false, chunk, offset);
        case R_FADD_II: return aluII("R_FADD_II", false, chunk, offset);

        case R_FSUB_RR: return aluRR("R_FSUB_RR", chunk, offset);
        case R_FSUB_RI: return aluRI("R_FSUB_RI", false, chunk, offset);
        case R_FSUB_IR: return aluIR("R_FSUB_IR", false, chunk, offset);
        case R_FSUB_II: return aluII("R_FSUB_II", false, chunk, offset);

        case R_FMUL_RR: return aluRR("R_FMUL_RR", chunk, offset);
        case R_FMUL_RI: return aluRI("R_FMUL_RI", false, chunk, offset);
        case R_FMUL_IR: return aluIR("R_FMUL_IR", false, chunk, offset);
        case R_FMUL_II: return aluII("R_FMUL_II", false, chunk, offset);

        case R_FDIV_RR: return aluRR("R_FDIV_RR", chunk, offset);
        case R_FDIV_RI: return aluRI("R_FDIV_RI", false, chunk, offset);
        case R_FDIV_IR: return aluIR("R_FDIV_IR", false, chunk, offset);
        case R_FDIV_II: return aluII("R_FDIV_II", false, chunk, offset);

        case R_FMOD_RR: return aluRR("R_FMOD_RR", chunk, offset);
        case R_FMOD_RI: return aluRI("R_FMOD_RI", false, chunk, offset);
        case R_FMOD_IR: return aluIR("R_FMOD_IR", false, chunk, offset);
        case R_FMOD_II: return aluII("R_FMOD_II", false, chunk, offset);

            // ---- microasm: INT64 bitwise (AND/OR/XOR) ----
        case R_IAND_RR: return aluRR("R_IAND_RR", chunk, offset);
        case R_IAND_RI: return aluRI("R_IAND_RI", true,  chunk, offset);
        case R_IAND_IR: return aluIR("R_IAND_IR", true,  chunk, offset);
        case R_IAND_II: return aluII("R_IAND_II", true,  chunk, offset);

        case R_IOR_RR:  return aluRR("R_IOR_RR",  chunk, offset);
        case R_IOR_RI:  return aluRI("R_IOR_RI",  true,  chunk, offset);
        case R_IOR_IR:  return aluIR("R_IOR_IR",  true,  chunk, offset);
        case R_IOR_II:  return aluII("R_IOR_II",  true,  chunk, offset);

        case R_IXOR_RR: return aluRR("R_IXOR_RR", chunk, offset);
        case R_IXOR_RI: return aluRI("R_IXOR_RI", true,  chunk, offset);
        case R_IXOR_IR: return aluIR("R_IXOR_IR", true,  chunk, offset);
        case R_IXOR_II: return aluII("R_IXOR_II", true,  chunk, offset);

            // ---- microasm: INT64 shifts (SHL/SHR logical, SAR arithmetic) ----
        case R_ISHL_RR: return aluRR("R_ISHL_RR", chunk, offset);
        case R_ISHL_RI: return aluRI("R_ISHL_RI", true,  chunk, offset);
        case R_ISHL_IR: return aluIR("R_ISHL_IR", true,  chunk, offset);
        case R_ISHL_II: return aluII("R_ISHL_II", true,  chunk, offset);

        case R_ISHR_RR: return aluRR("R_ISHR_RR", chunk, offset); // logical >>
        case R_ISHR_RI: return aluRI("R_ISHR_RI", true,  chunk, offset);
        case R_ISHR_IR: return aluIR("R_ISHR_IR", true,  chunk, offset);
        case R_ISHR_II: return aluII("R_ISHR_II", true,  chunk, offset);

        case R_ISAR_RR: return aluRR("R_ISAR_RR", chunk, offset); // arithmetic >>
        case R_ISAR_RI: return aluRI("R_ISAR_RI", true,  chunk, offset);
        case R_ISAR_IR: return aluIR("R_ISAR_IR", true,  chunk, offset);
        case R_ISAR_II: return aluII("R_ISAR_II", true,  chunk, offset);

            // ---- microasm: INT64 rotates (ROL/ROR) ----
        case R_IROL_RR: return aluRR("R_IROL_RR", chunk, offset);
        case R_IROL_RI: return aluRI("R_IROL_RI", true,  chunk, offset);
        case R_IROL_IR: return aluIR("R_IROL_IR", true,  chunk, offset);
        case R_IROL_II: return aluII("R_IROL_II", true,  chunk, offset);

        case R_IROR_RR: return aluRR("R_IROR_RR", chunk, offset);
        case R_IROR_RI: return aluRI("R_IROR_RI", true,  chunk, offset);
        case R_IROR_IR: return aluIR("R_IROR_IR", true,  chunk, offset);
        case R_IROR_II: return aluII("R_IROR_II", true,  chunk, offset);

            // ---- microasm: INT64 bit test / set / clear / toggle ----
        case R_IBTST_RR: return aluRR("R_IBTST_RR", chunk, offset);
        case R_IBTST_RI: return aluRI("R_IBTST_RI", true,  chunk, offset);
        case R_IBTST_IR: return aluIR("R_IBTST_IR", true,  chunk, offset);
        case R_IBTST_II: return aluII("R_IBTST_II", true,  chunk, offset);

        case R_IBSET_RR: return aluRR("R_IBSET_RR", chunk, offset);
        case R_IBSET_RI: return aluRI("R_IBSET_RI", true,  chunk, offset);
        case R_IBSET_IR: return aluIR("R_IBSET_IR", true,  chunk, offset);
        case R_IBSET_II: return aluII("R_IBSET_II", true,  chunk, offset);

        case R_IBCLR_RR: return aluRR("R_IBCLR_RR", chunk, offset);
        case R_IBCLR_RI: return aluRI("R_IBCLR_RI", true,  chunk, offset);
        case R_IBCLR_IR: return aluIR("R_IBCLR_IR", true,  chunk, offset);
        case R_IBCLR_II: return aluII("R_IBCLR_II", true,  chunk, offset);

        case R_IBTGL_RR: return aluRR("R_IBTGL_RR", chunk, offset);
        case R_IBTGL_RI: return aluRI("R_IBTGL_RI", true,  chunk, offset);
        case R_IBTGL_IR: return aluIR("R_IBTGL_IR", true,  chunk, offset);
        case R_IBTGL_II: return aluII("R_IBTGL_II", true,  chunk, offset);

            // ---- microasm: INT64 unary bitwise not ----
        case R_IBNOT:    return reg2Instruction("R_IBNOT", chunk, offset);

            // ---- microasm: bound list ----
        case R_LBIND_LOCAL:  return listBindLocalInstruction ("R_LBIND_LOCAL",  chunk, offset);
        case R_LBIND_UPVAL:  return listBindUpvalInstruction ("R_LBIND_UPVAL",  chunk, offset);
        case R_LBIND_GLOBAL: return listBindGlobalInstruction("R_LBIND_GLOBAL", chunk, offset);

        case R_LLEN:         return listLenWithHandle(chunk, offset);

        case R_LLOAD_R:   return listLoadRWithHandle ("R_LLOAD_R",  chunk, offset);
        case R_LLOAD_I:   return listLoadIWithHandle ("R_LLOAD_I",  chunk, offset);
        case R_LSTORE_R:  return listStoreRWithHandle("R_LSTORE_R", chunk, offset);
        case R_LSTORE_I:  return listStoreIWithHandle("R_LSTORE_I", chunk, offset);

        case R_LLOADU_R:  return listLoadRWithHandle ("R_LLOADU_R",  chunk, offset);
        case R_LLOADU_I:  return listLoadIWithHandle ("R_LLOADU_I",  chunk, offset);
        case R_LSTOREU_R: return listStoreRWithHandle("R_LSTOREU_R", chunk, offset);
        case R_LSTOREU_I: return listStoreIWithHandle("R_LSTOREU_I", chunk, offset);

            // streaming
        case R_LLOAD_PI:   return listLoadRWithHandle ("R_LLOAD_PI",   chunk, offset);
        case R_LLOADU_PI:  return listLoadRWithHandle ("R_LLOADU_PI",  chunk, offset);
        case R_LSTORE_PI:  return listStoreRWithHandle("R_LSTORE_PI",  chunk, offset);
        case R_LSTOREU_PI: return listStoreRWithHandle("R_LSTOREU_PI", chunk, offset);


            // ---- microasm: registers ----
        case R_MOV:  return reg2Instruction("R_MOV",  chunk, offset);
        case R_PUSH: return reg1Instruction("R_PUSH", chunk, offset);
        case R_POP:  return reg1Instruction("R_POP",  chunk, offset);

            // ---- microasm: reg-conditional jumps ----
        case R_JZ:  return regJumpInstruction("R_JZ",  chunk, offset);
        case R_JNZ: return regJumpInstruction("R_JNZ", chunk, offset);
        default:
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}
