#pragma once

#include "./common.h"
#include "./value.h"

typedef struct GC GC;

typedef enum
{
    // Constant and Literals
    PUSH_CONST,
    PUSH_NULL,
    PUSH_TRUE,
    PUSH_FALSE,
    DUP,
    DUP2,

    // Stack and Locals
    POP,
    LD_LOCAL,
    ST_LOCAL,
    LD_UPVAL,
    ST_UPVAL,

    // Globals
    LD_GLOBAL,
    LD_GLOBAL_I,
    DEF_GLOBAL,
    ST_GLOBAL,
    ST_GLOBAL_I,

    // Properties and Indexing
    LD_PROP,
    ST_PROP,
    LD_INDEX,
    ST_INDEX,
    LD_SUPER,

    // Arithmetic
    EQ,
    NEQ,
    GT,
    LT,
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,

    // Arithmetic _C
    EQ_C,
    NEQ_C,
    GT_C,
    LT_C,
    ADD_C,
    SUB_C,
    MUL_C,
    DIV_C,
    MOD_C,

    // Logic
    NOT,
    NEG,

    // Bitwise
    BAND,
    BOR,
    BXOR,
    BNOT,
    BSHL,
    BSHR,

    // Bitwise _C
    BAND_C,
    BOR_C,
    BXOR_C,
    BSHL_C,
    BSHR_C,

    // wtf is this?????
    DBG_PRINT,

    // Control Flow
    JMP,
    JMP_IF_FALSE,
    PJMP_IF_FALSE,
    JMP_IF_TRUE,
    PJMP_IF_TRUE,
    JMP_BACK,

    // Function Calls
    CALL,
    FAST_NATIVE_CALL,
    INVOKE,
    INVOKE_SUPER,
    RET,

    // Closures
    MAKE_CLOSURE,
    CLOSE_UPVAL,

    // Data Structures
    MAKE_LIST,
    LIST_DATA,
    MAKE_MAP,
    MAP_DATA,

    // Classes and Inheritance
    MAKE_CLASS,
    CLASS_INHERIT,
    DEF_METHOD,

    // Prefix/Postfix ops
    PRE_INC_LOCAL,
    PRE_DEC_LOCAL,
    POST_INC_LOCAL,
    POST_DEC_LOCAL,
    PRE_INC_GLOBAL,
    PRE_DEC_GLOBAL,
    POST_INC_GLOBAL,
    POST_DEC_GLOBAL,
    PRE_INC_GLOBAL_I,
    PRE_DEC_GLOBAL_I,
    POST_INC_GLOBAL_I,
    POST_DEC_GLOBAL_I,
    PRE_INC_UPVAL,
    PRE_DEC_UPVAL,
    POST_INC_UPVAL,
    POST_DEC_UPVAL,
    PRE_INC_PROP,
    PRE_DEC_PROP,
    POST_INC_PROP,
    POST_DEC_PROP,
    PRE_INC_INDEX,
    PRE_DEC_INDEX,
    POST_INC_INDEX,
    POST_DEC_INDEX,

    // microasm: typed immediate loads
    R_ILOAD_IMM,
    R_FLOAD_IMM,

    // microasm: fast register <-> local/global and ret
    R_LD_LOCAL_R,
    R_ST_LOCAL_R,
    R_LD_UPVAL_R,
    R_ST_UPVAL_R,
    R_LD_GLOBAL_R,
    R_ST_GLOBAL_R,
    R_RET_R,

    // microasm: scratch typed
    R_SST,
    R_SLD,

    // microasm: reg-conditional jumps
    R_JZ,
    R_JNZ,

    // microasm: typed ALU (int)
    R_IADD_RR, R_IADD_RI, R_IADD_IR, R_IADD_II,
    R_ISUB_RR, R_ISUB_RI, R_ISUB_IR, R_ISUB_II,
    R_IMUL_RR, R_IMUL_RI, R_IMUL_IR, R_IMUL_II,
    R_IDIV_RR, R_IDIV_RI, R_IDIV_IR, R_IDIV_II,
    R_IMOD_RR, R_IMOD_RI, R_IMOD_IR, R_IMOD_II,

    // microasm: typed ALU (float)
    R_FADD_RR, R_FADD_RI, R_FADD_IR, R_FADD_II,
    R_FSUB_RR, R_FSUB_RI, R_FSUB_IR, R_FSUB_II,
    R_FMUL_RR, R_FMUL_RI, R_FMUL_IR, R_FMUL_II,
    R_FDIV_RR, R_FDIV_RI, R_FDIV_IR, R_FDIV_II,
    R_FMOD_RR, R_FMOD_RI, R_FMOD_IR, R_FMOD_II,

    // microasm: bitwise (int64)
    R_IAND_RR, R_IAND_RI, R_IAND_IR, R_IAND_II,
    R_IOR_RR,  R_IOR_RI,  R_IOR_IR,  R_IOR_II,
    R_IXOR_RR, R_IXOR_RI, R_IXOR_IR, R_IXOR_II,

    // microasm: shifts (int64)
    // SHL = logical left, SHR = logical right (zero-fill), SAR = arithmetic right (sign-extend)
    R_ISHL_RR, R_ISHL_RI, R_ISHL_IR, R_ISHL_II,
    R_ISHR_RR, R_ISHR_RI, R_ISHR_IR, R_ISHR_II,
    R_ISAR_RR, R_ISAR_RI, R_ISAR_IR, R_ISAR_II,

    // microasm: rotates (int64)
    R_IROL_RR, R_IROL_RI, R_IROL_IR, R_IROL_II,
    R_IROR_RR, R_IROR_RI, R_IROR_IR, R_IROR_II,

    // microasm: bit test & modify (int64)
    // rbittst: dst = ((uint64)a >> (b&63)) & 1
    // rbitset: dst = a |  (1ULL << (b&63))
    // rbitclr: dst = a & ~(1ULL << (b&63))
    // rbittgl: dst = a ^  (1ULL << (b&63))
    R_IBTST_RR, R_IBTST_RI, R_IBTST_IR, R_IBTST_II,
    R_IBSET_RR, R_IBSET_RI, R_IBSET_IR, R_IBSET_II,
    R_IBCLR_RR, R_IBCLR_RI, R_IBCLR_IR, R_IBCLR_II,
    R_IBTGL_RR, R_IBTGL_RI, R_IBTGL_IR, R_IBTGL_II,

    // microasm: unary bitwise not (int64)
    R_IBNOT,

    // microasm: bound list (fast list index)
    R_LBIND_LOCAL,     // bind local  <regs: pos1=ilenDst or 0xF>[u16 slot]
    R_LBIND_UPVAL,     // bind upval  <regs: pos1=ilenDst or 0xF>[u16 idx]
    R_LBIND_GLOBAL,    // bind global <regs: pos1=ilenDst or 0xF>[u16 const(name)]

    R_LLEN,            // read current bound list length into i-reg <regs: pos1=dst>

    // safe (bounds/type) indexed access using bound list
    R_LLOAD_R,         // dstReg <- list[idx from iReg]     <regs: pos1=dst, pos2=iReg>
    R_LLOAD_I,         // dstReg <- list[idx immediate i64] <regs: pos1=dst>[imm64]
    R_LSTORE_R,        // list[idx from iReg] <- srcReg     <regs: pos1=src, pos2=iReg>
    R_LSTORE_I,        // list[idx immediate] <- srcReg     <regs: pos1=src>[imm64]

    // unsafe (no checks) indexed access using bound list
    R_LLOADU_R,        // dstReg <- list[idx from iReg]
    R_LLOADU_I,        // dstReg <- list[idx imm i64]
    R_LSTOREU_R,       // list[idx from iReg] <- srcReg
    R_LSTOREU_I,       // list[idx imm i64] <- srcReg

    // streaming (post-increment) variants
    R_LLOAD_PI,    // dst <- list[iReg];  iReg++
    R_LLOADU_PI,   // dst <- list[iReg];  iReg++   (unsafe)
    R_LSTORE_PI,   // list[iReg] <- src;  iReg++
    R_LSTOREU_PI,  // list[iReg] <- src;  iReg++   (unsafe)

    // microasm: registers
    R_PUSH,
    R_POP,
    R_MOV,

    // END
    MAX_OPCODES
} OpCode;

typedef struct
{
    int count;
    int capacity;
    uint16_t *code;
    int *lines;
    ValueArray constants;
} Chunk;

void initChunk(Chunk *chunk);
void freeChunk(GC *gc, Chunk *chunk);
void writeChunk(GC *gc, Chunk *chunk, uint16_t byte, int line);
int addConstant(GC *gc, Chunk *chunk, Value value);
int findConstant(Chunk *chunk, Value value);
