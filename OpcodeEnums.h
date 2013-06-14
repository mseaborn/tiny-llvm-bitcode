#ifndef OPCODE_ENUMS_H_
#define OPCODE_ENUMS_H_

namespace Opcodes {
  enum {
    // Put i32 as 0 since it is the most commonly used type.
    TYPE_I32,
    TYPE_I1,
    TYPE_I8,
    TYPE_I16,
    TYPE_I64,
    TYPE_VOID,
    TYPE_FLOAT,
    TYPE_DOUBLE,
  };

  enum InstOpcode {
    INST_RET_VOID,
    INST_RET_VALUE,
    INST_LOAD,
    INST_STORE,
    INST_ATOMICRMW, // TODO: Use a higher opcode, since this is rare
    INST_ALLOCA_FIXED,
    INST_ALLOCA_VARIABLE,
    INST_CALL,
    INST_BR_UNCOND,
    INST_BR_COND,
    INST_SWITCH,
    INST_UNREACHABLE,
    INST_SELECT,
    INST_PHI,
    // Binary operators
#define HANDLE_BINARY_INST(LLVM_OP, WIRE_OP) WIRE_OP,
#include "Instructions.def"
#undef HANDLE_BINARY_INST
    INST_CMP,
    INST_CAST,
    // Pseudo-instructions.
    // FWD_REF(TYPE) creates a placeholder for a forward reference.
    INST_FWD_REF,
    // FWD_DEF(N) replaces the Nth FWD_REF placeholder with the
    // previous instruction.
    INST_FWD_DEF,
    INST_CONSTANT_INT,
    INST_CONSTANT_FLOAT,
    INST_CONSTANT_DOUBLE,
    INST_CONSTANT_UNDEFVALUE,
  };
};

#endif
