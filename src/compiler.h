#ifndef __COMPILER_H__
#define __COMPILER_H__

#include <stddef.h>
#include <stdint.h>

#include "parser.h"

#define TAG_NUM 0b00
#define MASK_NUM 0b11
#define TAG_BOOL 0b111
#define TAG_CHAR 0b1011
#define TAG_FNUM 0b1111
#define TAG_INSTR 0b10011
#define TAG_BINDING 0b10111
#define TAG_ERR 0b11011

enum Instr {
    DO,
    DONE,
};
enum BindingType {
    // Bindings placed on the stack
    BINDING_STACK,
    // Bindings allocated on the heap
    BINDING_HEAP,
    // Bindings allocated in static memory, passed from compiler to runtime
    BINDING_STATIC,
    // Bindings to native functions
    BINDING_NATIVE,
    // Bindings to functions used by native functions,
    // which should not be callable on their own (e.g. ifdo)
    BINDING_HIDDEN,
    // Bindings to unknown locations. The compiler uses this to mark a token as
    // a binding, which will be resolved into one of the above types at runtime.
    BINDING_ANY,
};
enum RunErr {
    UNEXPECTED_CODE_RAW,
    UNEXPECTED_CODE_ERR,
    STACK_UNDERFLOW,
    BAD_DEREF,
    UNSPECIFIED,  // Unspecified (the value)
    UNKNOWN,
};

typedef uint64_t OpNum;
typedef uint64_t OpBool;
typedef uint64_t OpChar;
typedef uint64_t OpFnum;
typedef uint64_t OpInstr;
typedef uint64_t OpBinding;
typedef uint64_t OpErr;
union Op {
    OpNum num;
    OpBool boolean;
    OpChar character;
    OpFnum fnum;
    OpInstr instr;
    OpBinding binding;
    OpErr err;
    uint64_t raw;
};
enum OpType {
    OPNUM,
    OPBOOL,
    OPCHAR,
    OPFNUM,
    OPBINDING,
    OPINSTR,
    OPERR,
    OPRAW,
};

OpNum op_num(int64_t);
OpBool op_bool(bool);
OpChar op_char(char);
OpFnum op_fnum(double);
OpInstr op_instr(enum Instr);
OpBinding op_binding(enum BindingType, unsigned short ind);
OpErr op_err(enum RunErr);

int64_t op_num_get(OpNum);
bool op_bool_get(OpBool);
char op_char_get(OpChar);
float op_fnum_get(OpFnum);
enum Instr op_instr_get(OpInstr);
enum BindingType op_binding_get_type(OpBinding);
short op_binding_get_ind(OpBinding);
enum RunErr op_err_get(OpErr);

enum OpType op_type(union Op);
void op_print(union Op);

struct OpList {
    union Op *arr;
    size_t len;
    size_t cap;
};
void oplist_add(struct OpList *, union Op);

struct OpList compile(struct AST);

// This is the canonical order of the native functions (alphabetical),
// with hidden functions coming after available bindings.
// The runtime initialization of native bindings should contain available
// elements in this same order.
enum NativeFns {
    FnAdd,
    FnSub,
    FnAtoi,
    FnDec,
    FnIf,
    FnInc,
    FnItoa,
    FnLambda,
    FnLet,
    FnLetstar,
    FnLetrec,
    FnIsNull,
};
// clang-format off
static char const * const native_fn_names[] = {
    "+", // add
    "-", // sub
    "atoi",
    "dec",
    "if",
    "inc",
    "itoa",
    "lambda",
    "let",
    "let*",
    "letrec",
    "null?", // is_null
};
// clang-format on

#endif
