#include "compiler.h"

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct BindingList {
    char *buf;
    size_t len;
    size_t cap;
};

// Adds a binding string to the list, extending list if necessary
void binding_list_add(struct BindingList *list, const char *binding) {
    size_t binding_len = strlen(binding);
    if (list->len + binding_len + 1 >= list->cap) {
        size_t dbl_len = list->len * 2;
        size_t enough_len = list->len + binding_len + 2;
        size_t new_cap = dbl_len > enough_len ? dbl_len : enough_len;
        char *new_buf = malloc(new_cap);
        char *old_buf = list->buf;
        list->buf = new_buf;
        free(old_buf);
        list->cap = new_cap;
    }

    strcat(list->buf, " ");
    strcat(list->buf, binding);
    list->len += binding_len + 1;
}

// Returns the first instance of the binding in the list; if not present,
// extends the list and returns the index of the new location
size_t binding_find(struct BindingList *list, const char *binding) {
    size_t ind = 0;

    // First, check if this is matches the name of a native binding
    for (; ind < sizeof(native_fn_names) / sizeof(native_fn_names[0]); ++ind) {
        if (strcmp(binding, native_fn_names[ind]) == 0) {
            return ind;
        }
    }
    // Otherwise, it's some other defined binding. Check our own dynamic list
    char *buf = list->buf;
    const char *tok = buf;
    while (tok != NULL) {
        tok = strtok(buf, " ");
        buf = NULL;  // To force strtok to keep tracing through the buffer
        if (strcmp(binding, tok) == 0) {
            // Found the binding
            return ind;
        }
        ++ind;
    }
    // Did not find the binding in the list, so we add it and return that index
    binding_list_add(list, binding);
    return ind;
}

void compile_rec(struct AST *ast, struct OpList *ops,
                 struct BindingList *bindings) {
    switch (ast->val.type) {
        case AST_TOKEN:;
            size_t binding_ind = binding_find(bindings, ast->val.data.token);
            oplist_add(ops, (union Op){.binding = op_binding(BINDING_ANY,
                                                             binding_ind)});
            break;
        case AST_NUM:
            oplist_add(ops, (union Op){.num = op_num(ast->val.data.num)});
            break;
        case AST_FNUM:
            oplist_add(ops, (union Op){.fnum = op_num(ast->val.data.fnum)});
            break;
        case AST_BOOLEAN:
            oplist_add(ops,
                       (union Op){.boolean = op_bool(ast->val.data.boolean)});
            break;
        case AST_CHAR:
            oplist_add(
                ops, (union Op){.character = op_char(ast->val.data.character)});
            break;
        case AST_CHILDREN:
            // DO instr at the beginning, followed by the number of arguments
            oplist_add(ops, (union Op){.instr = op_instr(DO)});
            // Is it more useful to list the total number of opcodes here,
            // instead of the number of blocks?
            oplist_add(ops, (union Op){.raw = ast->val.data.children.len});

            // Recurse on the children
            for (int i = 0; i < ast->val.data.children.len; ++i) {
                struct AST *child = &ast->val.data.children.arr[i];
                compile_rec(child, ops, bindings);
            }

            // DONE instr at the end of the scope
            oplist_add(ops, (union Op){.instr = op_instr(DONE)});
            break;
    }
}

struct OpList compile(struct AST ast) {
    struct OpList ops = (struct OpList){
        .arr = malloc(sizeof(union Op) * 32),
        .len = 0,
        .cap = 32,
    };
    struct BindingList bindings = (struct BindingList){
        .buf = malloc(64),
        .len = 0,
        .cap = 64,
    };
    compile_rec(&ast, &ops, &bindings);
    // Add a -1 to mark code finish
    oplist_add(&ops, (union Op){.raw = ~0});
    return ops;
}

/////////////////////////////////////////////////////////////////////
/**************** Op Type definitions and functions ****************/
/////////////////////////////////////////////////////////////////////

OpNum op_num(int64_t val) {
    int64_t ret = val << 2;
    return (OpNum)(*((uint64_t *)&ret));
}

OpBool op_bool(bool val) {
    uint64_t ret;
    if (val) {
        ret = 1;
    } else {
        ret = 0;
    }
    return (OpBool)((ret << 32) | TAG_BOOL);
}

OpChar op_char(char val) { return (OpChar)(((uint64_t)val << 32) | TAG_CHAR); }

OpFnum op_fnum(double val) {
    float f = (float)val;
    uint32_t as_int = *((uint32_t *)&f);
    return (OpFnum)(((uint64_t)as_int << 32) | TAG_FNUM);
}

OpInstr op_instr(enum Instr instr) {
    return (OpInstr)(((uint64_t)instr << 32) | TAG_INSTR);
}

OpBinding op_binding(enum BindingType btype, unsigned short ind) {
    return (OpBinding)(((uint64_t)btype << 48) | ((uint64_t)ind << 32) |
                       TAG_BINDING);
}

OpErr op_err(enum RunErr err) {
    return (OpErr)(((uint64_t)err << 32) | TAG_ERR);
}

int64_t op_num_get(OpNum num) { return ((int64_t)num) >> 2; }

bool op_bool_get(OpBool val) { return (bool)(((uint64_t)val) >> 32); }

char op_char_get(OpChar val) { return (char)(((uint64_t)val) >> 32); }

float op_fnum_get(OpFnum val) {
    uint32_t masked = ((uint64_t)val) >> 32;
    return *((float *)&masked);
}

enum Instr op_instr_get(OpInstr instr) {
    return (enum Instr)(((uint64_t)instr) >> 32);
}

enum BindingType op_binding_get_type(OpBinding binding) {
    return (enum BindingType)(((uint64_t)binding) >> 48);
}

short op_binding_get_ind(OpBinding binding) {
    return (short)(((uint64_t)binding) >> 32);
}

enum RunErr op_err_get(OpErr err) {
    return (enum RunErr)(((uint64_t)err) >> 32);
}

enum OpType op_type(union Op op) {
    uint64_t tag = op.raw & ((1LL ^ 32) - 1);
    if ((tag & MASK_NUM) == TAG_NUM) {
        return OPNUM;
    }

    switch (tag) {
        case TAG_BOOL:
            return OPBOOL;
        case TAG_CHAR:
            return OPCHAR;
        case TAG_FNUM:
            return OPFNUM;
        case TAG_INSTR:
            return OPINSTR;
        case TAG_BINDING:
            return OPBINDING;
        case TAG_ERR:
            return OPERR;
        default:
            return OPRAW;
    }
}

void op_print(union Op op) {
    enum OpType type = op_type(op);
    switch (type) {
        case OPNUM:
            printf("NUM:\t%ld", op_num_get(op.num));
            break;
        case OPBOOL:
            printf("BOOL:\t%d", op_bool_get(op.boolean));
            break;
        case OPCHAR:
            printf("CHAR:\t%d", op_char_get(op.character));
            break;
        case OPFNUM:
            printf("FNUM:\t%f", op_fnum_get(op.fnum));
            break;
        case OPBINDING:
            printf("BIND:\t%d", op_binding_get_ind(op.binding));
            break;
        case OPINSTR:
            if (op_instr_get(op.instr) == DO) {
                printf("INST:\tDO");
            } else {
                printf("INSTR:\tDONE");
            }
            break;
        case OPERR:
            printf("ERR:\t%d", op_err_get(op.err));
            break;
        case OPRAW:
            break;
    }
    printf("\tRAW:\t%ld\n", op.raw);
}

void oplist_add(struct OpList *ops, union Op op) {
    if (ops->len == ops->cap) {
        union Op *new_arr = malloc(sizeof(union Op) * ops->cap * 2);
        memcpy(new_arr, ops->arr, sizeof(union Op) * ops->len);
        union Op *old_arr = ops->arr;
        ops->arr = new_arr;
        free(old_arr);
        ops->cap *= 2;
    }

    ops->arr[ops->len] = op;
    ++ops->len;
}
