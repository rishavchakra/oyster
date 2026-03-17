#include "runtime.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "compiler.h"
#include "hashmap.h"

union Op run(struct OpList *code) {
    size_t pc = 0;
    bool is_heap_pc = false;

    struct Stack stack = stack_init();
    struct hashmap *binding_map =
        hashmap_new(sizeof(struct BindingMapEntry), 32, 0, 0, binding_hash,
                    binding_compare, NULL, NULL);

    OpErr err = op_err(UNKNOWN);

run:;
    union Op ip = get_ip(code, NULL, pc, is_heap_pc);
    enum OpType itype = op_type(ip);
    switch (itype) {
        case OPINSTR:
            if (op_instr_get(ip.instr) == DO) {
            } else {
                // DONE
                if (get_ip(code, NULL, pc + 1, is_heap_pc).raw == ~0) {
                    // End code
                    goto done;
                }
            }
            break;
        case OPBINDING:;
            union Op stack_top = stack_peek_op(&stack);
            if (stack_top.raw == op_instr(DO)) {
                // This binding will have type ANY, because it's in the code
                short bind = op_binding_get_ind(ip.binding);
                // Hashmap resolves that
                OpBinding binding = *(OpBinding *)hashmap_get(
                    binding_map, &(struct BindingMapEntry){.ind = bind});
                stack_push_op(&stack, (union Op)binding);

                goto run;
            } else if (op_type(stack_top) == OPERR) {
                err = stack_top.err;
                goto error;
            }
            // Otherwise, it's not to be used as a function
            // don't do it, just use it
            // pop off the stack as usual, etc.
            // fallthrough to same impl as everything else
        case OPNUM:
        case OPBOOL:
        case OPCHAR:
        case OPFNUM:;
            // TODO: should be used as an arg for func on the stack, not pushed
            // if pops off a DO, error
            uint8_t *oploc = stack_alloc(&stack, sizeof(union Op));
            memcpy(oploc, &ip, sizeof(union Op));
            pc++;
            goto run;
        case OPRAW:
            // Raws should only be used as arguments, not as instructions/vals.
            // We may encounter a raw disguised as a different type of Op.
            // So be it.
            err = op_err(UNEXPECTED_CODE_RAW);
            goto error;
            break;
        case OPERR:
            err = op_err(UNEXPECTED_CODE_ERR);
            break;
    }

error:
    stack_free(stack);
    hashmap_free(binding_map);
    return (union Op)err;

done:
    if (is_heap_pc) {
        // End of lambda
        pc = *(size_t *)stack_pop(&stack, sizeof(size_t));
        goto run;
    }
    // End of main execution context!
    return stack_pop_op(&stack);
}

union Op get_ip(struct OpList *code, uint8_t *heap, size_t pc, bool is_heap) {
    if (is_heap) {
        uint8_t *codeloc = heap + pc;
        return *(union Op *)codeloc;
    } else {
        return code->arr[pc];
    }
}

///////////////////////////////////////////////////
/**************** Stack functions ****************/
///////////////////////////////////////////////////

struct Stack stack_init() {
    uint8_t *buf = malloc(256);
    return (struct Stack){.buf = buf, .len = 0, .cap = 256};
}

void stack_free(struct Stack stack) { free(stack.buf); }

uint8_t *stack_alloc(struct Stack *stack, size_t size) {
    if (stack->len + size > stack->cap) {
        // Reallocate, with more space
        size_t dbl_len = stack->len * 2;
        size_t enough_len = stack->len + size;
        size_t new_cap = dbl_len > enough_len ? dbl_len : enough_len;
        uint8_t *new_buf = malloc(new_cap);
        uint8_t *old_buf = stack->buf;
        stack->buf = new_buf;
        free(old_buf);
        stack->cap = new_cap;
    }
    uint8_t *ret = stack->buf + stack->len;
    stack->len += size;
    return ret;
}

uint8_t *stack_pop(struct Stack *stack, size_t size) {
    if (stack->len < size) {
        printf("ERROR: Tried to pop past beginning of stack\n");
        exit(1);
    }
    stack->len -= size;
    return stack->buf + stack->len;
}

void stack_push_op(struct Stack *stack, union Op op) {
    if (stack->len + sizeof(union Op) > stack->cap) {
        size_t dbl_len = stack->len * 2;
        size_t enough_len = stack->len + sizeof(union Op);
        size_t new_cap = dbl_len > enough_len ? dbl_len : enough_len;
        uint8_t *new_buf = malloc(new_cap);
        uint8_t *old_buf = stack->buf;
        stack->buf = new_buf;
        free(old_buf);
        stack->cap = new_cap;
    }
    uint8_t *ptr = stack->buf + stack->len;
    *(union Op *)ptr = op;
    stack->len += sizeof(union Op);
}

union Op stack_peek_op(struct Stack *stack) {
    if (stack->len < sizeof(union Op)) {
        return (union Op)op_err(STACK_UNDERFLOW);
    }

    union Op *op_ptr = (union Op *)(stack->buf - sizeof(union Op));
    return *op_ptr;
}

union Op stack_pop_op(struct Stack *stack) {
    if (stack->len < sizeof(union Op)) {
        return (union Op)op_err(STACK_UNDERFLOW);
    }
    union Op *op_ptr = (union Op *)(stack->buf - sizeof(union Op));
    stack->len -= sizeof(union Op);
    return *op_ptr;
}

uint64_t binding_hash(const void *item, uint64_t seed0, uint64_t seed1) {
    return *(short *)item;
}

int binding_compare(const void *a, const void *b, void *udata) {
    return *(short *)a - *(short *)b;
}

union Op binding_deref(OpBinding binding, struct Stack *stack, uint8_t *heap,
                       uint8_t *statics) {
    enum BindingType btype = op_binding_get_type(binding);
    short bind = op_binding_get_ind(binding);
    switch (btype) {
        case BINDING_STACK:
            return ((union Op *)stack)[bind];
        case BINDING_HEAP:
            return *(union Op *)&heap[bind];
        case BINDING_STATIC:
            return *(union Op *)&heap[bind];
        case BINDING_NATIVE:
        case BINDING_HIDDEN:
        case BINDING_ANY:
        default:
            // We should not be able to dereference functions or unspecified ANY
            return (union Op)op_err(BAD_DEREF);
    }
}
