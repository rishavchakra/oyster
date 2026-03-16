#ifndef __RUNTIME_H__
#define __RUNTIME_H__

#include <stddef.h>
#include <stdint.h>

#include "compiler.h"

union Op run(struct OpList *);

union Op get_ip(struct OpList *code, uint8_t *heap, size_t pc, bool is_heap);

struct Stack {
    uint8_t *buf;
    size_t len;
    size_t cap;
};

struct Stack stack_init();
void stack_free(struct Stack);
// Returns a pointer to the top of the newly allocated memory
uint8_t *stack_alloc(struct Stack *, size_t);
// Returns a pointer to the top n bytes, and decreases the length pointer
uint8_t *stack_pop(struct Stack *, size_t);
// Pushes an Op to the top of the stack
void stack_push_op(struct Stack *, union Op);
// Interprets the top of the stack as an Op and returns it
union Op stack_peek_op(struct Stack *);
union Op stack_pop_op(struct Stack *);

// Binding to be put on the stack
struct Binding {
    // The index of the binding, into section specified by type
    size_t ind;
    enum BindingType type;
    // The index of this binding's previous value, on the stack
    uint32_t prev_binding;
};

// Used for the binding hash map
struct BindingMapEntry {
    // index of the binding, used as the key in the hashmap
    short ind;
    // the binding pointer, used as the value in the hashmap
    OpBinding binding;
};
uint64_t binding_hash(const void *item, uint64_t seed0, uint64_t seed1);
int binding_compare(const void *a, const void *b, void *udata);
void bindings_init_native_funcs(struct hashmap *);
union Op binding_deref(OpBinding, struct Stack *, uint8_t *heap,
                       uint8_t *statics);

////////////////////////////////////////////////////////////////
/**************** NATIVE FUNCTION DECLARATIONS ****************/
////////////////////////////////////////////////////////////////

#define native_fn(NAME)                                 \
    void NAME(struct OpList *code, struct Stack *stack, \
              struct hashmap *binding_map, size_t *pc, bool *is_heap_pc)

// Non-canonical order, declarations don't matter
native_fn(native_add);
native_fn(native_atoi);
native_fn(native_dec);
native_fn(native_if);
native_fn(native_inc);
native_fn(native_is_null);
native_fn(native_itoa);
native_fn(native_lambda);
native_fn(native_let);
native_fn(native_letrec);
native_fn(native_letstar);
native_fn(native_sub);

#endif
