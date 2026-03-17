#include <stdio.h>
#include <stdlib.h>

#include "compiler.h"
#include "hashmap.h"
#include "runtime.h"

void bindings_init_native_funcs(struct hashmap *map) {
    // Add all the native functions to the map
    short ind = 0;
    hashmap_set(
        map, &(struct BindingMapEntry){
                 .ind = FnAdd, .binding = op_binding(BINDING_NATIVE, FnAdd)});
    hashmap_set(
        map, &(struct BindingMapEntry){
                 .ind = FnSub, .binding = op_binding(BINDING_NATIVE, FnSub)});
    hashmap_set(
        map, &(struct BindingMapEntry){
                 .ind = FnAtoi, .binding = op_binding(BINDING_NATIVE, FnAtoi)});
    hashmap_set(
        map, &(struct BindingMapEntry){
                 .ind = FnDec, .binding = op_binding(BINDING_NATIVE, FnDec)});
    hashmap_set(map,
                &(struct BindingMapEntry){
                    .ind = FnIf, .binding = op_binding(BINDING_NATIVE, FnIf)});
    hashmap_set(
        map, &(struct BindingMapEntry){
                 .ind = FnInc, .binding = op_binding(BINDING_NATIVE, FnInc)});
    hashmap_set(
        map, &(struct BindingMapEntry){
                 .ind = FnItoa, .binding = op_binding(BINDING_NATIVE, FnItoa)});
    hashmap_set(map, &(struct BindingMapEntry){
                         .ind = FnLambda,
                         .binding = op_binding(BINDING_NATIVE, FnLambda)});
    hashmap_set(
        map, &(struct BindingMapEntry){
                 .ind = FnLet, .binding = op_binding(BINDING_NATIVE, FnLet)});
    hashmap_set(map, &(struct BindingMapEntry){
                         .ind = FnLetstar,
                         .binding = op_binding(BINDING_NATIVE, FnLetstar)});
    hashmap_set(map, &(struct BindingMapEntry){
                         .ind = FnLetrec,
                         .binding = op_binding(BINDING_NATIVE, FnLetrec)});
    hashmap_set(map, &(struct BindingMapEntry){
                         .ind = FnIsNull,
                         .binding = op_binding(BINDING_NATIVE, FnIsNull)});
    ++ind;
}

native_fn(native_inc) {}

native_fn(native_dec) {}

native_fn(native_add) {
    union Op last_ret = stack_pop_op(stack);

    // Single arg check
    if (code->arr[*pc + 1].raw == op_instr(DONE)) {
        switch (op_type(arg)) {
            case OPNUM:
            case OPBOOL:
            case OPFNUM:
                stack_push_op(stack, arg);
                break;
            default:
                printf("ERROR: Sub encountered bad first arg: ");
                op_print(arg);
                exit(1);
        }
        return;
    }

    int64_t a;
    float fa;
    int64_t b;
    float fb;
    bool a_isnum;
    bool b_isnum;
    switch (op_type(last_ret)) {
        case OPINSTR:
            if (op_instr_get(last_ret.instr) == DO) {
                a = 0;
                fa = 0;
                a_isnum = true;
                break;
            }
            goto error;
        case OPNUM:
            a = op_num_get(last_ret.num);
            fa = a;
            a_isnum = true;
            break;
        case OPCHAR:
            a = op_char_get(last_ret.character);
            fa = a;
            a_isnum = true;
            break;
        case OPFNUM:
            fa = op_fnum_get(last_ret.fnum);
            a = fa;
            a_isnum = false;
            break;
        default:
        error:
            // TODO: Better error handling, in line with the rest of the err
            // system
            printf("ERROR: Add encountered bad value: ");
            op_print(last_ret);
    }
    switch (op_type(arg)) {
        case OPNUM:
            b_isnum = true;
            b = op_num_get(arg.num);
            fb = b;
            break;
        case OPCHAR:
            b_isnum = true;
            b = op_char_get(arg.character);
            fb = b;
            break;
        case OPFNUM:
            b_isnum = false;
            fb = op_fnum_get(arg.fnum);
            b = fb;
            break;
        default:
            printf("ERROR: Add encountered bad arg value: ");
            op_print(arg);
            exit(1);
    }

    union Op ret;
    if (a_isnum && b_isnum) {
        ret = (union Op){.num = op_num(a + b)};
    } else {
        // One of them is a float, so both of them get converted to float
        ret = (union Op){.fnum = op_fnum(fa + fb)};
    }
    stack_push_op(stack, ret);
    stack_push_op(stack,
                  (union Op){.binding = op_binding(BINDING_NATIVE, FnAdd)});
}

native_fn(native_sub) {
    stack_pop_op(stack);
    if (code->arr[*pc + 1].raw == op_instr(DONE)) {
        // Single arg sub, just negate this
        switch (op_type(arg)) {
            case OPNUM:
                stack_push_op(stack,
                              (union Op){.num = op_num(-op_num_get(arg.num))});
                break;
            case OPBOOL:
                stack_push_op(
                    stack,
                    (union Op){.boolean = op_bool(!op_bool_get(arg.boolean))});
                break;
            case OPFNUM:
                stack_push_op(
                    stack, (union Op){.fnum = op_fnum(-op_fnum_get(arg.fnum))});
                break;
            default:
                printf("ERROR: Sub encountered bad first arg: ");
                op_print(arg);
        }
        return;
    }
    stack_push_op(stack, arg);
    stack_push_op(stack,
                  (union Op){.binding = op_binding(BINDING_NATIVE, FnSubArgs)});
}

native_fn(native_sub_args) {
    union Op last_ret = stack_pop_op(stack);
    int64_t a;
    float fa;
    int64_t b;
    float fb;
    bool a_isnum;
    bool b_isnum;
    switch (op_type(last_ret)) {
        case OPNUM:
            a = op_num_get(last_ret.num);
            fa = a;
            a_isnum = true;
            break;
        case OPCHAR:
            a = op_char_get(last_ret.character);
            fa = a;
            a_isnum = true;
            break;
        case OPFNUM:
            fa = op_fnum_get(last_ret.fnum);
            a = fa;
            a_isnum = false;
            break;
        default:
            // TODO: Better error handling, like with the rest of the err system
            printf("ERROR: Add encountered bad value: ");
            op_print(last_ret);
    }
    switch (op_type(arg)) {
        case OPNUM:
            b_isnum = true;
            b = op_num_get(arg.num);
            fb = b;
            break;
        case OPCHAR:
            b_isnum = true;
            b = op_char_get(arg.character);
            fb = b;
            break;
        case OPFNUM:
            b_isnum = false;
            fb = op_fnum_get(arg.fnum);
            b = fb;
            break;
        default:
            printf("ERROR: Add encountered bad arg value: ");
            op_print(arg);
    }

    union Op ret;
    if (a_isnum && b_isnum) {
        ret = (union Op){.num = op_num(a - b)};
    } else {
        // One of them is a float, so both of them get converted to float
        ret = (union Op){.fnum = op_fnum(fa - fb)};
    }
    stack_push_op(stack, ret);
    if (code->arr[*pc + 1].raw != op_instr(DONE)) {
        stack_push_op(stack, (union Op){.binding = op_binding(BINDING_NATIVE,
                                                              FnSubArgs)});
    }
}

native_fn(native_if) {
    // size_t do_level = 0;
    // do {
    //
    // } while (do_level > 0);
}

native_fn(native_let) {}

native_fn(native_letrec) {}

native_fn(native_letstar) {}

native_fn(native_lambda) {}

native_fn(native_atoi) {}

native_fn(native_itoa) {}

native_fn(native_is_null) {}

native_fn(native_ifdo) {}

native_fn(native_ifdont) {}
