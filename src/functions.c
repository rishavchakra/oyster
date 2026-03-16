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
native_fn(native_add) {}
native_fn(native_sub) {}
native_fn(native_if) {}
native_fn(native_let) {}
native_fn(native_letrec) {}
native_fn(native_letstar) {}
native_fn(native_lambda) {}
native_fn(native_atoi) {}
native_fn(native_itoa) {}
native_fn(native_is_null) {}
