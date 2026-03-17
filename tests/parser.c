#include "parser.h"

#include <stdbool.h>
#include <stdio.h>

void test(bool pred, size_t *succ, size_t *fail) {
    if (pred) {
        (*succ)++;
    } else {
        (*fail)++;
    }
}

int main() {
    // size_t succ = 0;
    // size_t fail = 0;

    {
        struct AST ast = parse("5");
        ast_node_print(&ast.val);
        ast_node_print(&ast.val.data.children.arr[0].val);
    }
    {
        struct AST ast = parse("(5)");
        ast_node_print(&ast.val);
        ast_node_print(
            &ast.val.data.children.arr[0].val.data.children.arr[0].val);
    }
    {
        struct AST ast = parse("(inc 5)");
        ast_node_print(&ast.val);
        ast_node_print(
            &ast.val.data.children.arr[0].val.data.children.arr[0].val);
    }
    {
        struct AST ast = parse("(+ (+ 2 2) 3 5)");
        ast_node_print(&ast.val);
        ast_node_print(
            &ast.val.data.children.arr[0].val.data.children.arr[0].val);
    }
}
