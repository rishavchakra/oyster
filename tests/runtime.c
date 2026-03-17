#include "runtime.h"

#include <stdbool.h>
#include <stdio.h>

#include "compiler.h"
#include "parser.h"

void test(bool pred, size_t *succ, size_t *fail) {
    if (pred) {
        (*succ)++;
    } else {
        (*fail)++;
    }
}

int main() {
    size_t succ = 0;
    size_t fail = 0;

    {
        struct AST ast = parse("5");
        struct OpList ops = compile(ast);
        union Op res = run(&ops);
        test(res.raw == ((union Op)op_num(5)).raw, &succ, &fail);
    }

    // {
    //     struct AST ast = parse("(+ 3 2)");
    //     struct OpList ops = compile(ast);
    //     union Op res = run(&ops);
    //     test(res.raw == ((union Op)op_num(5)).raw, &succ, &fail);
    // }

    printf(
        "========RUNTIME TEST RESULTS========\n"
        "%ld/%ld Succeeded\n"
        "%ld/%ld Failed\n"
        "=====================================\n",
        succ, succ + fail, fail, succ + fail);
    return fail;
}
