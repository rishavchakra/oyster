#include "compiler.h"

#include <stdbool.h>
#include <stdio.h>

#include "parser.h"

void test(bool pred, size_t *succ, size_t *fail) {
    if (pred) {
        (*succ)++;
    } else {
        (*fail)++;
    }
}

int main() {
    size_t success = 0;
    size_t failed = 0;

    {
        struct AST ast = parse("5");
        struct OpList ops = compile(ast);
        test(ops.len == 5, &success, &failed);
    }

    printf(
        "========COMPILER TEST RESULTS========\n"
        "%ld/%ld Succeeded\n"
        "%ld/%ld Failed\n"
        "=====================================\n",
        success, success + failed, failed, success + failed);
    return failed;
}
