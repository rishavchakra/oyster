#include "parser.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct AST parse(const char *text) {
    const char *text_ptr = text;
    struct AST ast = (struct AST){
        .parent = NULL,
        .val = (struct ASTNode){
            .data = (union ASTNodeData){.children = ast_childlist_init()},
            .type = AST_CHILDREN}};
    struct AST *cur_ast = &ast;
    enum ParseStage stage = STAGE_START;

parse:
    switch (stage) {
        case STAGE_START:
        start:
            switch (*text_ptr) {
                case ' ':
                case '\t':
                case '\r':
                case '\n':
                    ++text_ptr;
                    goto start;
                case '\0':
                    // End of input
                    goto finish;
                default:
                    stage = STAGE_EXPR;
                    goto parse;
            }

        case STAGE_TOKEN:
            char *end_ptr;
            errno = 0;
            int64_t as_num = strtoll(text_ptr, &end_ptr, 10);
            if (errno == 0) {
                // Successfully parsed int
                struct AST num_ast = (struct AST){
                    .parent = cur_ast,
                    .val = (struct ASTNode){
                        .type = AST_NUM,
                        .data = (union ASTNodeData){.num = as_num}}};
                ast_childlist_add(&cur_ast->val.data.children, num_ast);
                text_ptr = end_ptr;
                stage = STAGE_EXPR;
                goto parse;
            }
            errno = 0;
            double as_fnum = strtod(text_ptr, &end_ptr);
            if (errno == 0) {
                // Successfully parsed a double
                // do something with the double
                struct AST fnum_ast = (struct AST){
                    .parent = cur_ast,
                    .val = (struct ASTNode){
                        .type = AST_FNUM,
                        .data = (union ASTNodeData){.fnum = as_fnum}}};
                ast_childlist_add(&cur_ast->val.data.children, fnum_ast);
                text_ptr = end_ptr;
                stage = STAGE_EXPR;
                goto parse;
            }

            const char *token_start = text_ptr;
        token:
            switch (*text_ptr) {
                case 'a' ... 'z':
                case 'A' ... 'Z':
                case '0' ... '9':
                case '+':
                case '-':
                case '*':
                case '_':
                case '=':
                    ++text_ptr;
                    goto token;
                default:
                    break;
            }
            size_t strlen = text_ptr - token_start;
            char *const str = malloc(strlen + 1);
            memcpy(str, token_start, strlen);
            str[strlen] = '\0';
            struct AST token_ast =
                (struct AST){.parent = cur_ast,
                             .val = (struct ASTNode){
                                 .type = AST_TOKEN,
                                 .data = (union ASTNodeData){.token = str}}};
            ast_childlist_add(&cur_ast->val.data.children, token_ast);

            stage = STAGE_EXPR;
            goto parse;

        case STAGE_EXPR:
        expr:
            switch (*text_ptr) {
                case ' ':
                case '\t':
                case '\r':
                case '\n':
                    ++text_ptr;
                    goto expr;
                case 'a' ... 'z':
                case 'A' ... 'Z':
                case '0' ... '9':
                case '+':
                case '-':
                case '*':
                case '_':
                case '=':
                    stage = STAGE_TOKEN;
                    goto parse;
                case '(':
                    struct ASTChildList list = ast_childlist_init();
                    struct AST childlist_ast = (struct AST){
                        .parent = cur_ast,
                        .val = (struct ASTNode){
                            .type = AST_CHILDREN,
                            .data = (union ASTNodeData){.children = list}}};
                    ast_childlist_add(&cur_ast->val.data.children,
                                      childlist_ast);
                    ++text_ptr;
                    goto expr;
                case ')':
                    cur_ast = cur_ast->parent;
                    // TODO: handle random closing parentheses (error)
                    ++text_ptr;
                    goto expr;
                case '#':
                    ++text_ptr;
                    stage = STAGE_OCTOTHORPE;
                    goto parse;
                case '\0':
                    goto finish;
                default:
                    printf("ERROR: Unexpected character in expression: %d\n",
                           *text_ptr);
                    goto finish;
            }
            break;

        case STAGE_OCTOTHORPE:
            if (*text_ptr != '\\') {
                printf("ERROR: Unexpected character after octothorpe: %d\n",
                       *text_ptr);
                goto finish;
            }
            switch (*text_ptr) {
                case 32 ... 127:
                    struct AST char_ast = (struct AST){
                        .parent = cur_ast,
                        .val = (struct ASTNode){.type = AST_CHAR,
                                                .data = (union ASTNodeData){
                                                    .character = *text_ptr}}};
                    ast_childlist_add(&cur_ast->val.data.children, char_ast);
                    break;
                default:
                    printf(
                        "ERROR: Unexpected octothorpe-defined character: %d\n",
                        *text_ptr);
                    goto finish;
            }
            text_ptr += 2;
            stage = STAGE_EXPR;
            goto parse;
    }

finish:
    return ast;
}

void ast_node_print(struct ASTNode *node) {}

struct ASTChildList ast_childlist_init() {
    struct ASTChildList ret;
    ret.len = 0;
    ret.cap = 8;
    struct AST *arr = malloc(sizeof(struct AST) * 8);
    ret.arr = arr;
    return ret;
}

void ast_childlist_free(struct ASTChildList *list) { free(list->arr); }

void ast_childlist_add(struct ASTChildList *list, struct AST ast) {
    if (list->len == list->cap) {
        // Grow the array
        struct AST *new_arr = malloc(sizeof(struct AST) * list->cap * 2);
        memcpy(new_arr, list->arr, sizeof(struct AST) * list->cap);
        struct AST *old_arr = list->arr;
        list->arr = new_arr;
        free(old_arr);

        list->cap *= 2;
    }

    list->arr[list->len] = ast;
    ++list->len;
}
