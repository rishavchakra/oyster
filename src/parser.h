#ifndef __PARSER_H__
#define __PARSER_H__

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

enum ParseStage {
    STAGE_START,
    STAGE_TOKEN,
    STAGE_EXPR,
    STAGE_OCTOTHORPE,
};

struct ASTChildList {
    struct AST *arr;
    size_t len;
    size_t cap;
};

enum ASTNodeType {
    AST_TOKEN,
    AST_NUM,
    AST_FNUM,
    AST_BOOLEAN,
    AST_CHAR,
    AST_CHILDREN,
};

union ASTNodeData {
    char *token;
    int64_t num;
    double fnum;
    bool boolean;
    char character;
    char *str;
    struct ASTChildList children;
};

struct ASTNode {
    union ASTNodeData data;
    enum ASTNodeType type;
};

struct AST {
    struct AST *parent;
    struct ASTNode val;
};

void ast_node_print(struct ASTNode *);

struct ASTChildList ast_childlist_init();
void ast_childlist_free(struct ASTChildList *);
void ast_childlist_add(struct ASTChildList *, struct AST);

struct AST parse(const char *);

#endif
