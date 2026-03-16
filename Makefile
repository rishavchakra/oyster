CC := gcc
CFLAGS := -Wall -Werror -g -std=c23

BUILD_DIR := ./build
SRC_DIR := ./src
TEST_DIR := ./tests
TEST_BUILD_DIR := ./$(BUILD_DIR)/test

SRCS := $(shell find $(SRC_DIR) -name '*.c')
OBJS := $(SRCS:%=$(BUILD_DIR)/%.o)

P_O := $(BUILD_DIR)/parser.o
C_O := $(BUILD_DIR)/compiler.o
R_O := $(BUILD_DIR)/runtime.o
H_O := $(BUILD_DIR)/hashmap.o
F_O := $(BUILD_DIR)/functions.o

.PHONY: all clean fmt

all: parser compiler runtime hashmap parser_test compiler_test runtime_test

test: parser_test compiler_test runtime_test
	@$(TEST_BUILD_DIR)/parser
	@$(TEST_BUILD_DIR)/compiler
	@$(TEST_BUILD_DIR)/runtime

parser: $(SRC_DIR)/parser.c
	@mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $(SRC_DIR)/parser.c -o $(P_O)

compiler: $(SRC_DIR)/compiler.c
	@mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $(SRC_DIR)/compiler.c -o $(C_O)

runtime: $(SRC_DIR)/runtime.c
	@mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $(SRC_DIR)/runtime.c -o $(R_O)

hashmap: $(SRC_DIR)/hashmap.c
	@mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $(SRC_DIR)/hashmap.c -o $(H_O)

funcs: $(SRC_DIR)/functions.c
	@mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $(SRC_DIR)/functions.c -o $(F_O)

parser_test: parser $(TEST_DIR)/parser.c
	@mkdir -p $(TEST_BUILD_DIR)
	$(CC) $(CFLAGS) -I$(SRC_DIR) $(TEST_DIR)/parser.c $(P_O) -o $(TEST_BUILD_DIR)/parser

compiler_test: parser compiler $(TEST_DIR)/compiler.c
	@mkdir -p $(TEST_BUILD_DIR)
	$(CC) $(CFLAGS) -I$(SRC_DIR) $(TEST_DIR)/compiler.c $(P_O) $(C_O) -o $(TEST_BUILD_DIR)/compiler

runtime_test: parser compiler runtime hashmap funcs $(TEST_DIR)/runtime.c
	@mkdir -p $(TEST_BUILD_DIR)
	$(CC) $(CFLAGS) -I$(SRC_DIR) $(TEST_DIR)/runtime.c $(P_O) $(C_O) $(R_O) $(H_O) $(F_O) -o $(TEST_BUILD_DIR)/runtime

fmt:
	clang-format -style=file -i src/*
	clang-format -style=file -i tests/*

clean:
	rm -r $(BUILD_DIR)
