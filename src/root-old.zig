const std = @import("std");

const ParseStage = enum {
    start,
    string,
    num,
};

const AST = struct {
    val: Child,
    parent: ?*const AST,

    const Child = union(enum) {
        token: Token,
        tree: std.ArrayList(AST),
    };
    const Token = union(enum) {
        keyword,
        func,
        num: i64,
        float: f64,
    };
};

pub fn scheme_parse(text: []const u8, alloc: std.mem.Allocator) !AST {
    var text_ptr: usize = 0;
    var ast: AST = AST{ .parent = null, .val = AST.Child{ .tree = std.ArrayList(AST).empty } };

    var cur_ast: *AST = &ast;
    parse: switch (ParseStage.start) {
        .start => {
            if (text_ptr >= text.len) {
                break :parse;
            }
            start: switch (text[text_ptr]) {
                ' ' | '\t' | '\r' | '\n' => {
                    text_ptr += 1;
                    break :start;
                },
                '(' => {
                    text_ptr += 1;
                    continue :parse .string;
                },
                '0'...'9' => {
                    continue :parse .num;
                },
                else => break :parse,
            }
        },
        .string => {},
        .num => {
            var num_str = try std.ArrayList(u8).initCapacity(alloc, 5); // Arbitrary 5 idk
            var is_float = false;
            num: switch (text[text_ptr]) {
                '0'...'9' => {
                    try num_str.append(alloc, text[text_ptr]);
                    text_ptr += 1;
                    break :num;
                },
                '.' => {
                    try num_str.append(alloc, '.');
                    if (is_float) {
                        unreachable;
                    }
                    is_float = true;
                    text_ptr += 1;
                    break :num;
                },
                '_' => {
                    text_ptr += 1;
                    break :num;
                },
                else => break :parse,
            }
            const num_slice = try num_str.toOwnedSlice(alloc);
            if (is_float) {
                const parsed_num: i64 = try std.fmt.parseInt(i64, num_slice, 0);
                switch (cur_ast.val) {
                    .token => unreachable, // TODO: better error
                    .tree => {
                        try cur_ast.val.tree.append(alloc, AST{ .parent = cur_ast, .val = AST.Child{ .token = .{ .num = parsed_num } } });
                    },
                }
            } else {
                const parsed_num: f64 = try std.fmt.parseFloat(f64, num_slice);
                switch (cur_ast.val) {
                    .token => unreachable, // TODO: better error
                    .tree => {
                        try cur_ast.val.tree.append(alloc, AST{ .parent = cur_ast, .val = AST.Child{ .token = .{ .float = parsed_num } } });
                    },
                }
            }
            continue :parse .start;
        },
    }
    return ast;
}

test "parse whitespace" {
    const text: []const u8 = "\n\n\n\n";
    const ast = try scheme_parse(text, std.testing.allocator);
    try std.testing.expectEqualDeep(AST{ .parent = null, .val = AST.Child{ .tree = std.ArrayList(AST).empty } }, ast);
}

test "parse num" {
    const text: []const u8 = "53\n";
    const ast = try scheme_parse(text, std.testing.allocator);
    var tree = std.ArrayList(AST).empty;
    try tree.append(std.testing.allocator, AST{ .parent = &ast, .val = .{ .token = .{ .num = 53 } } });
    try std.testing.expectEqualDeep(AST{ .parent = null, .val = AST.Child{ .tree = tree } }, ast);
}
