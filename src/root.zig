const std = @import("std");

const ParseStage = enum {
    start,
    token,
    expr,
};

const AST = struct {
    val: Node,
    parent: ?*const AST,

    const Node = union(enum) {
        token: Token,
        children: std.ArrayList(AST),
    };
    const Token = union(enum) {
        binding: []const u8,
        num: i64,
        float: f64,
        str: []const u8,
    };

    fn print(self: *const AST) void {
        switch (self.val) {
            .token => |tok| {
                switch (tok) {
                    .binding => |binding_name| {
                        std.debug.print("BINDING:{s}\n", .{binding_name});
                    },
                    .num => |num| {
                        std.debug.print("INT:\t{d}\n", .{num});
                    },
                    .float => |f| {
                        std.debug.print("FLOAT:\t{d}\n", .{f});
                    },
                    .str => |s| {
                        std.debug.print("{s}\n", .{s});
                    },
                }
            },
            .children => |children| {
                std.debug.print("<\n", .{});
                for (children.items) |child| {
                    print(&child);
                }
                std.debug.print(">\n", .{});
            },
        }
    }

    fn deinit(self: *AST, alloc: std.mem.Allocator) void {
        switch (self.val) {
            .children => |children| {
                for (children.items) |child| {
                    @constCast(&child).deinit(alloc);
                }
                @constCast(&children).deinit(alloc);
            },
            .token => |tok| {
                switch (tok) {
                    .binding => |b| {
                        alloc.free(b);
                    },
                    .str => |str| {
                        alloc.free(str);
                    },
                    else => {},
                }
            },
        }
    }
};

pub fn scheme_parse(text: [:0]const u8, alloc: std.mem.Allocator) !AST {
    var text_ptr: usize = 0;
    var ast: AST = AST{ .parent = null, .val = AST.Node{ .children = try std.ArrayList(AST).initCapacity(alloc, 8) } };
    var cur_ast: *AST = &ast;

    parse: switch (ParseStage.start) {
        .start => {
            start: switch (text[text_ptr]) {
                ' ', '\t', '\r', '\n' => {
                    text_ptr += 1;
                    continue :start text[text_ptr];
                },
                '(' => {
                    text_ptr += 1;
                    continue :parse .expr;
                },
                0 => {
                    break :parse;
                },
                else => {
                    continue :parse .expr;
                },
            }
        },
        .token => {
            // Arbitrary starting with 16 as binding name length
            var chars_list = try std.ArrayList(u8).initCapacity(alloc, 16);
            defer chars_list.deinit(alloc);
            var is_num = true;
            var is_float = false;
            token: switch (text[text_ptr]) {
                'a'...'z', 'A'...'Z', '+', '-', '=', '_' => |char| {
                    is_num = false;
                    try chars_list.append(alloc, char);
                    text_ptr += 1;
                    continue :token text[text_ptr];
                },
                '0'...'9' => |char| {
                    try chars_list.append(alloc, char);
                    text_ptr += 1;
                    continue :token text[text_ptr];
                },
                '.' => |char| {
                    is_float = true;
                    try chars_list.append(alloc, char);
                    text_ptr += 1;
                    continue :token text[text_ptr];
                },
                ' ', ')' => {
                    text_ptr += 1;
                    break :token;
                },
                0 => {
                    break :token;
                },
                else => {
                    std.debug.print("Unexpected function call: {c}\n", .{text[text_ptr]});
                },
            }
            const str = try chars_list.toOwnedSlice(alloc);
            if (is_num) {
                if (is_float) {
                    const float = try std.fmt.parseFloat(f64, str);
                    try cur_ast.val.children.append(alloc, AST{ .parent = cur_ast, .val = AST.Node{ .token = .{ .float = float } } });
                } else {
                    const num = try std.fmt.parseInt(i64, str, 10);
                    try cur_ast.val.children.append(alloc, AST{ .parent = cur_ast, .val = AST.Node{ .token = .{ .num = num } } });
                }
                alloc.free(str);
            } else {
                try cur_ast.val.children.append(alloc, AST{ .parent = cur_ast, .val = AST.Node{ .token = .{ .binding = str } } });
            }
            continue :parse .expr;
        },
        .expr => {
            expr: switch (text[text_ptr]) {
                '(' => {
                    // Beginning of new sub-expression
                    const child_list = try std.ArrayList(AST).initCapacity(alloc, 8);
                    var node = AST{ .parent = cur_ast, .val = AST.Node{ .children = child_list } };
                    try cur_ast.val.children.append(alloc, node);
                    cur_ast = &node;
                    continue :expr text[text_ptr];
                },
                ')' => {
                    // End of expression
                    cur_ast = @constCast(cur_ast.parent.?);
                    continue :expr text[text_ptr];
                },
                'a'...'z', 'A'...'Z', '0'...'9', '+', '-', '=', '_' => {
                    continue :parse .token;
                },
                0 => {
                    // EOF
                    break :parse;
                },
                else => {
                    std.debug.print("Unexpected character in expression: {c}\n", .{text[text_ptr]});
                },
            }
        },
    }

    return ast;
}

test "parse newlines" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "\n\n\n\n";
    var ast = try scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    ast.print();
    // try std.testing.expectEqualDeep(AST{ .parent = null, .val = AST.Child{ .tree = std.ArrayList(AST).initCapacity(alloc, 8) } }, ast);
}

test "parse number" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "5";
    var ast = try scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    ast.print();
    // try std.testing.expectEqualDeep(AST{ .parent = null, .val = AST.Child{ .tree = std.ArrayList(AST).initCapacity(alloc, 8) } }, ast);
}

test "parse float" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "3.2";
    var ast = try scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    ast.print();
    // try std.testing.expectEqualDeep(AST{ .parent = null, .val = AST.Child{ .tree = std.ArrayList(AST).initCapacity(alloc, 8) } }, ast);
}

test "parse binding" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "varname";
    var ast = try scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    ast.print();
    // try std.testing.expectEqualDeep(AST{ .parent = null, .val = AST.Child{ .tree = std.ArrayList(AST).initCapacity(alloc, 8) } }, ast);
}

test "parse let" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(let ((a 3)) a)";
    var ast = try scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    ast.print();
    // try std.testing.expectEqualDeep(AST{ .parent = null, .val = AST.Child{ .tree = std.ArrayList(AST).initCapacity(alloc, 8) } }, ast);
}

test "parse expr" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(funcname 5 3)";
    var ast = try scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    ast.print();
    // try std.testing.expectEqualDeep(AST{ .parent = null, .val = AST.Child{ .tree = std.ArrayList(AST).initCapacity(alloc, 8) } }, ast);
}
