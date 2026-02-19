const std = @import("std");

const ParseStage = enum {
    start,
    token,
    expr,
    quot,
    dbl_quot,
    octothorpe,
};

pub const AST = struct {
    val: Node,
    parent: ?*const AST,

    pub const Node = union(enum) {
        binding: []const u8,
        num: i64,
        float: f64,
        boolean: bool,
        char: u8,
        str: []const u8,
        // Either a leaf (token) or a list of children
        children: std.ArrayList(AST),

        pub fn print(self: Node) void {
            switch (self) {
                .binding => |binding_name| {
                    std.debug.print("BINDING:{s}\n", .{binding_name});
                },
                .num => |num| {
                    std.debug.print("INT:\t{d}\n", .{num});
                },
                .float => |f| {
                    std.debug.print("FLOAT:\t{d}\n", .{f});
                },
                .boolean => |b| {
                    std.debug.print("BOOL:\t{any}\n", .{b});
                },
                .char => |c| {
                    std.debug.print("CHAR:\t{d}\n", .{c});
                },
                .str => |s| {
                    std.debug.print("STRING :\t{s}\n", .{s});
                },
                .children => {
                    std.debug.print("PARENT NODE\n", .{});
                },
            }
        }
    };

    pub fn print(self: *const AST) void {
        self.print_rec(0);
    }

    fn print_rec(self: *const AST, level: u32) void {
        for (0..level) |_| {
            std.debug.print("\t", .{});
        }
        switch (self.val) {
            .binding => |binding_name| {
                std.debug.print("BINDING:{s}\n", .{binding_name});
            },
            .num => |num| {
                std.debug.print("INT:\t{d}\n", .{num});
            },
            .float => |f| {
                std.debug.print("FLOAT:\t{d}\n", .{f});
            },
            .boolean => |b| {
                std.debug.print("BOOL:\t{any}\n", .{b});
            },
            .char => |c| {
                std.debug.print("CHAR:\t{d}\n", .{c});
            },
            .str => |s| {
                std.debug.print("STRING :\t{s}\n", .{s});
            },
            .children => |children| {
                std.debug.print("<\n", .{});
                for (children.items) |child| {
                    child.print_rec(level + 1);
                }

                for (0..level) |_| {
                    std.debug.print("\t", .{});
                }
                std.debug.print(">\n", .{});
            },
        }
    }

    pub fn deinit(self: *AST, alloc: std.mem.Allocator) void {
        switch (self.val) {
            .children => |children| {
                for (children.items) |child| {
                    @constCast(&child).deinit(alloc);
                }
                @constCast(&children).deinit(alloc);
            },
            .binding => |b| {
                alloc.free(b);
            },
            .str => |str| {
                alloc.free(str);
            },
            else => {},
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
                'a'...'z', 'A'...'Z', '+', '-', '=', '_', '*' => |char| {
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
                // '\'' => {
                //     if (text_ptr != start_text_ptr) {
                //         // This is a problem. Chars and quote strings require ' to be the first char
                //         std.debug.print("ERROR: Encountered ' in middle of token\n", .{});
                //     }
                //     text_ptr += 1;
                //     continue :parse .quot;
                // },
                ' ', '\t', '\r', '\n' => {
                    // End of token: save string as token and continue
                    text_ptr += 1;
                    break :token;
                },
                ')' => {
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
                    try cur_ast.val.children.append(alloc, AST{ .parent = cur_ast, .val = AST.Node{ .float = float } });
                } else {
                    const num = try std.fmt.parseInt(i64, str, 10);
                    try cur_ast.val.children.append(alloc, AST{ .parent = cur_ast, .val = AST.Node{ .num = num } });
                }
                // String not used later, only the parsed number value
                alloc.free(str);
            } else {
                try cur_ast.val.children.append(alloc, AST{ .parent = cur_ast, .val = AST.Node{ .binding = str } });
            }
            continue :parse .expr;
        },
        .quot => {
            var chars_list = try std.ArrayList(u8).initCapacity(alloc, 2);
            defer chars_list.deinit(alloc);
            var num_chars: usize = 0;
            quot: switch (text[text_ptr]) {
                '\'' => {
                    // End quote
                    text_ptr += 1;
                    break :quot;
                },
                32...38, 40...127 => |c| {
                    // Non-control, non-quote characters
                    try chars_list.append(alloc, c);
                    text_ptr += 1;
                    num_chars += 1;
                    continue :quot text[text_ptr];
                },
                0 => {
                    break :quot;
                },
                else => unreachable, // Not possible with ASCII characters
            }

            if (num_chars == 1) {
                // Found a single character
                try cur_ast.val.children.append(alloc, AST{ .parent = cur_ast, .val = AST.Node{ .char = chars_list.items[0] } });
            } else {
                // We have a quot expression. Handle this later, it's annoying
            }
            continue :parse .expr;
        },
        .dbl_quot => {
            var chars_list = try std.ArrayList(u8).initCapacity(alloc, 8);
            defer chars_list.deinit(alloc);
            quot: switch (text[text_ptr]) {
                '"' => {
                    text_ptr += 1;
                    break :quot;
                },
                32...33, 35...127 => |c| {
                    // Non-control, non-quote characters
                    try chars_list.append(alloc, c);
                    text_ptr += 1;
                    continue :quot text[text_ptr];
                },
                0 => {
                    break :quot;
                },
                else => unreachable, // TODO: proper error handling
            }
            try cur_ast.val.children.append(alloc, AST{ .parent = cur_ast, .val = AST.Node{ .str = try chars_list.toOwnedSlice(alloc) } });
            continue :parse .expr;
        },
        .octothorpe => {
            // Handle #\a type character parsing
            if (text[text_ptr] != '\\') {
                // Don't know how to handle this yet
                unreachable; // TODO: better error handling
            }
            switch (text[text_ptr + 1]) {
                32...127 => |c| {
                    try cur_ast.val.children.append(alloc, AST{ .parent = cur_ast, .val = AST.Node{ .char = c } });
                },
                else => unreachable, // TODO: proper error handling
            }
            if (std.ascii.isWhitespace(text[text_ptr + 2])) {
                text_ptr += 3;
            } else {
                text_ptr += 2; // TODO: Better error handling for things that aren't supposed to be there
            }
            continue :parse .expr;
        },
        .expr => {
            expr: switch (text[text_ptr]) {
                ' ', '\t', '\r', '\n' => {
                    text_ptr += 1;
                    continue :expr text[text_ptr];
                },
                '(' => {
                    // Beginning of new sub-expression
                    const child_list = try std.ArrayList(AST).initCapacity(alloc, 8);
                    // var node = AST{ .parent = cur_ast, .val = AST.Node{ .children = child_list } };
                    try cur_ast.val.children.append(alloc, AST{ .parent = cur_ast, .val = AST.Node{ .children = child_list } });
                    cur_ast = &cur_ast.val.children.items[cur_ast.val.children.items.len - 1];
                    text_ptr += 1;
                    continue :expr text[text_ptr];
                },
                ')' => {
                    // End of expression
                    cur_ast = @constCast(cur_ast.parent.?);
                    text_ptr += 1;
                    continue :expr text[text_ptr];
                },
                'a'...'z', 'A'...'Z', '0'...'9', '+', '-', '=', '_', '*' => {
                    continue :parse .token;
                },
                '\'' => {
                    text_ptr += 1;
                    continue :parse .quot;
                },
                '"' => {
                    text_ptr += 1;
                    continue :parse .dbl_quot;
                },
                '#' => {
                    text_ptr += 1;
                    continue :parse .octothorpe;
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
    try std.testing.expectEqual(@intFromEnum(AST.Node.children), @intFromEnum(ast.val));
    try std.testing.expectEqual(0, ast.val.children.items.len);
    try std.testing.expectEqual(null, ast.parent);
}

test "parse number" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "5\n10";
    var ast = try scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    try std.testing.expectEqual(2, ast.val.children.items.len);
    try std.testing.expectEqual(5, ast.val.children.items[0].val.num);
    try std.testing.expectEqual(10, ast.val.children.items[1].val.num);
}

test "parse float" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "3.2";
    var ast = try scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    try std.testing.expectEqual(1, ast.val.children.items.len);
    try std.testing.expectEqual(3.2, ast.val.children.items[0].val.float);
}

test "parse binding" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "varname";
    var ast = try scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    try std.testing.expectEqual(1, ast.val.children.items.len);
    try std.testing.expectEqualStrings("varname", ast.val.children.items[0].val.binding);
}

test "parse let" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(let ((a 3)) a)";
    var ast = try scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    try std.testing.expectEqual(1, ast.val.children.items.len);
    const expr = ast.val.children.items[0].val;
    try std.testing.expectEqual(3, expr.children.items.len);
    try std.testing.expectEqualStrings("let", expr.children.items[0].val.binding);
    try std.testing.expectEqualStrings("a", expr.children.items[2].val.binding);
    try std.testing.expectEqual(1, expr.children.items[1].val.children.items.len);
}

test "parse expr" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(funcname 5 3)";
    var ast = try scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    try std.testing.expectEqual(1, ast.val.children.items.len);
    const expr = ast.val.children.items[0].val;
    try std.testing.expectEqual(3, expr.children.items.len);
    try std.testing.expectEqualStrings("funcname", expr.children.items[0].val.binding);
    try std.testing.expectEqual(5, expr.children.items[1].val.num);
    try std.testing.expectEqual(3, expr.children.items[2].val.num);
}

test "parse nested parens" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "((()))";
    var ast = try scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    try std.testing.expectEqual(1, ast.val.children.items.len);
    try std.testing.expectEqual(1, ast.val.children.items[0].val.children.items.len);
    try std.testing.expectEqual(1, ast.val.children.items[0].val.children.items[0].val.children.items.len);
    try std.testing.expectEqual(0, ast.val.children.items[0].val.children.items[0].val.children.items[0].val.children.items.len);
}

test "parse char" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(atoi '4')";
    var ast = try scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    try std.testing.expectEqual(1, ast.val.children.items.len);
    const expr = ast.val.children.items[0].val;
    try std.testing.expectEqual(2, expr.children.items.len);
    try std.testing.expectEqualStrings("atoi", expr.children.items[0].val.binding);
    try std.testing.expectEqual('4', expr.children.items[1].val.char);
}

test "parse str" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(string \"abcd\")";
    var ast = try scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    try std.testing.expectEqual(1, ast.val.children.items.len);
    const expr = ast.val.children.items[0].val;
    try std.testing.expectEqual(2, expr.children.items.len);
    try std.testing.expectEqualStrings("string", expr.children.items[0].val.binding);
    try std.testing.expectEqualStrings("abcd", expr.children.items[1].val.str);
}

test "parse weird" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(string-ref (string #\\a #\\b #\\c #\\d #\\e) 3)";
    var ast = try scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    try std.testing.expectEqual(1, ast.val.children.items.len);
    const expr = ast.val.children.items[0].val;
    try std.testing.expectEqual(3, expr.children.items.len);
}
