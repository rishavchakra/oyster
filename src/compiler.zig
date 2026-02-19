const std = @import("std");
const parser = @import("parser.zig");

/// Type tags to be used for 32-bit values, to fit into a single 64-bit word
pub const Tag = enum(u32) {
    Bool = 0b000001_11,
    Char = 0b000010_11,
    Float = 0b000011_11,
    Instr = 0b000100_11,
    Str = 0b000101_11,
    HeapPtr = 0b000110_11,
    Binding = 0b000111_11,
};

pub const OpInt = packed struct {
    tag: u2,
    val: i62,
    pub fn init(val: i64) OpInt {
        if (val >= 1 << 62) {
            // Error case: int too big
        }
        return OpInt{
            .tag = 0b00,
            .val = @truncate(val),
        };
    }
};

pub const OpBool = packed struct {
    tag: Tag,
    val: bool,
    pad: u31, // Not used
    pub fn init(val: bool) OpBool {
        return OpBool{
            .tag = .Bool,
            .val = val,
            .pad = 0,
        };
    }
};

pub const OpChar = packed struct {
    tag: Tag,
    val: u8,
    pad: u24, // Not used
    pub fn init(val: u8) OpChar {
        return OpChar{
            .tag = .Char,
            .val = val,
            .pad = 0,
        };
    }
};

pub const OpFloat = packed struct {
    tag: Tag,
    val: f32,
    pub fn init(val: f32) OpFloat {
        return OpFloat{
            .tag = .Float,
            .val = val,
        };
    }
};

pub const OpHeapPtr = packed struct {
    tag: Tag,
    val: u32,
    pub fn init(val: u32) OpHeapPtr {
        return OpHeapPtr{ .tag = .HeapPtr, .val = val };
    }
};

pub const Instr = enum(u8) {
    Push,
    Jump,
    Eval,
    Unset,
    Set,
    Squash,
};

pub const OpInstr = packed struct {
    tag: Tag,
    val: Instr,
    pad: u24, // Not used
    pub fn init(val: Instr) OpInstr {
        return OpInstr{
            .tag = .Instr,
            .val = val,
            .pad = 0,
        };
    }
};

pub const OpBinding = packed struct {
    tag: Tag,
    val: u32,
    pub fn init(val: u32) OpBinding {
        return OpBinding{
            .tag = .Binding,
            .val = val,
        };
    }
};

const OpHalfWidth = packed struct {
    tag: u32,
    val: u32,
};

/// This does not give any comptime or runtime type checking,
/// in order to keep the size to 64 bits (packed union).
/// Just reinterpret the bits.
pub const OpCode = packed union {
    raw: u64,
    instr: OpInstr,
    int: OpInt,
    boolean: OpBool,
    char: OpChar,
    float: OpFloat,
    codepoint: usize,
    heap_ptr: OpHeapPtr,
    /// Bindings are not interpreted in the same way as other opcodes
    /// because they are string pointers which may step on opcode tags
    /// Bindings are indices into
    binding: OpBinding,
    /// This is only used internally, for easy instruction decoding
    half: OpHalfWidth,

    pub fn type_of(self: OpCode) OpCodeType {
        // Read from the tags to get the true value and type of the opcode
        const as_int = self.int;
        if (as_int.tag == 0) {
            return .Int;
        }

        const tag = self.half.tag;
        switch (tag) {
            @intFromEnum(Tag.Bool) => return .Bool,
            @intFromEnum(Tag.Char) => return .Char,
            @intFromEnum(Tag.Float) => return .Float,
            @intFromEnum(Tag.Instr) => return .Instr,
            @intFromEnum(Tag.Binding) => return .Binding,
            else => return .Raw,
        }
    }

    pub fn print(self: OpCode) void {
        const op = type_of(self);
        std.debug.print("{s}\t", .{@tagName(op)});
        switch (op) {
            .Binding => {
                std.debug.print("{d}", .{self.binding.val});
            },
            .Instr => {
                std.debug.print("{s}", .{@tagName(self.instr.val)});
            },
            .Int => {
                std.debug.print("{d}", .{self.int.val});
            },
            .Float => {
                std.debug.print("{d}", .{self.float.val});
            },
            .Char => {
                std.debug.print("{d}", .{self.char.val});
            },
            .Raw => {
                if (self.raw == std.math.maxInt(u64)) {
                    std.debug.print("RETURN", .{});
                } else {
                    std.debug.print("{d}", .{self.raw});
                }
            },
            .Bool => {
                if (self.boolean.val) {
                    std.debug.print("T", .{});
                } else {
                    std.debug.print("F", .{});
                }
            },
        }
        std.debug.print("\t\tRAW: {d}\n", .{self.raw});
    }
};

pub const OpCodeType = enum {
    Binding,
    Instr,
    Int,
    Bool,
    Char,
    Float,
    Raw,
};

pub const CompileOutput = struct {
    statics: []u8,
    code: []OpCode,

    pub fn deinit(self: *CompileOutput, alloc: std.mem.Allocator) void {
        alloc.free(self.statics);
        alloc.free(self.code);
    }
};

pub fn compile(ast: *const parser.AST, alloc: std.mem.Allocator) !CompileOutput {
    var statics: std.ArrayList(u8) = try .initCapacity(alloc, 64);
    var opcode_list: std.ArrayList(OpCode) = try .initCapacity(alloc, 16);
    var bindings_map: std.StringHashMap(usize) = .init(alloc);
    defer bindings_map.deinit();
    try compile_rec(ast, &opcode_list, &statics, &bindings_map, alloc);
    try opcode_list.append(alloc, OpCode{ .instr = OpInstr.init(.Set) });
    try opcode_list.append(alloc, OpCode{ .raw = std.math.maxInt(u64) });
    return CompileOutput{
        .statics = try statics.toOwnedSlice(alloc),
        .code = try opcode_list.toOwnedSlice(alloc),
    };
}

fn compile_rec(ast: *const parser.AST, opcode_list: *std.ArrayList(OpCode), statics: *std.ArrayList(u8), bindings: *std.StringHashMap(usize), alloc: std.mem.Allocator) !void {
    switch (ast.val) {
        .num => |n| {
            try opcode_list.append(alloc, OpCode{ .instr = OpInstr.init(.Push) });
            try opcode_list.append(alloc, OpCode{ .int = OpInt.init(n) });
        },
        .float => {
            // Not yet implemented
            unreachable;
        },
        .boolean => |b| {
            try opcode_list.append(alloc, OpCode{ .instr = OpInstr.init(.Push) });
            try opcode_list.append(alloc, OpCode{ .boolean = OpBool.init(b) });
        },
        .char => |c| {
            try opcode_list.append(alloc, OpCode{ .instr = OpInstr.init(.Push) });
            try opcode_list.append(alloc, OpCode{ .char = OpChar.init(c) });
        },
        .str => |s| {
            const len_ind = statics.items.len;
            try statics.appendNTimes(alloc, 0, 4); // alloc 4 bytes for string length
            const str_slice: *[4]u8 = @ptrCast(statics.items[len_ind..]);
            std.mem.writeInt(u32, str_slice, @as(u32, @truncate(s.len)), .little);

            const str_ind = statics.items.len;
            try statics.appendNTimes(alloc, 0, s.len);
            @memcpy(statics.items[str_ind..], s);

            try opcode_list.append(alloc, OpCode{ .binding = OpBinding.init(@truncate(str_ind)) });
        },
        .binding => |binding| {
            if (bindings.contains(binding)) {
                // Binding already found
                const existing_binding_ind = bindings.get(binding).?;
                try opcode_list.append(alloc, OpCode{ .instr = OpInstr.init(.Eval) });
                try opcode_list.append(alloc, OpCode{ .binding = OpBinding.init(@truncate(existing_binding_ind)) });
                return;
            }

            // New binding found
            // Add the length of the string to the statics section
            try statics.append(alloc, @as(u8, @truncate(binding.len)));
            const binding_str_ind = statics.items.len;
            // Add the string to the statics section
            try statics.appendNTimes(alloc, 0, binding.len);
            const binding_ptr = statics.items[binding_str_ind .. binding_str_ind + binding.len];
            @memcpy(binding_ptr, binding);
            try opcode_list.append(alloc, OpCode{ .instr = OpInstr.init(.Eval) });
            try opcode_list.append(alloc, OpCode{ .binding = OpBinding.init(@truncate(binding_str_ind)) });
            try bindings.put(binding, binding_str_ind);
        },
        .children => |children| {
            if (children.items.len == 0) {
                return;
            }

            switch (children.items[0].val) {
                .binding => {
                    const binding = children.items[0].val.binding;
                    if (std.mem.eql(u8, binding, "if")) {
                        if (children.items.len != 4) {
                            // TODO: better error handling
                            std.debug.print("ERROR: malformed if\n", .{});
                        }
                        // Handle condition
                        try compile_rec(&children.items[1], opcode_list, statics, bindings, alloc);

                        try opcode_list.append(alloc, OpCode{ .instr = OpInstr.init(.Jump) });
                        const cond_target_ind = opcode_list.items.len; // The index of the conditional jump target
                        try opcode_list.append(alloc, OpCode{ .codepoint = undefined });

                        // First arm
                        try compile_rec(&children.items[2], opcode_list, statics, bindings, alloc);

                        // Unconditional jump to end of second arm
                        // Pushing True: no need for unconditional jump instruction
                        try opcode_list.append(alloc, OpCode{ .instr = OpInstr.init(.Push) });
                        try opcode_list.append(alloc, OpCode{ .boolean = OpBool.init(true) });
                        try opcode_list.append(alloc, OpCode{ .instr = OpInstr.init(.Jump) });
                        try opcode_list.append(alloc, OpCode{ .codepoint = undefined });

                        // Beginning of second arm = target of cond false
                        const second_arm_ind = opcode_list.items.len;
                        opcode_list.items[cond_target_ind] = OpCode{ .codepoint = second_arm_ind };

                        // Run second arm
                        try compile_rec(&children.items[3], opcode_list, statics, bindings, alloc);
                        // End of second arm = target of unconditional jump
                        const end_ind = opcode_list.items.len;
                        opcode_list.items[second_arm_ind - 1] = OpCode{ .codepoint = end_ind };
                    } else if (std.mem.eql(u8, binding, "let")) {
                        switch (children.items[1].val) {
                            .children => |bindings_list| {
                                for (bindings_list.items) |var_binding| {
                                    if (var_binding.val != .children) {
                                        std.debug.print("ERROR: malformed let\n", .{});
                                    }
                                    try compile_rec(&var_binding, opcode_list, statics, bindings, alloc);

                                    const set_opcode_ind = opcode_list.items.len - 3;
                                    opcode_list.items[set_opcode_ind] = OpCode{ .instr = OpInstr.init(.Set) };
                                    _ = opcode_list.orderedRemove(opcode_list.items.len - 1); // removes the argc opcode added by finding a binding
                                }

                                for (children.items[2..]) |child| {
                                    try compile_rec(&child, opcode_list, statics, bindings, alloc);
                                }

                                // The number of bindings that need to be freed
                                const num_bindings = children.items[1].val.children.items.len;
                                try opcode_list.append(alloc, OpCode{ .instr = OpInstr.init(.Unset) });
                                try opcode_list.append(alloc, OpCode{ .raw = num_bindings });
                            },
                            // The first argument to let should be a list of bindings
                            else => std.debug.print("ERROR: malformed let\n", .{}),
                        }
                    } else {
                        // Not a reserved function name
                        for (children.items[1..]) |child| {
                            try compile_rec(&child, opcode_list, statics, bindings, alloc);
                        }

                        // Compile the function binding call (recursive in case of sub-logic)
                        try compile_rec(&children.items[0], opcode_list, statics, bindings, alloc);

                        // Put the provided arity of the called function in the code, in case that's needed
                        try opcode_list.append(alloc, OpCode{ .raw = children.items.len - 1 });
                    }
                },
                else => {
                    // Not a binding
                    for (children.items) |child| {
                        try compile_rec(&child, opcode_list, statics, bindings, alloc);
                    }

                    if (children.items.len > 1) {
                        // Squash all except the last child away
                        try opcode_list.append(alloc, OpCode{ .instr = OpInstr.init(.Squash) });
                        try opcode_list.append(alloc, OpCode{ .raw = children.items.len - 1 });
                    }
                },
            }
        },
    }
}

test "basic" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(+ 1 2)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const opcodes = compile_output.code;
    try std.testing.expectEqual(9, opcodes.len);
    const expected = [_]OpCode{
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(1) },
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(2) },
        OpCode{ .instr = OpInstr.init(.Eval) },
        OpCode{ .binding = OpBinding.init(1) }, // First binding: +
        OpCode{ .raw = 2 }, // Arity of function: 2
        OpCode{ .instr = OpInstr.init(.Set) },
        OpCode{ .raw = std.math.maxInt(u64) },
    };
    for (expected, opcodes, 0..) |e, a, i| {
        std.testing.expectEqual(e.raw, a.raw) catch std.debug.print("ERROR: index {d} does not match (raw): {d}, {d}\n", .{ i, e.raw, a.raw });
    }
}

test "basic 2" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(+ 1 2) (* 3 4)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const opcodes = compile_output.code;
    try std.testing.expectEqual(18, opcodes.len);
    const expected = [_]OpCode{
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(1) },
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(2) },
        OpCode{ .instr = OpInstr.init(.Eval) },
        OpCode{ .binding = OpBinding.init(1) }, // First binding: +
        OpCode{ .raw = 2 }, // Arity of function: 2

        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(3) },
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(4) },
        OpCode{ .instr = OpInstr.init(.Eval) },
        OpCode{ .binding = OpBinding.init(3) }, // First binding: +
        OpCode{ .raw = 2 }, // Arity of function: 2

        OpCode{ .instr = OpInstr.init(.Squash) },
        OpCode{ .raw = 1 },
        OpCode{ .instr = OpInstr.init(.Set) },
        OpCode{ .raw = std.math.maxInt(u64) },
    };
    for (expected, opcodes, 0..) |e, a, i| {
        std.testing.expectEqual(e.raw, a.raw) catch std.debug.print("ERROR: index {d} does not match (raw): {d}, {d}\n", .{ i, e.raw, a.raw });
    }
}

test "nested" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(+ (* 3 4) 5)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const opcodes = compile_output.code;

    try std.testing.expectEqual(14, opcodes.len);
    const expected = [_]OpCode{
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(3) },
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(4) },
        OpCode{ .instr = OpInstr.init(.Eval) },
        OpCode{ .binding = OpBinding.init(1) }, // First binding: *
        OpCode{ .raw = 2 }, // Arity of function: 2
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(5) },
        OpCode{ .instr = OpInstr.init(.Eval) },
        OpCode{ .binding = OpBinding.init(3) }, // Second binding: +
        OpCode{ .raw = 2 }, // Arity of function: 2
        OpCode{ .instr = OpInstr.init(.Set) },
        OpCode{ .raw = std.math.maxInt(u64) },
    };
    for (expected, opcodes, 0..) |e, a, i| {
        std.testing.expectEqual(e.raw, a.raw) catch std.debug.print("ERROR: index {d} does not match (raw): {d}, {d}\n", .{ i, e.raw, a.raw });
    }
}

test "let" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(let ((a 3)) a)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const opcodes = compile_output.code;

    try std.testing.expectEqual(10, opcodes.len);
    const expected = [_]OpCode{
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(3) },
        OpCode{ .instr = OpInstr.init(.Set) },
        OpCode{ .binding = OpBinding.init(1) }, // binding name: a
        OpCode{ .instr = OpInstr.init(.Eval) },
        OpCode{ .binding = OpBinding.init(1) }, // binding name: a
        OpCode{ .instr = OpInstr.init(.Unset) },
        OpCode{ .raw = 1 }, // Squash one let binding down
        OpCode{ .instr = OpInstr.init(.Set) },
        OpCode{ .raw = std.math.maxInt(u64) },
    };
    for (expected, opcodes, 0..) |e, a, i| {
        std.testing.expectEqual(e.raw, a.raw) catch std.debug.print("ERROR: index {d} does not match (raw): {d}, {d}\n", .{ i, e.raw, a.raw });
    }
}

test "let 2" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(let ((a 3) (b 5)) a)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const opcodes = compile_output.code;

    try std.testing.expectEqual(14, opcodes.len);
    const expected = [_]OpCode{
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(3) },
        OpCode{ .instr = OpInstr.init(.Set) },
        OpCode{ .binding = OpBinding.init(1) }, // binding name: a
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(5) },
        OpCode{ .instr = OpInstr.init(.Set) },
        OpCode{ .binding = OpBinding.init(3) }, // binding name: b
        OpCode{ .instr = OpInstr.init(.Eval) },
        OpCode{ .binding = OpBinding.init(1) }, // binding name: a
        OpCode{ .instr = OpInstr.init(.Unset) },
        OpCode{ .raw = 2 }, // Squash two let bindings down
        OpCode{ .instr = OpInstr.init(.Set) },
        OpCode{ .raw = std.math.maxInt(u64) },
    };
    for (expected, opcodes, 2..) |e, a, i| {
        std.testing.expectEqual(e.raw, a.raw) catch std.debug.print("ERROR: index {d} does not match (raw): {d}, {d}\n", .{ i, e.raw, a.raw });
    }
}

test "if" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(if 1 2 3)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const opcodes = compile_output.code;

    try std.testing.expectEqual(14, opcodes.len);
    const expected = [_]OpCode{
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(1) }, // First binding: let
        OpCode{ .instr = OpInstr.init(.Jump) },
        OpCode{ .raw = 10 },
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(2) },
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .boolean = OpBool.init(true) },
        OpCode{ .instr = OpInstr.init(.Jump) },
        OpCode{ .raw = 12 },
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(3) }, // Squash one let binding down
        OpCode{ .instr = OpInstr.init(.Set) },
        OpCode{ .raw = std.math.maxInt(u64) },
    };
    for (expected, opcodes, 0..) |e, a, i| {
        std.testing.expectEqual(e.raw, a.raw) catch std.debug.print("ERROR: index {d} does not match (raw): {d}, {d}\n", .{ i, e.raw, a.raw });
    }
}

test "add1" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(add1 2)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const opcodes = compile_output.code;

    try std.testing.expectEqual(7, opcodes.len);
    const expected = [_]OpCode{
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(2) },
        OpCode{ .instr = OpInstr.init(.Eval) },
        OpCode{ .binding = OpBinding.init(1) }, // First binding: add1
        OpCode{ .raw = 1 }, // Arity of function: 1
        OpCode{ .instr = OpInstr.init(.Set) },
        OpCode{ .raw = std.math.maxInt(u64) },
    };
    for (expected, opcodes, 0..) |e, a, i| {
        std.testing.expectEqual(e.raw, a.raw) catch std.debug.print("ERROR: index {d} does not match (raw): {d}, {d}\n", .{ i, e.raw, a.raw });
    }
}

test "chars" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(string #\\4 #\\3 #\\2)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const opcodes = compile_output.code;

    try std.testing.expectEqual(11, opcodes.len);
    const expected = [_]OpCode{
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .char = OpChar.init('4') },
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .char = OpChar.init('3') },
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .char = OpChar.init('2') },
        OpCode{ .instr = OpInstr.init(.Eval) },
        OpCode{ .binding = OpBinding.init(1) }, // First binding: string
        OpCode{ .raw = 3 }, // Arity of function: 1
        OpCode{ .instr = OpInstr.init(.Set) },
        OpCode{ .raw = std.math.maxInt(u64) },
    };
    for (expected, opcodes, 0..) |e, a, i| {
        std.testing.expectEqual(e.raw, a.raw) catch std.debug.print("ERROR: index {d} does not match (raw): {d}, {d}\n", .{ i, e.raw, a.raw });
    }
}

test "weird" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(string-ref (string #\\a #\\b #\\c #\\d #\\e) 3)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const opcodes = compile_output.code;

    try std.testing.expectEqual(20, opcodes.len);
    const expected = [_]OpCode{
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .char = OpChar.init('a') },
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .char = OpChar.init('b') },
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .char = OpChar.init('c') },
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .char = OpChar.init('d') },
        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .char = OpChar.init('e') },
        OpCode{ .instr = OpInstr.init(.Eval) },
        OpCode{ .binding = OpBinding.init(1) }, // First binding: string
        OpCode{ .raw = 5 }, // Arity of function: 1

        OpCode{ .instr = OpInstr.init(.Push) },
        OpCode{ .int = OpInt.init(3) },

        OpCode{ .instr = OpInstr.init(.Eval) },
        OpCode{ .binding = OpBinding.init(8) }, // First binding: string-ref
        OpCode{ .raw = 2 }, // Arity of function: 1
        OpCode{ .instr = OpInstr.init(.Set) },
        OpCode{ .raw = std.math.maxInt(u64) },
    };
    for (expected, opcodes, 0..) |e, a, i| {
        std.testing.expectEqual(e.raw, a.raw) catch std.debug.print("ERROR: index {d} does not match (raw): {d}, {d}\n", .{ i, e.raw, a.raw });
    }
}
