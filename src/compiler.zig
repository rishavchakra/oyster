const std = @import("std");
const parser = @import("parser.zig");

pub const Instr = enum(u64) {
    Push = 0b00_0011_1111, // LOAD64
    Jump = 0b01_0011_1111,
    Eval = 0b10_0011_1111,
    Return = 0b11_0011_1111,
};

pub const Int = packed struct {
    tag: u2,
    val: i62,
    pub fn init(val: i64) Int {
        if (val >= 1 << 62) {
            // Error case: int too big
        }
        return Int{
            .tag = 0b00,
            .val = @truncate(val),
        };
    }
};

pub const Boolean = packed struct {
    tag: u7,
    val: bool,
    pub fn init(val: bool) Boolean {
        return Boolean{
            .tag = 0b001_1111,
            .val = val,
        };
    }
};

pub const Char = packed struct {
    tag: u8,
    val: u8,
    pub fn init(val: u8) Char {
        return Char{
            .tag = 0b0000_1111,
            .val = val,
        };
    }
};

pub const Float = packed struct {
    tag: u32,
    val: f32,
    pub fn init(val: f32) Float {
        // This is annoying!!
        // Floats cannot be variable-width like ints can,
        // so 32 bits is the limit. Find out what tag to use for this.
        _ = val;
        return Float{
            .tag = 0,
            .val = 0.0,
        };
    }
};

/// This does not give any comptime or runtime type checking,
/// in order to keep the size to 64 bits (packed union).
/// Just reinterpret the bits.
pub const OpCode = packed union {
    raw: u64,
    instr: Instr,
    int: Int,
    boolean: Boolean,
    char: Char,
    float: Float,
    codepoint: usize,
    /// Bindings are not interpreted in the same way as other opcodes
    /// because they are string pointers which may step on opcode tags
    binding: *const []const u8,

    pub fn type_of(self: OpCode) OpCodeType {
        // Read from the tags to get the true value and type of the opcode
        const as_int = self.int;
        if (as_int.tag == 0b00) {
            return .int;
        }
        const as_bool = self.boolean;
        if (as_bool.tag == 0b001_1111) {
            return .boolean;
        }
        const as_char = self.char;
        if (as_char.tag == 0b0000_1111) {
            return .char;
        }
        // Instruction parsing: what are the values we use?
        if (self.raw & 0b1111_1111 == 0b0011_1111) {
            // const as_instr = self.instr;
            return .instr;
        }
        return .raw;
    }

    pub fn print(self: OpCode) void {
        const op = type_of(self);
        std.debug.print("{s}:\t", .{@tagName(op)});
        switch (op) {
            .binding => {
                std.debug.print("{s}\n", .{self.binding.*});
            },
            .instr => {
                std.debug.print("{s}\n", .{@tagName(self.instr)});
            },
            .int => {
                std.debug.print("\t{d}\n", .{self.int.val});
            },
            .float => {
                std.debug.print("\t{d}\n", .{self.float.val});
            },
            .char => {
                std.debug.print("\t{d}\n", .{self.char.val});
            },
            .codepoint => {
                std.debug.print("\t{d}\n", .{self.codepoint});
            },
            .raw => {
                std.debug.print("\t{d}\n", .{self.raw});
            },
            .boolean => {
                if (self.boolean.val) {
                    std.debug.print("T\n", .{});
                } else {
                    std.debug.print("F\n", .{});
                }
            },
        }
    }
};

/// We do not use this in the opcodes list because it isn't packed,
/// so it's 128 bits long instead of a proper 64
pub const OpCodeType = enum {
    binding,
    instr,
    int,
    boolean,
    char,
    float,
    raw,
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
    try compile_rec(ast, &opcode_list, &statics, alloc);
    try opcode_list.append(alloc, OpCode{ .instr = .Return });
    return CompileOutput{
        .statics = try statics.toOwnedSlice(alloc),
        .code = try opcode_list.toOwnedSlice(alloc),
    };
}

fn compile_rec(ast: *const parser.AST, opcode_list: *std.ArrayList(OpCode), statics: *std.ArrayList(u8), alloc: std.mem.Allocator) !void {
    switch (ast.val) {
        .num => |n| {
            try opcode_list.append(alloc, OpCode{ .instr = .Push });
            try opcode_list.append(alloc, OpCode{ .int = Int.init(n) });
        },
        .float => {
            // Not yet implemented
            unreachable;
        },
        .boolean => |b| {
            try opcode_list.append(alloc, OpCode{ .instr = .Push });
            try opcode_list.append(alloc, OpCode{ .boolean = Boolean.init(b) });
        },
        .char => |c| {
            try opcode_list.append(alloc, OpCode{ .instr = .Push });
            try opcode_list.append(alloc, OpCode{ .char = Char.init(c) });
        },
        .str => {
            // Not yet implemented
            unreachable;
        },
        .binding => |binding| {
            std.debug.print("BINDING: {s}\n", .{binding});
            try statics.append(alloc, @truncate(binding.len));
            const binding_ind = statics.items.len;
            for (binding) |c| {
                try statics.append(alloc, c);
            }
            try opcode_list.append(alloc, OpCode{ .instr = .Eval });
            try opcode_list.append(alloc, OpCode{ .binding = &statics.items[binding_ind .. binding_ind + binding.len] });
        },
        .children => |children| {
            if (children.items.len == 0) {
                return;
            }

            switch (children.items[0].val) {
                .binding => {
                    const binding = children.items[0].val.binding;
                    if (std.mem.eql(u8, binding, "if")) {
                        if (children.items.len != 3) {
                            // TODO: better error  handling
                            std.debug.print("ERROR: malformed if\n", .{});
                        }
                        // Handle `if`
                        try compile_rec(&children.items[0], opcode_list, statics, alloc);

                        try opcode_list.append(alloc, OpCode{ .instr = .Jump });
                        const cond_target_ind = opcode_list.items.len; // The index of the conditional jump target
                        try opcode_list.append(alloc, OpCode{ .codepoint = undefined });

                        // First arm
                        try compile_rec(&children.items[1], opcode_list, statics, alloc);

                        // Unconditional jump to end of second arm
                        // Pushing True: no need for unconditional jump instruction
                        try opcode_list.append(alloc, OpCode{ .instr = .Push });
                        try opcode_list.append(alloc, OpCode{ .boolean = Boolean.init(true) });
                        try opcode_list.append(alloc, OpCode{ .codepoint = undefined });

                        // Beginning of second arm = target of cond false
                        const second_arm_ind = opcode_list.items.len;
                        opcode_list.items[cond_target_ind] = OpCode{ .codepoint = second_arm_ind };

                        // Run second arm
                        try compile_rec(&children.items[2], opcode_list, statics, alloc);
                        // End of second arm = target of unconditional jump
                        const end_ind = opcode_list.items.len;
                        opcode_list.items[second_arm_ind - 1] = OpCode{ .codepoint = end_ind };
                    } else if (std.mem.eql(u8, binding, "let")) {
                        // The first argument to let should be a list of bindings
                        if (children.items[1].val != .children) {
                            // TODO: better error handling
                            std.debug.print("ERROR: malformed let\n", .{});
                        }

                        for (children.items) |child| {
                            try compile_rec(&child, opcode_list, statics, alloc);
                        }
                    } else {
                        // Not a reserved function name
                        for (children.items[1..]) |child| {
                            try compile_rec(&child, opcode_list, statics, alloc);
                        }

                        try statics.append(alloc, @truncate(binding.len));
                        const binding_ind = statics.items.len;
                        for (binding) |c| {
                            try statics.append(alloc, c);
                        }
                        try opcode_list.append(alloc, OpCode{ .instr = .Eval });
                        try opcode_list.append(alloc, OpCode{ .binding = &statics.items[binding_ind .. binding_ind + binding.len] });
                    }
                },
                else => {
                    // Not a binding
                    for (children.items) |child| {
                        try compile_rec(&child, opcode_list, statics, alloc);
                    }
                },
            }
        },
    }
}

test "expr +" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(+ 1 2)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const opcodes = compile_output.code;
    // std.debug.print("BINDING: {s}\n", .{opcodes[1].binding.*});
    for (opcodes) |op| {
        op.print();
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
    // std.debug.print("BINDING: {s}\n", .{opcodes[1].binding.*});
    for (opcodes) |op| {
        op.print();
    }
}

test "define" {}

test "if" {}
