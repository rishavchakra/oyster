const std = @import("std");
const parser = @import("parser.zig");

pub const Instr = enum(u64) {
    Push = 0b00_0011_1111, // LOAD64
    Pop = 0b01_0011_1111,
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
    var visited: std.AutoHashMap(*const parser.AST, void) = .init(alloc);
    var dfs_stack: std.ArrayList(*const parser.AST) = try .initCapacity(alloc, 16);
    defer visited.deinit();
    defer dfs_stack.deinit(alloc);
    var statics: std.ArrayList(u8) = try .initCapacity(alloc, 64);

    var opcode_list: std.ArrayList(OpCode) = try .initCapacity(alloc, 16);

    try dfs_stack.append(alloc, ast);
    while (dfs_stack.items.len > 0) {
        const cur_ast = dfs_stack.pop().?;
        const is_visited = try visited.getOrPut(cur_ast);
        if (is_visited.found_existing) {
            continue;
        }

        switch (cur_ast.val) {
            .num => |n| {
                try opcode_list.append(alloc, OpCode{ .instr = .Push });
                try opcode_list.append(alloc, OpCode{ .int = Int.init(n) });
            },
            .float => {
                // Since there is no float tagging yet, I can't quite implement this yet
                unreachable;
            },
            .boolean => |b| {
                try opcode_list.append(alloc, OpCode{ .instr = .Push });
                try opcode_list.append(alloc, OpCode{ .boolean = Boolean.init(b) });
            },
            .char => {
                try opcode_list.append(alloc, OpCode{ .instr = .Push });
                try opcode_list.append(alloc, OpCode{ .char = Char.init(cur_ast.val.char) });
            },
            .str => {
                // There is also no string parsing yet because they're heap-allocated (future work)
                unreachable;
            },
            .binding => |binding| {
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
                    break;
                }

                const operation = children.items[0];
                // Specific function checking
                if (operation.val == .binding) {
                    const binding = children.items[0].val.binding;
                    if (std.mem.eql(u8, binding, "if")) {
                        // If
                    } else if (std.mem.eql(u8, binding, "let")) {
                        // The first argument to let should be a list of bindings
                        if (children.items[1].val != .children) {
                            // TODO: better error message
                            // Also, return error{} doesn't work(?)
                            // return error{};
                        }

                        var expr_i = children.items.len - 1;
                        while (expr_i > 1) {
                            // Don't recurse on the first argument (bindings list)
                            const expr = &children.items[expr_i];
                            try dfs_stack.append(alloc, expr);
                            expr_i -= 1;
                        }

                        const bindings = children.items[1].val.children.items;
                        var bindings_i = bindings.len - 1;
                        while (bindings_i >= 0) {
                            // Recurse on all the binding children
                            const child = &bindings[bindings_i];
                            try dfs_stack.append(alloc, child);
                            bindings_i -= 1;
                        }
                    }

                    // Evaluate the result of the operation on the operands once the operands are all evaluated
                    try statics.append(alloc, @truncate(binding.len));
                    const binding_ind = statics.items.len;
                    for (binding) |c| {
                        try statics.append(alloc, c);
                    }
                    try opcode_list.append(alloc, OpCode{ .instr = .Eval });
                    try opcode_list.append(alloc, OpCode{ .binding = &statics.items[binding_ind .. binding_ind + binding.len] });
                }

                // Operation
                try dfs_stack.append(alloc, &operation);

                // Operands
                var i = children.items.len - 1;
                while (i > 0) {
                    const child = &children.items[i];
                    try dfs_stack.append(alloc, child);
                    i -= 1;
                }
            },
        }
    }

    try opcode_list.append(alloc, OpCode{ .instr = .Return });
    const code = try opcode_list.toOwnedSlice(alloc);

    return CompileOutput{
        .code = code,
        .statics = try statics.toOwnedSlice(alloc),
    };
    // return opcode_list;
}

test "expr +" {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(+ 1 2)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const opcodes = compile_output.code;
    std.debug.print("BINDING: {s}\n", .{opcodes[1].binding.*});
    for (opcodes) |op| {
        op.print();
    }
}

test "define" {}

test "if" {}
