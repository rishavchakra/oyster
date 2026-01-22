const std = @import("std");
const parser = @import("parser.zig");

pub const Instr = enum(u64) {
    Push, // LOAD64
    Pop,
    Eval,
    Return,
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
    val: u1,
    pub fn init(val: bool) Boolean {
        return Boolean{
            .tag = 0b001_1111,
            .val = @intFromBool(val),
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
    binding: [*]const u8,

    pub fn read_val(self: OpCode) !OpCodeType {
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
        // if (self.raw >= X and self.raw <= Y) {
        //     const as_instr = self.instr;
        // }

        return error{};
    }

    pub fn print(self: OpCode) void {
        const op = read_val(self);
        std.debug.print("{s}:\t", .{@tagName(op)});
        switch (op) {
            .binding => |binding| {
                std.debug.print("{s}\n", .{binding});
            },
            .instr => |instr| {
                std.debug.print("{s}\n", .{@tagName(instr)});
            },
            .int => |i| {
                std.debug.print("\t{d}\n", .{i});
            },
            .boolean => |b| {
                if (b) {
                    std.debug.print("T\n", .{});
                } else {
                    std.debug.print("F\n", .{});
                }
            },
            .char => |c| {
                std.debug.print("\t{d}\n", .{c});
            },
            .float => |f| {
                std.debug.print("\t{d}\n", .{f});
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
};

pub fn compile(ast: *const parser.AST, alloc: std.mem.Allocator) !std.ArrayList(OpCode) {
    var visited: std.AutoHashMap(*const parser.AST, void) = .init(alloc);
    var dfs_stack: std.ArrayList(*const parser.AST) = try .initCapacity(alloc, 16);
    defer visited.deinit();
    defer dfs_stack.deinit(alloc);

    var opcode_list: std.ArrayList(OpCode) = try .initCapacity(alloc, 16);

    try dfs_stack.append(alloc, ast);
    while (dfs_stack.items.len > 0) {
        const cur_ast = dfs_stack.pop().?;
        const is_visited = try visited.getOrPut(cur_ast);
        if (is_visited.found_existing) {
            continue;
        }

        switch (cur_ast.val) {
            .num => {
                try opcode_list.append(alloc, OpCode{ .instr = .Push });
                try opcode_list.append(alloc, OpCode{ .int = Int.init(cur_ast.val.num) });
            },
            .float => {
                // Since there is no float tagging yet, I can't quite implement this yet
                unreachable;
            },
            .boolean => {
                try opcode_list.append(alloc, OpCode{ .instr = .Push });
                try opcode_list.append(alloc, OpCode{ .boolean = Boolean.init(cur_ast.val.boolean) });
            },
            .char => {
                try opcode_list.append(alloc, OpCode{ .instr = .Push });
                try opcode_list.append(alloc, OpCode{ .char = Char.init(cur_ast.val.char) });
            },
            .str => {
                // There is also no string parsing yet because they're heap-allocated (future work)
                unreachable;
            },
            .binding => {
                try opcode_list.append(alloc, OpCode{ .instr = .Eval });
                try opcode_list.append(alloc, OpCode{ .binding = cur_ast.val.children.items[0].val.binding.ptr });
            },
            .children => {
                const num_children = cur_ast.val.children.items.len;

                if (num_children > 0) {
                    // Operation
                    try dfs_stack.append(alloc, &cur_ast.val.children.items[0]);

                    // Operands
                    var i = num_children - 1;
                    while (i > 0) {
                        const child = &cur_ast.val.children.items[i];
                        try dfs_stack.append(alloc, child);
                        i -= 1;
                    }

                    // Evaluate the result of the operation on the operands once the operands are all evaluated
                    try opcode_list.append(alloc, OpCode{ .instr = .Eval });
                    try opcode_list.append(alloc, OpCode{ .binding = cur_ast.val.children.items[0].val.binding.ptr });
                }
            },
        }
    }

    try opcode_list.append(alloc, OpCode{ .instr = .Return });

    return opcode_list;
}
