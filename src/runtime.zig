const std = @import("std");
const compiler = @import("compiler.zig");

// pub const Runtime = struct {
//     const Self = @This();
//     pub const FuncPtrList = std.ArrayList((fn (*Self) void));
//     pub const FuncPtrMap = std.StringHashMap(FuncPtrList);
//     pub const Stack = std.ArrayList(u8);
//
//     stack: Stack,
//     func_ptrs: std.StringHashMap(FuncPtrList),
//     alloc: std.mem.Allocator,
//     pc: usize,
// };

// pub const Runtime = struct {};

const native_funcs = [_]NativeFunc{
    native_if,
};

const Stack = std.ArrayList(u64);
const Binding = union(enum) {
    native: u32,
    static: u32,
    stack: u32,
};
const BindingList = std.ArrayList(u64);
const BindingMap = std.AutoHashMap([*]const u8, BindingList);
const BindingLifetimes = std.ArrayList(std.StringHashMap(void));
const NativeFunc = fn (*Stack, *BindingMap, *BindingLifetimes, std.mem.Allocator) anyerror!void;

pub fn interpret(bytecode: compiler.CompileOutput, alloc: std.mem.Allocator) !i64 {
    var pc: usize = 0;
    const stack: Stack = .initCapacity(alloc, 64);
    const bindings: BindingMap = .init(alloc);
    _ = bindings;
    const binding_lifetimes: BindingLifetimes = .initCapacity(alloc, 8);
    _ = binding_lifetimes;

    const opcodes = bytecode.code;

    run: switch (opcodes[pc]) {
        .instr => |instr| {
            switch (instr) {
                .Push => {
                    // LOAD [value]
                    _ = opcodes[pc + 1].type_of() catch |err| return err;
                    stack.append(alloc, opcodes[pc + 1].raw);
                    pc += 2;
                },
                .Eval => {
                    // Evaluate the following binding
                    // and apply it to the stack operands
                },
                .Return => {
                    const ret = try stack.pop();
                    _ = ret;
                    break :run;
                },
            }
        },
    }

    return 0;
}

fn native_if(stack: *Stack, binding_map: *BindingMap, lifetimes: *BindingLifetimes, alloc: std.mem.Allocator) !void {
    _ = binding_map;
    _ = lifetimes;
    const cond = compiler.OpCode{ .raw = try stack.pop() };
    const is_false = cond.type_of() == .boolean and cond.boolean.val == false;
    const arm1 = try stack.pop();
    const arm2 = try stack.pop();
    if (!is_false) {
        stack.append(compiler.OpCode{ .raw = arm1 }, alloc);
    } else {
        stack.append(compiler.OpCode{ .raw = arm2 }, alloc);
    }
}

fn native_add1(stack: *Stack, binding_map: *BindingMap, lifetimes: *BindingLifetimes, alloc: std.mem.Allocator) !void {
    _ = binding_map;
    _ = lifetimes;
    const arg = compiler.OpCode{try stack.pop()};
    if (arg.type_of() != .int) {
        return error{};
    }
    stack.append(compiler.OpCode{ .int = compiler.Int.init(arg.int.val + 1) }, alloc);
}
