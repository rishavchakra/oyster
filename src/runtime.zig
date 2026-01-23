const std = @import("std");
const compiler = @import("compiler.zig");

const funcs = [_]Func{
    native_add1,
    native_sub1,
    native_atoi,
    native_itoa,
    native_is_null,
    native_is_int,
    native_is_bool,
    native_is_zero,
    native_not,
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
const Func = fn (*Stack, *BindingMap, *BindingLifetimes, std.mem.Allocator) anyerror!void;

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
                    _ = opcodes[pc + 1].type_of() catch |err| return err;
                    stack.append(alloc, opcodes[pc + 1].raw);
                    pc += 2;
                },
                .Eval => {
                    // Evaluate the following binding
                    // and apply it to the stack operands
                    // Check it against the mapping
                    // and based on the location figure out which area to look
                },
                .Jump => {
                    const cond = compiler.OpCode{ .raw = stack.pop().? };
                    const target = opcodes[pc + 1].codepoint;
                    if (cond.type_of() == .boolean and !cond.boolean.val) {
                        // Take the jump (second arm) if false
                        continue :run opcodes[target];
                    } else {
                        continue :run opcodes[pc + 2];
                    }
                },
                .Squash => {
                    const num_squash = opcodes[pc + 1].raw;
                    const res = stack.pop().?;
                    for (0..num_squash) |_| {
                        _ = stack.pop().?;
                    }
                    try stack.append(alloc, res);
                },
                .Return => {
                    const ret = stack.pop().?;
                    _ = ret;
                    break :run;
                },
            }
        },
    }

    return 0;
}

//================ Native Functions ================

fn native_let(stack: *Stack, binding_map: *BindingMap, lifetimes: *BindingLifetimes, alloc: std.mem.Allocator) !void {
    _ = stack;
    _ = binding_map;
    _ = lifetimes;
    _ = alloc;
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

fn native_sub1(stack: *Stack, binding_map: *BindingMap, lifetimes: *BindingLifetimes, alloc: std.mem.Allocator) !void {
    _ = binding_map;
    _ = lifetimes;
    const arg = compiler.OpCode{try stack.pop()};
    if (arg.type_of() != .int) {
        return error{};
    }
    stack.append(compiler.OpCode{ .int = compiler.Int.init(arg.int.val - 1) }, alloc);
}

fn native_atoi(stack: *Stack, binding_map: *BindingMap, lifetimes: *BindingLifetimes, alloc: std.mem.Allocator) !void {
    _ = stack;
    _ = binding_map;
    _ = lifetimes;
    _ = alloc;
}

fn native_itoa(stack: *Stack, binding_map: *BindingMap, lifetimes: *BindingLifetimes, alloc: std.mem.Allocator) !void {
    _ = stack;
    _ = binding_map;
    _ = lifetimes;
    _ = alloc;
}

fn native_is_null(stack: *Stack, binding_map: *BindingMap, lifetimes: *BindingLifetimes, alloc: std.mem.Allocator) !void {
    _ = stack;
    _ = binding_map;
    _ = lifetimes;
    _ = alloc;
}

fn native_is_zero(stack: *Stack, binding_map: *BindingMap, lifetimes: *BindingLifetimes, alloc: std.mem.Allocator) !void {
    _ = stack;
    _ = binding_map;
    _ = lifetimes;
    _ = alloc;
}

fn native_is_int(stack: *Stack, binding_map: *BindingMap, lifetimes: *BindingLifetimes, alloc: std.mem.Allocator) !void {
    _ = stack;
    _ = binding_map;
    _ = lifetimes;
    _ = alloc;
}

fn native_is_bool(stack: *Stack, binding_map: *BindingMap, lifetimes: *BindingLifetimes, alloc: std.mem.Allocator) !void {
    _ = stack;
    _ = binding_map;
    _ = lifetimes;
    _ = alloc;
}

fn native_not(stack: *Stack, binding_map: *BindingMap, lifetimes: *BindingLifetimes, alloc: std.mem.Allocator) !void {
    _ = stack;
    _ = binding_map;
    _ = lifetimes;
    _ = alloc;
}
