const std = @import("std");
const compiler = @import("compiler.zig");
const parser = @import("parser.zig");

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
const BindingList = std.ArrayList(Binding);
const BindingMap = std.AutoHashMap([*]const u8, BindingList);
const BindingLifetimes = std.ArrayList(std.StringHashMap(void));
const Func = fn (*Stack, *BindingMap, std.mem.Allocator) anyerror!void;

pub fn interpret(bytecode: compiler.CompileOutput, alloc: std.mem.Allocator) !u64 {
    var pc: usize = 0;
    var stack: Stack = try .initCapacity(alloc, 64);
    const bindings: BindingMap = .init(alloc);
    const opcodes = bytecode.code;

    run: while (true) {
        const optype = opcodes[pc].type_of();
        switch (optype) {
            .instr => {
                const instr = opcodes[pc].instr;
                switch (instr) {
                    .Push => {
                        try stack.append(alloc, opcodes[pc + 1].raw);
                        pc += 2;
                    },
                    .Eval => {
                        const binding_name = opcodes[pc + 1].binding;
                        const val_list = bindings.get(binding_name.*.ptr);
                        if (val_list == null) {
                            std.debug.print("UNBOUND BINDING: {s}\n", .{binding_name.*});
                            break :run;
                        }
                        const binding = val_list.?.getLast();
                        switch (binding) {
                            .stack => |ind| try stack.append(alloc, stack.items[ind]),
                            .native => |ind| try funcs[ind](&stack, &bindings, alloc),
                            .static => unreachable,
                            // unreachable for now,
                            // this is for things like string constants
                        }
                    },
                    .Jump => {
                        const cond = compiler.OpCode{ .raw = stack.pop().? };
                        const target = opcodes[pc + 1].codepoint;
                        if (cond.type_of() == .boolean and !cond.boolean.val) {
                            // Take the jump (second arm) if false
                            pc = target;
                        } else {
                            // Don't take the jump, skip over jump and the target opcode
                            pc = pc + 2;
                        }
                    },
                    .Squash => {
                        const num_squash = opcodes[pc + 1].raw;
                        const res = stack.pop().?;
                        for (0..num_squash) |_| {
                            _ = stack.pop().?;
                            // Pop both the binding and the value off the stack.
                            // Value is useless now, but we use the binding name
                            // to remove the topmost value from the bindings list
                            // in map[name].
                            // If this is the last binding in the list,
                            // maybe deallocate the list (although not necessary).
                        }
                        try stack.append(alloc, res);
                    },
                    .Return => {
                        const ret = stack.pop().?;
                        return ret;
                    },
                }
            },
            else => {
                break :run;
            },
        }
    }

    return 0;
}

//================ Native Functions ================

fn native_let(stack: *Stack, binding_map: *BindingMap, alloc: std.mem.Allocator) !void {
    _ = stack;
    _ = binding_map;
    _ = alloc;
    // Go through the following bindings of format:
    // (EVAL [binding] [stack value])
    // evaluate the stack values,
    // put them into the bindings map,
    // and push both binding and value onto stack.
    // the EVAL at the beginning is an artifact of the compiler output
    // and could be solved with a let opcode
    // (but I think it would be funny if I had as few opcodes as possible)
}

fn native_add1(stack: *Stack, binding_map: *BindingMap, alloc: std.mem.Allocator) !void {
    _ = binding_map;
    const arg = compiler.OpCode{stack.pop().?};
    if (arg.type_of() != .int) {
        return error{};
    }
    stack.append(compiler.OpCode{ .int = compiler.Int.init(arg.int.val + 1) }, alloc);
}

fn native_sub1(stack: *Stack, binding_map: *BindingMap, alloc: std.mem.Allocator) !void {
    _ = binding_map;
    const arg = compiler.OpCode{try stack.pop()};
    if (arg.type_of() != .int) {
        return error{};
    }
    stack.append(compiler.OpCode{ .int = compiler.Int.init(arg.int.val - 1) }, alloc);
}

fn native_atoi(stack: *Stack, binding_map: *BindingMap, alloc: std.mem.Allocator) !void {
    _ = stack;
    _ = binding_map;
    _ = alloc;
    // Easy conversion between int opcode and char opcode
}

fn native_itoa(stack: *Stack, binding_map: *BindingMap, alloc: std.mem.Allocator) !void {
    _ = stack;
    _ = binding_map;
    _ = alloc;
    // Easy conversion between int opcode and char opcode
}

fn native_is_null(stack: *Stack, binding_map: *BindingMap, alloc: std.mem.Allocator) !void {
    _ = stack;
    _ = binding_map;
    _ = alloc;
    // I've been testing is_null on Chicken Scheme
    // and I can't find a single value that makes this true...
}

fn native_is_zero(stack: *Stack, binding_map: *BindingMap, alloc: std.mem.Allocator) !void {
    _ = stack;
    _ = binding_map;
    _ = alloc;
    // Easy zero checking, just check across different types
}

fn native_is_int(stack: *Stack, binding_map: *BindingMap, alloc: std.mem.Allocator) !void {
    _ = stack;
    _ = binding_map;
    _ = alloc;
    // Check the tag (type_of() function) of topmost stack value
}

fn native_is_bool(stack: *Stack, binding_map: *BindingMap, alloc: std.mem.Allocator) !void {
    _ = stack;
    _ = binding_map;
    _ = alloc;
    // Check the tag (type_of() function) of topmost stack value
}

fn native_not(stack: *Stack, binding_map: *BindingMap, alloc: std.mem.Allocator) !void {
    _ = stack;
    _ = binding_map;
    _ = alloc;
    // Check the tag (type_of() function) of topmost stack value for bool,
    // negate
    // otherwise, error
}

test native_add1 {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(add1 2)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compiler.compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    for (compile_output.code) |op| {
        op.print();
    }
    const res = try interpret(compile_output, alloc);
    std.debug.print("RC: {d}\n", .{res});
}
