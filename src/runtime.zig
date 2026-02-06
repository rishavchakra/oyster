const std = @import("std");
const compiler = @import("compiler.zig");
const parser = @import("parser.zig");

const Stack = std.ArrayList(compiler.OpCode);
const Binding = union(enum) {
    reserved: Func,
    native: Func,
    static: usize,
    stack_ind: usize,
};
const BindingList = std.ArrayList(Binding);
const BindingMap = std.StringHashMap(BindingList);
const BindingLifetimes = std.ArrayList(std.StringHashMap(void));
const Env = struct {
    stack: *Stack,
    bindings: *BindingMap,
    heap: std.mem.Allocator,
};
const Func = *const fn (Env, std.mem.Allocator) anyerror!void;

pub fn interpret(bytecode: compiler.CompileOutput, alloc: std.mem.Allocator) !u64 {
    const opcodes = bytecode.code;
    var pc: usize = 0;

    var stack: Stack = try .initCapacity(alloc, 64);
    defer stack.deinit(alloc);

    var heap_buf: [1024]u8 = undefined;
    var fba: std.heap.FixedBufferAllocator = .init(&heap_buf);
    var arena: std.heap.ArenaAllocator = .init(fba.allocator());
    defer arena.deinit();
    const heap_alloc = arena.allocator();

    var bindings: BindingMap = .init(alloc);
    try init_bindings(&bindings, alloc);
    defer deinit_bindings(&bindings, alloc);

    run: while (true) {
        const optype = opcodes[pc].type_of();
        switch (optype) {
            .Instr => {
                switch (opcodes[pc].instr.val) {
                    .Push => {
                        try stack.append(alloc, opcodes[pc + 1]);
                        pc += 2;
                    },
                    .Eval => {
                        const binding_ind = opcodes[pc + 1].binding;
                        const binding_len = bytecode.statics[binding_ind - 1];
                        const binding_name = bytecode.statics[binding_ind .. binding_ind + binding_len];
                        const val_list = bindings.get(binding_name);
                        if (val_list == null) {
                            std.debug.print("UNBOUND BINDING: \"{s}\"\n", .{binding_name});
                            break :run;
                        }
                        const binding = val_list.?.getLast();
                        switch (binding) {
                            .stack_ind => |ind| try stack.append(alloc, stack.items[ind]),
                            .native => |fptr| {
                                try fptr(Env{ .stack = &stack, .bindings = &bindings, .heap = heap_alloc }, alloc);
                            },
                            .reserved => |fptr| {
                                try fptr(Env{ .stack = &stack, .bindings = &bindings, .heap = heap_alloc }, alloc);
                            },
                            .static => unreachable,
                            // unreachable for now,
                            // this is for things like string constants
                        }
                        pc += 2;
                    },
                    .Jump => {
                        const cond = stack.pop().?;
                        const target = opcodes[pc + 1].codepoint;
                        if (cond.type_of() == .Bool and !cond.boolean.val) {
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
                            const binding_val = stack.pop().?;
                            const binding = stack.pop().?;
                            const binding_ind = binding.binding;
                            const binding_len = bytecode.statics[binding_ind - 1];
                            const binding_name = bytecode.statics[binding_ind .. binding_ind + binding_len];

                            // _ = stack.pop().?;
                            // Pop both the binding and the value off the stack.
                            // Value is useless now, but we use the binding name
                            // to remove the topmost value from the bindings list
                            // in map[name].
                            // If this is the last binding in the list,
                            // maybe deallocate the list (although not necessary).
                        }
                        try stack.append(alloc, res);
                        pc += 2;
                    },
                    .Set => {
                        const val = stack.pop().?.raw;
                        const binding = opcodes[pc + 1].raw;
                        if (binding == std.math.maxInt(u64)) {
                            // Handle: return value
                            return val;
                        }

                        // Handle setting a binding to a new value
                        const binding_ind = opcodes[pc + 1].binding;
                        const binding_len = bytecode.statics[binding_ind - 1];
                        const binding_name = bytecode.statics[binding_ind .. binding_ind + binding_len];
                        var found_binding = try bindings.getOrPut(binding_name);
                        if (found_binding.found_existing) {
                            // Have a list, add the binding onto it
                            try found_binding.value_ptr.append(alloc, Binding{ .stack_ind = stack.items.len + 1 });
                        } else {
                            // Make a new list, put it on the binding
                            var binding_list = try BindingList.initCapacity(alloc, 1);
                            try binding_list.append(alloc, Binding{ .stack_ind = stack.items.len + 1 });
                            found_binding.value_ptr = &binding_list;
                        }

                        // Push the binding back onto the stack, along with its value
                        try stack.append(alloc, compiler.OpCode{ .binding = binding });
                        try stack.append(alloc, compiler.OpCode{ .raw = val });
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

pub fn init_bindings(bindings: *BindingMap, alloc: std.mem.Allocator) !void {
    var inc_bindings = try BindingList.initCapacity(alloc, 1);
    var dec_bindings = try BindingList.initCapacity(alloc, 1);
    var atoi_bindings = try BindingList.initCapacity(alloc, 1);
    var itoa_bindings = try BindingList.initCapacity(alloc, 1);
    var is_null_bindings = try BindingList.initCapacity(alloc, 1);
    var is_int_bindings = try BindingList.initCapacity(alloc, 1);
    var is_bool_bindings = try BindingList.initCapacity(alloc, 1);
    var is_zero_bindings = try BindingList.initCapacity(alloc, 1);
    var not_bindings = try BindingList.initCapacity(alloc, 1);

    try inc_bindings.append(alloc, Binding{ .native = &native_inc });
    try dec_bindings.append(alloc, Binding{ .native = &native_dec });
    try atoi_bindings.append(alloc, Binding{ .native = &native_atoi });
    try itoa_bindings.append(alloc, Binding{ .native = &native_itoa });
    try is_null_bindings.append(alloc, Binding{ .native = &native_is_null });
    try is_int_bindings.append(alloc, Binding{ .native = &native_is_int });
    try is_bool_bindings.append(alloc, Binding{ .native = &native_is_bool });
    try is_zero_bindings.append(alloc, Binding{ .native = &native_is_zero });
    try not_bindings.append(alloc, Binding{ .native = &native_not });

    try bindings.put("inc", inc_bindings);
    try bindings.put("dec", dec_bindings);
    try bindings.put("atoi", atoi_bindings);
    try bindings.put("itoa", itoa_bindings);
    try bindings.put("is_null", is_null_bindings);
    try bindings.put("is_int", is_int_bindings);
    try bindings.put("is_bool", is_bool_bindings);
    try bindings.put("is_zero", is_zero_bindings);
    try bindings.put("not", not_bindings);
}

pub fn deinit_bindings(bindings: *BindingMap, alloc: std.mem.Allocator) void {
    var binding_lists = bindings.iterator();
    while (binding_lists.next()) |ls| {
        ls.value_ptr.deinit(alloc);
    }
    bindings.deinit();
}

//================ Native Functions ================

fn native_inc(env: Env, alloc: std.mem.Allocator) !void {
    const arg = env.stack.pop().?;
    if (arg.type_of() != .Int) {
        // return error{};
    }
    try env.stack.append(alloc, compiler.OpCode{ .int = compiler.OpInt.init(arg.int.val + 1) });
}

fn native_dec(env: Env, alloc: std.mem.Allocator) !void {
    const arg = env.stack.pop().?;
    if (arg.type_of() != .Int) {
        // return error{};
    }
    try env.stack.append(alloc, compiler.OpCode{ .int = compiler.OpInt.init(arg.int.val - 1) });
}

fn native_atoi(env: Env, alloc: std.mem.Allocator) !void {
    const arg = env.stack.pop().?;
    const arg_type = arg.type_of();
    if (arg_type == .Char) {
        if (arg.char.val < '0' or arg.char.val > '9') {
            std.debug.print("ERROR: Bad char in atoi\n", .{});
            return;
        }
        try env.stack.append(alloc, compiler.OpCode{ .int = compiler.OpInt.init(arg.char.val - 48) });
    }
    // TODO: handle typeerror case
}

fn native_itoa(env: Env, alloc: std.mem.Allocator) !void {
    const arg = env.stack.pop().?;
    const arg_type = arg.type_of();
    if (arg_type == .Int) {
        if (arg.int.val < 0 or arg.int.val > 9) {
            std.debug.print("ERROR: int out of bounds in itoa\n", .{});
        }
        try env.stack.append(alloc, compiler.OpCode{ .char = compiler.OpChar.init(@as(u8, @truncate(@abs(arg.int.val))) + '0') });
    }
    // TODO: handle typeerror case
}

fn native_is_null(env: Env, alloc: std.mem.Allocator) !void {
    _ = env;
    _ = alloc;
    // I've been testing is_null on Chicken Scheme
    // and I can't find a single value that makes this true...
}

fn native_is_zero(env: Env, alloc: std.mem.Allocator) !void {
    const arg = env.stack.pop().?;
    const arg_type = arg.type_of();
    const is_zero = switch (arg_type) {
        .Int => arg.int.val == 0,
        .Char => arg.char.val == 0,
        .Float => arg.float.val == 0.0,
        .Bool => arg.boolean.val == false,
        .Raw => arg.raw == 0,
        else => false, // TODO: Throw a type error instead
    };
    try env.stack.append(alloc, compiler.OpCode{ .boolean = compiler.OpBool.init(is_zero) });
}

fn native_is_int(env: Env, alloc: std.mem.Allocator) !void {
    const arg = env.stack.pop().?;
    const arg_type = arg.type_of();
    try env.stack.append(alloc, compiler.OpCode{ .boolean = compiler.OpBool.init(arg_type == .Int) });
}

fn native_is_bool(env: Env, alloc: std.mem.Allocator) !void {
    const arg = env.stack.pop().?;
    const arg_type = arg.type_of();
    try env.stack.append(alloc, compiler.OpCode{ .boolean = compiler.OpBool.init(arg_type == .Bool) });
}

fn native_not(env: Env, alloc: std.mem.Allocator) !void {
    const arg = env.stack.pop().?;
    const arg_type = arg.type_of();
    if (arg_type == .Bool) {
        try env.stack.append(alloc, compiler.OpCode{ .boolean = compiler.OpBool.init(!arg.boolean.val) });
    }
}

test native_inc {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(inc 2)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compiler.compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const res = try interpret(compile_output, alloc);
    const opcode = compiler.OpCode{ .raw = res };
    try std.testing.expectEqual(.Int, opcode.type_of());
    try std.testing.expectEqual(3, opcode.int.val);
}

test native_dec {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(dec 4)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compiler.compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const res = try interpret(compile_output, alloc);
    const opcode = compiler.OpCode{ .raw = res };
    try std.testing.expectEqual(.Int, opcode.type_of());
    try std.testing.expectEqual(3, opcode.int.val);
}

test native_atoi {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(atoi '2')";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compiler.compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const res = try interpret(compile_output, alloc);
    const opcode = compiler.OpCode{ .raw = res };
    try std.testing.expectEqual(.Int, opcode.type_of());
    try std.testing.expectEqual(2, opcode.int.val);
}

test native_itoa {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(itoa 2)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compiler.compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const res = try interpret(compile_output, alloc);
    const opcode = compiler.OpCode{ .raw = res };
    try std.testing.expectEqual(.Char, opcode.type_of());
    try std.testing.expectEqual('2', opcode.char.val);
}

test "let" {
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    const text: [:0]const u8 = "(let ((a 3)) a)";
    const ast = try parser.scheme_parse(text, alloc);
    const compile_output = try compiler.compile(&ast, alloc);
    const res = try interpret(compile_output, alloc);
    const opcode = compiler.OpCode{ .raw = res };
    try std.testing.expectEqual(.Int, opcode.type_of());
    try std.testing.expectEqual(3, opcode.int.val);
}
