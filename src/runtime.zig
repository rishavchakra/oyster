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
    statics: []u8,
    heap: *std.ArrayList(u8),
};
const Func = *const fn (u64, Env, std.mem.Allocator) anyerror!void;

const HeapTag = enum(u8) {
    Cons,
    String,
    Vec,
};

pub fn interpret(bytecode: compiler.CompileOutput, alloc: std.mem.Allocator) !u64 {
    const opcodes = bytecode.code;
    var pc: usize = 0;

    var stack: Stack = try .initCapacity(alloc, 64);
    defer stack.deinit(alloc);

    var heap: std.ArrayList(u8) = try .initCapacity(alloc, 1024);
    defer heap.deinit(alloc);

    var bindings: BindingMap = .init(alloc);
    try init_bindings(&bindings, alloc);
    defer deinit_bindings(&bindings, alloc);

    var binding_stack: Stack = try .initCapacity(alloc, 64);
    defer binding_stack.deinit(alloc);

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
                        const binding_ind: usize = opcodes[pc + 1].binding.val;
                        const binding_len = bytecode.statics[binding_ind - 1];
                        const binding_name = bytecode.statics[binding_ind .. binding_ind + binding_len];
                        const val_list = bindings.get(binding_name);
                        if (val_list == null) {
                            std.debug.print("UNBOUND BINDING: \"{s}\"\n", .{binding_name});
                            break :run;
                        }
                        const binding = val_list.?.getLast();
                        const arity = opcodes[pc + 2].raw;
                        const env = Env{
                            .stack = &stack,
                            .bindings = &bindings,
                            .statics = bytecode.statics,
                            .heap = &heap,
                        };
                        switch (binding) {
                            .stack_ind => |ind| {
                                // TODO: fix this with the new binding stack
                                try stack.append(alloc, stack.items[ind]);
                                pc += 2; // No arity to jump over, just .Eval and binding ind
                            },
                            .native => |fptr| {
                                // Function pointer will handle leaving return values on stack
                                try fptr(arity, env, alloc);
                                pc += 3; // .Eval, binding ind, arity count
                            },
                            .reserved => |fptr| {
                                // Function pointer will handle leaving return values on stack
                                try fptr(arity, env, alloc);
                                pc += 3; // .Eval, binding ind, arity count
                            },
                            .static => unreachable,
                            // unreachable for now,
                            // this is for things like string constants
                            // note: or not.
                        }
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
                    .Unset => {
                        const num_unset = opcodes[pc + 1].raw;
                        // const res = stack.pop().?;
                        for (0..num_unset) |_| {
                            // _ = stack.pop().?; // The binding's value
                            const binding = binding_stack.pop().?; // The binding pointer
                            const binding_ind: usize = binding.binding.val;
                            const binding_len = bytecode.statics[binding_ind - 1];
                            const binding_name = bytecode.statics[binding_ind .. binding_ind + binding_len];
                            var binding_list = bindings.getPtr(binding_name).?;
                            const stack_ind = binding_list.pop().?.stack_ind;
                            if (binding_list.items.len == 0) {
                                // Remove the binding list altogether
                                binding_list.clearAndFree(alloc);
                                _ = bindings.remove(binding_name);
                            }

                            _ = stack.orderedRemove(stack_ind);
                        }
                        // try stack.append(alloc, res);
                        pc += 2;
                    },
                    .Set => {
                        if (opcodes[pc + 1].raw == std.math.maxInt(u64)) {
                            // Handle: return value
                            return stack.pop().?.raw;
                        }

                        // Handle setting a binding to a new value
                        const binding = opcodes[pc + 1].binding;
                        const binding_name_ind = opcodes[pc + 1].binding.val;
                        const binding_name_len = bytecode.statics[binding_name_ind - 1];
                        const binding_name = bytecode.statics[binding_name_ind .. binding_name_ind + binding_name_len];
                        var found_binding = try bindings.getOrPut(binding_name);
                        if (found_binding.found_existing) {
                            // Have a list, add the binding onto it
                            try found_binding.value_ptr.append(alloc, Binding{ .stack_ind = stack.items.len - 1 });
                        } else {
                            // Make a new list, put it on the binding
                            var binding_list = try BindingList.initCapacity(alloc, 1);
                            try binding_list.append(alloc, Binding{ .stack_ind = stack.items.len - 1 });
                            found_binding.value_ptr.* = binding_list;
                        }

                        // Push the binding back onto the stack
                        try binding_stack.append(alloc, compiler.OpCode{ .binding = binding });
                        pc += 2;
                    },
                    .Squash => {
                        const num_squash = opcodes[pc + 1].raw;
                        const top = stack.pop().?;
                        for (0..num_squash) |_| {
                            _ = stack.pop().?;
                        }
                        try stack.append(alloc, top);
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
    var plus_bindings = try BindingList.initCapacity(alloc, 1);
    var atoi_bindings = try BindingList.initCapacity(alloc, 1);
    var itoa_bindings = try BindingList.initCapacity(alloc, 1);
    var is_null_bindings = try BindingList.initCapacity(alloc, 1);
    var is_int_bindings = try BindingList.initCapacity(alloc, 1);
    var is_bool_bindings = try BindingList.initCapacity(alloc, 1);
    var is_zero_bindings = try BindingList.initCapacity(alloc, 1);
    var not_bindings = try BindingList.initCapacity(alloc, 1);
    var cons_bindings = try BindingList.initCapacity(alloc, 1);
    var car_bindings = try BindingList.initCapacity(alloc, 1);
    var cdr_bindings = try BindingList.initCapacity(alloc, 1);
    var string_bindings = try BindingList.initCapacity(alloc, 1);
    var string_getc_bindings = try BindingList.initCapacity(alloc, 1);
    var string_setc_bindings = try BindingList.initCapacity(alloc, 1);
    var string_append_bindings = try BindingList.initCapacity(alloc, 1);
    var vec_bindings = try BindingList.initCapacity(alloc, 1);
    var vec_get_bindings = try BindingList.initCapacity(alloc, 1);
    var vec_set_bindings = try BindingList.initCapacity(alloc, 1);
    var vec_append_bindings = try BindingList.initCapacity(alloc, 1);
    var begin_bindings = try BindingList.initCapacity(alloc, 1);

    try inc_bindings.append(alloc, Binding{ .native = &native_inc });
    try dec_bindings.append(alloc, Binding{ .native = &native_dec });
    try plus_bindings.append(alloc, Binding{ .native = &native_plus });
    try atoi_bindings.append(alloc, Binding{ .native = &native_atoi });
    try itoa_bindings.append(alloc, Binding{ .native = &native_itoa });
    try is_null_bindings.append(alloc, Binding{ .native = &native_is_null });
    try is_int_bindings.append(alloc, Binding{ .native = &native_is_int });
    try is_bool_bindings.append(alloc, Binding{ .native = &native_is_bool });
    try is_zero_bindings.append(alloc, Binding{ .native = &native_is_zero });
    try not_bindings.append(alloc, Binding{ .native = &native_not });
    try cons_bindings.append(alloc, Binding{ .native = &native_cons });
    try car_bindings.append(alloc, Binding{ .native = &native_car });
    try cdr_bindings.append(alloc, Binding{ .native = &native_cdr });
    try string_bindings.append(alloc, Binding{ .native = &native_string });
    try string_getc_bindings.append(alloc, Binding{ .native = &native_string_getc });
    try string_setc_bindings.append(alloc, Binding{ .native = &native_string_setc });
    try string_append_bindings.append(alloc, Binding{ .native = &native_string_append });
    try vec_bindings.append(alloc, Binding{ .native = &native_vec });
    try vec_get_bindings.append(alloc, Binding{ .native = &native_vec_get });
    try vec_set_bindings.append(alloc, Binding{ .native = &native_vec_set });
    try vec_append_bindings.append(alloc, Binding{ .native = &native_vec_append });
    try begin_bindings.append(alloc, Binding{ .native = &native_begin });

    try bindings.put("inc", inc_bindings);
    try bindings.put("dec", dec_bindings);
    try bindings.put("+", plus_bindings);
    try bindings.put("atoi", atoi_bindings);
    try bindings.put("itoa", itoa_bindings);
    try bindings.put("is-null", is_null_bindings);
    try bindings.put("is-int", is_int_bindings);
    try bindings.put("is-bool", is_bool_bindings);
    try bindings.put("is-zero", is_zero_bindings);
    try bindings.put("not", not_bindings);
    try bindings.put("cons", cons_bindings);
    try bindings.put("car", car_bindings);
    try bindings.put("cdr", cdr_bindings);
    try bindings.put("string", string_bindings);
    try bindings.put("string-ref", string_getc_bindings);
    try bindings.put("string-set", string_setc_bindings);
    try bindings.put("string-append", string_append_bindings);
    try bindings.put("vec", vec_bindings);
    try bindings.put("vec-get", vec_get_bindings);
    try bindings.put("vec-set", vec_set_bindings);
    try bindings.put("vec-append", vec_append_bindings);
    try bindings.put("begin", begin_bindings);
}

pub fn deinit_bindings(bindings: *BindingMap, alloc: std.mem.Allocator) void {
    var binding_lists = bindings.iterator();
    while (binding_lists.next()) |ls| {
        ls.value_ptr.deinit(alloc);
    }
    bindings.deinit();
}

//================ Native Functions ================

fn native_inc(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
    const arg = env.stack.pop().?;
    if (arg.type_of() != .Int) {
        // return error{};
    }
    try env.stack.append(alloc, compiler.OpCode{ .int = compiler.OpInt.init(arg.int.val + 1) });
}

fn native_dec(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
    const arg = env.stack.pop().?;
    if (arg.type_of() != .Int) {
        // return error{};
    }
    try env.stack.append(alloc, compiler.OpCode{ .int = compiler.OpInt.init(arg.int.val - 1) });
}

fn native_plus(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    var sum: i62 = 0;
    var sumf: f32 = 0.0;
    var is_f = false;
    for (0..arity) |_| {
        const arg = env.stack.pop().?;
        switch (arg.type_of()) {
            .Int => {
                sum += arg.int.val;
                sumf += @floatFromInt(arg.int.val);
            },
            .Char => {
                sum += arg.char.val;
                sumf += @floatFromInt(arg.char.val);
            },
            .Bool => {
                sum += @intFromBool(arg.boolean.val);
                sumf += @floatFromInt(@intFromBool(arg.boolean.val));
            },
            .Float => {
                sumf += arg.float.val;
                is_f = true;
            },
            .Raw => {
                sum += @intCast(arg.raw);
                sumf += @floatFromInt(arg.raw);
            },
            else => {
                std.debug.print("WARNING: plus encountered non-numeric value\n", .{});
            },
        }
    }
    try env.stack.append(alloc, compiler.OpCode{ .int = compiler.OpInt.init(sum) });
}

fn native_atoi(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
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

fn native_itoa(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
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

fn native_is_null(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
    _ = env;
    _ = alloc;
    // I've been testing is_null on Chicken Scheme
    // and I can't find a single value that makes this true...
}

fn native_is_zero(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
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

fn native_is_int(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
    const arg = env.stack.pop().?;
    const arg_type = arg.type_of();
    try env.stack.append(alloc, compiler.OpCode{ .boolean = compiler.OpBool.init(arg_type == .Int) });
}

fn native_is_bool(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
    const arg = env.stack.pop().?;
    const arg_type = arg.type_of();
    try env.stack.append(alloc, compiler.OpCode{ .boolean = compiler.OpBool.init(arg_type == .Bool) });
}

fn native_not(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
    const arg = env.stack.pop().?;
    const arg_type = arg.type_of();
    if (arg_type == .Bool) {
        try env.stack.append(alloc, compiler.OpCode{ .boolean = compiler.OpBool.init(!arg.boolean.val) });
    }
}

fn native_cons(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
    try env.heap.append(alloc, @intFromEnum(HeapTag.Cons));

    const cdr = env.stack.pop().?.raw;
    const car = env.stack.pop().?.raw;

    const heap_ind = env.heap.items.len;
    try env.heap.appendNTimes(alloc, 0, 16);
    const car_slice: *[8]u8 = @ptrCast(env.heap.items[heap_ind .. heap_ind + 8]);
    const cdr_slice: *[8]u8 = @ptrCast(env.heap.items[heap_ind + 8 .. heap_ind + 16]);
    std.mem.writeInt(u64, car_slice, car, .little);
    std.mem.writeInt(u64, cdr_slice, cdr, .little);
    try env.stack.append(alloc, compiler.OpCode{ .heap_ptr = compiler.OpHeapPtr.init(@truncate(heap_ind)) });
}

fn native_car(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
    const arg = env.stack.pop().?;
    const heap_ind = arg.heap_ptr.val;
    const heap_slice = env.heap.items[heap_ind .. heap_ind + 8];
    const val = std.mem.readInt(u64, @ptrCast(heap_slice), .little); // TODO: check on endianness
    try env.stack.append(alloc, compiler.OpCode{ .raw = val });
}

fn native_cdr(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
    const arg = env.stack.pop().?;
    const heap_ind = arg.heap_ptr.val;
    const heap_slice = env.heap.items[heap_ind + 8 .. heap_ind + 16];
    const val = std.mem.readInt(u64, @ptrCast(heap_slice), .little); // TODO: check on endianness
    try env.stack.append(alloc, compiler.OpCode{ .raw = val });
}

fn native_string(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    // First the string length,
    const heap_strlen_ind = env.heap.items.len;
    try env.heap.appendNTimes(alloc, 0, @sizeOf(u32));
    const heap_strlen_slice: *[@sizeOf(u32)]u8 = @ptrCast(env.heap.items[heap_strlen_ind..]);
    std.mem.writeInt(u32, heap_strlen_slice, @truncate(arity), .little);

    // Then the type tag,
    try env.heap.append(alloc, @intFromEnum(HeapTag.String));

    const heap_str_ind = env.heap.items.len;
    for (0..arity) |i| {
        const c = env.stack.items[env.stack.items.len - arity + i].char.val;
        try env.heap.append(alloc, c);
    }
    env.stack.shrinkRetainingCapacity(env.stack.items.len - arity); // Pop off all the arguments

    // And finally the string itself
    try env.stack.append(alloc, compiler.OpCode{ .heap_ptr = compiler.OpHeapPtr.init(@truncate(heap_str_ind)) });
}

fn native_string_getc(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
    const ref_ind = env.stack.pop().?.int.val;
    const str_ind = env.stack.pop().?.heap_ptr.val;
    const strlen_slice = env.heap.items[str_ind - 4 .. str_ind];
    const strlen = std.mem.readInt(u32, @ptrCast(strlen_slice), .little);
    // TODO: proper type checking + error handling

    if (ref_ind < 0 or ref_ind >= strlen) {
        std.debug.print("ERROR: string index out of bounds\n", .{});
        return; // TODO: return a proper error
    }

    const heap_ref_ind: usize = @intCast(str_ind + ref_ind);
    const char = env.heap.items[heap_ref_ind];
    try env.stack.append(alloc, compiler.OpCode{ .char = compiler.OpChar.init(char) });
}

fn native_string_setc(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
    const new_char = env.stack.pop().?.char.val;
    const ref_ind = env.stack.pop().?.int.val;
    const str_ind = env.stack.pop().?.heap_ptr.val;
    const strlen_slice = env.heap.items[str_ind - 4 .. str_ind];
    const strlen = std.mem.readInt(u32, @ptrCast(strlen_slice), .little);
    // TODO: proper type checking + error handling

    if (ref_ind < 0 or ref_ind >= strlen) {
        std.debug.print("ERROR: string index out of bounds\n", .{});
        try env.stack.append(alloc, compiler.OpCode{ .boolean = compiler.OpBool.init(false) });
        return;
    }

    const heap_ref_ind: usize = @intCast(str_ind + ref_ind);
    env.heap.items[heap_ref_ind] = new_char;
    try env.stack.append(alloc, compiler.OpCode{ .boolean = compiler.OpBool.init(true) });
}

fn native_string_append(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
    _ = env;
    _ = alloc;
}

fn native_vec(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    // First the length,
    const heap_veclen_ind = env.heap.items.len;
    try env.heap.appendNTimes(alloc, 0, 4);
    const veclen_slice: *[4]u8 = @ptrCast(env.heap.items[heap_veclen_ind .. heap_veclen_ind + 4]);
    std.mem.writeInt(u32, @ptrCast(veclen_slice), @truncate(arity), .little);

    // Then the type tag,
    try env.heap.append(alloc, @intFromEnum(HeapTag.Vec));

    // And finally the elems
    const vec_ind = env.heap.items.len;
    for (0..arity) |i| {
        const elem = env.stack.items[env.stack.items.len - arity + i].raw;
        const heap_ind = env.heap.items.len;
        try env.heap.appendNTimes(alloc, 0, 8);
        const heap_slice: *[8]u8 = @ptrCast(env.heap.items[heap_ind .. heap_ind + 8]);
        std.mem.writeInt(u64, heap_slice, elem, .little);
    }
    env.stack.shrinkRetainingCapacity(env.stack.items.len - arity);
    try env.stack.append(alloc, compiler.OpCode{ .heap_ptr = compiler.OpHeapPtr.init(@truncate(vec_ind)) });
}

fn native_vec_get(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
    const ref_ind = env.stack.pop().?.int.val;
    const vec_ind = env.stack.pop().?.heap_ptr.val;
    const vec_len_slice = env.heap.items[vec_ind - 5 .. vec_ind - 1]; // Skip over the type tag for now
    const vec_len = std.mem.readInt(u32, @ptrCast(vec_len_slice), .little);

    if (ref_ind < 0 or ref_ind >= vec_len) {
        std.debug.print("ERROR: vec index out of bounds\n", .{});
        return; // TODO: return a proper error
    }

    const heap_ref_ind: usize = @as(usize, @intCast(vec_ind + (ref_ind * 8)));
    const elem_slice = env.heap.items[heap_ref_ind .. heap_ref_ind + 8];
    const elem_raw = std.mem.readInt(u64, @ptrCast(elem_slice), .little);
    try env.stack.append(alloc, compiler.OpCode{ .raw = elem_raw });
}

fn native_vec_set(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;

    const new_elem = env.stack.pop().?;
    const ref_ind = env.stack.pop().?.int.val;
    const vec_ind = env.stack.pop().?.heap_ptr.val;
    const vec_len_slice = env.heap.items[vec_ind - 5 .. vec_ind - 1];
    const vec_len = std.mem.readInt(u32, @ptrCast(vec_len_slice), .little);

    if (ref_ind < 0 or ref_ind > vec_len - 1) {
        std.debug.print("ERROR: vec index out of bounds\n", .{});
        try env.stack.append(alloc, compiler.OpCode{ .boolean = compiler.OpBool.init(false) });
        return; // TODO: return a proper error
    }

    const heap_ref_ind: usize = @as(usize, @intCast(vec_ind + (ref_ind * 8)));
    const elem_slice = env.heap.items[heap_ref_ind .. heap_ref_ind + 8];
    std.mem.writeInt(u64, @ptrCast(elem_slice), new_elem.raw, .little);
    try env.stack.append(alloc, compiler.OpCode{ .boolean = compiler.OpBool.init(true) });
}

fn native_vec_append(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
    _ = env;
    _ = alloc;
}

fn native_begin(arity: u64, env: Env, alloc: std.mem.Allocator) !void {
    _ = arity;
    _ = env;
    _ = alloc;
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
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(let ((a 3) (b 5)) a)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compiler.compile(&ast, alloc);
    defer compile_output.deinit(alloc);

    const res = try interpret(compile_output, alloc);
    const opcode = compiler.OpCode{ .raw = res };
    try std.testing.expectEqual(.Int, opcode.type_of());
    try std.testing.expectEqual(3, opcode.int.val);
}

test native_car {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(car (cons (let ((a 3)) a) 5))";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compiler.compile(&ast, alloc);
    defer compile_output.deinit(alloc);

    const res = try interpret(compile_output, alloc);
    const opcode = compiler.OpCode{ .raw = res };
    try std.testing.expectEqual(.Int, opcode.type_of());
    try std.testing.expectEqual(3, opcode.int.val);
}

test native_cdr {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(cdr (cons (let ((a 3)) a) 5))";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compiler.compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const res = try interpret(compile_output, alloc);
    const opcode = compiler.OpCode{ .raw = res };
    try std.testing.expectEqual(.Int, opcode.type_of());
    try std.testing.expectEqual(5, opcode.int.val);
}

test native_string_getc {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(string-ref (string #\\a #\\b #\\c #\\d #\\e) 3)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compiler.compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const res = try interpret(compile_output, alloc);
    const opcode = compiler.OpCode{ .raw = res };
    try std.testing.expectEqual(.Char, opcode.type_of());
    try std.testing.expectEqual('d', opcode.char.val);
}

test native_string_setc {
    const alloc = std.testing.allocator;
    // bangular
    const text: [:0]const u8 = "(let ((a (string #\\a #\\n #\\g #\\u #\\l #\\a #\\r))) (string-set a 0 #\\b) (string-ref a 0))";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compiler.compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const res = try interpret(compile_output, alloc);
    const opcode = compiler.OpCode{ .raw = res };
    try std.testing.expectEqual(.Char, opcode.type_of());
    try std.testing.expectEqual('b', opcode.char.val);
}

test native_string_append {}

test native_vec_get {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(vec-get (vec (let ((a 3)) a) 5) 1)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compiler.compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const res = try interpret(compile_output, alloc);
    const opcode = compiler.OpCode{ .raw = res };
    try std.testing.expectEqual(.Int, opcode.type_of());
    try std.testing.expectEqual(5, opcode.int.val);
}

test native_vec_set {
    const alloc = std.testing.allocator;
    const text: [:0]const u8 = "(let ((a (vec 0 1 2 3))) (vec-set a 1 6) (+ (vec-get a 1) (vec-get a 2)))";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compiler.compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    const res = try interpret(compile_output, alloc);
    const opcode = compiler.OpCode{ .raw = res };
    try std.testing.expectEqual(.Int, opcode.type_of());
    try std.testing.expectEqual(8, opcode.int.val);
}

// test native_vec_append {}
//
// test native_begin {}
