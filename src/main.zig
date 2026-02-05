const std = @import("std");
const oyster = @import("oyster");
const parser = oyster.parser;
const compiler = oyster.compiler;
const runtime = oyster.runtime;

pub fn main() !void {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    const alloc = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) {
            @panic("MEMORY LEAK");
        }
    }
    const text: [:0]const u8 = "(inc 2)";
    var ast = try parser.scheme_parse(text, alloc);
    defer ast.deinit(alloc);
    var compile_output = try compiler.compile(&ast, alloc);
    defer compile_output.deinit(alloc);
    for (compile_output.code) |op| {
        op.print();
    }
    const res = try runtime.interpret(compile_output, alloc);
    std.debug.print("Return: {d}\n", .{res});
}
