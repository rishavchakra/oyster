pub const parser = @import("parser.zig");
pub const compiler = @import("compiler.zig");
pub const runtime = @import("runtime.zig");

test {
    _ = @import("parser.zig");
    _ = @import("compiler.zig");
    _ = @import("runtime.zig");
}
