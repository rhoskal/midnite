const std = @import("std");

const ast = @import("compiler").frontend.ast;
const Formatter = @import("formatter.zig").Formatter;

pub const FormatError = error{
    OutOfMemory,
};

/// Formats an AST node into a string according to the language's style rules.
pub fn format(allocator: std.mem.Allocator, node: *const ast.Node) FormatError![]const u8 {
    var formatter = Formatter.init(allocator);
    defer formatter.deinit();

    try formatter.formatNode(node);

    return formatter.writer.context.toOwnedSlice();
}
