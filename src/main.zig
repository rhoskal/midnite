const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const source = try std.fs.cwd().readFileAlloc(allocator, "examples/keywords.mox", 1024 * 1024);
    defer allocator.free(source);

    const tokens = try lexer.tokenize(source);
    for (tokens) |token| {
        std.debug.print("Token: {s} {s}\n", .{
            @tagName(token.kind),
            token.lexeme,
        });
    }

    // const ast = try parser.parse(tokens);
    // std.debug.print("AST: {}\n", .{ast});
}
