const std = @import("std");

const compiler = @import("compiler");
const Formatter = @import("formatter.zig").Formatter;

// pub const FormatError = error{
//     OutOfMemory,
//     FileNotFound,
// } || std.fs.File.OpenError || std.fs.File.ReadError || compiler.frontend.parser.ParserError;

/// Formats an AST node into a string according to the language's style rules.
pub fn format(allocator: std.mem.Allocator, filepath: []const u8) ![]const u8 {
    const source = try std.fs.cwd().readFileAlloc(allocator, filepath, 1024 * 1024);
    defer allocator.free(source);

    var lexer = compiler.frontend.lexer.Lexer.init(source, filepath);
    var parser = try compiler.frontend.parser.Parser.init(allocator, &lexer);
    defer parser.deinit();

    const ast = try parser.parseProgram();
    defer {
        ast.release(allocator);
        allocator.destroy(ast);
    }

    var formatter = Formatter.init(allocator);

    try formatter.formatNode(ast);

    return formatter.writer.context.toOwnedSlice();
}
