const std = @import("std");

const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const diagnostics = @import("diagnostics.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} <filepath>\n", .{args[0]});
        std.process.exit(1);
    }

    const file = args[1];

    const source = std.fs.cwd().readFileAlloc(allocator, file, 1024 * 1024) catch |err| {
        std.debug.print("Error reading file '{s}': {any}\n", .{ file, err });
        return err;
    };

    var lex = lexer.Lexer.init(source, file);
    const stderr = std.io.getStdErr().writer();

    while (true) {
        const token = lex.nextToken() catch |err| {
            const report = diagnostics.Diagnostic.create(err, lex.loc, lex.source);
            try report.format(stderr);
            std.process.exit(1);
        };

        if (token.kind == .Eof) break;

        const stdout = std.io.getStdOut().writer();
        try stdout.print("Token: {s} ({any})\n", .{ token.lexeme, token.kind });
    }

    // const ast = try parser.parse(tokens);
    // std.debug.print("AST: {}\n", .{ast});
}
