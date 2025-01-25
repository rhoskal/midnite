const std = @import("std");

const lexer = @import("frontend/lexer.zig");
const parser = @import("frontend/parser.zig");
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

    var l = lexer.Lexer.init(source, file);
    const stderr = std.io.getStdErr().writer();

    while (true) {
        const token = l.nextToken() catch |err| {
            const report = diagnostics.Diagnostic.create(err, l.loc, l.source);
            try report.format(stderr);
            std.process.exit(1);
        };

        if (std.meta.eql(token.kind, .{ .special = .Eof })) break;

        const stdout = std.io.getStdOut().writer();
        try stdout.print("Token: {s} ({any})\n", .{ token.lexeme, token.kind });
    }

    const p = try parser.Parser.init(allocator, &l);
    const ast = try p.parseProgram();
    std.debug.print("\n{}\n", .{ast});
}
