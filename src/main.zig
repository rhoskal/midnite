const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

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

    const filepath = args[1];

    const source = std.fs.cwd().readFileAlloc(allocator, filepath, 1024 * 1024) catch |err| {
        std.debug.print("Error reading file '{s}': {any}\n", .{ filepath, err });
        return err;
    };

    var tokenizer = lexer.Lexer.init(source);

    var tokens = std.ArrayList(lexer.Token).init(allocator);
    defer tokens.deinit();

    while (true) {
        const token = try tokenizer.nextToken();
        try tokens.append(token);

        if (token.kind == .Eof) break;
    }

    std.debug.print("\nTokens from {s}:\n", .{filepath});
    std.debug.print("----------------------------------------\n", .{});
    for (tokens.items) |token| {
        std.debug.print("{d:>4}:{d:<4} {s:<20} '{s}'\n", .{
            token.line,
            token.column,
            @tagName(token.kind),
            token.lexeme,
        });
    }

    // const ast = try parser.parse(tokens);
    // std.debug.print("AST: {}\n", .{ast});
}
