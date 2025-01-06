const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

fn showErrorContext(source: []const u8, line: usize, column: usize) void {
    var current_line: usize = 1;
    var last_newline: usize = 0;
    var i: usize = 0;

    // Find the start of the error line
    while (i < source.len) : (i += 1) {
        if (current_line == line) break;
        if (source[i] == '\n') {
            current_line += 1;
            last_newline = i + 1;
        }
    }

    // Find the end of the error line
    var line_end = last_newline;
    while (line_end < source.len and source[line_end] != '\n') : (line_end += 1) {}

    // Print the line with error
    const line_content = source[last_newline..line_end];
    std.debug.print("\n{d} | {s}\n", .{ line, line_content });

    // Print the error pointer
    std.debug.print("  | ", .{});
    for (0..column - 1) |_| {
        std.debug.print(" ", .{});
    }
    std.debug.print("^\n", .{});
}

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
        const token = tokenizer.nextToken() catch |err| {
            switch (err) {
                error.InvalidCharacter => {
                    std.debug.print("\nError: Invalid character at line {d}, column {d}\n", .{ tokenizer.line, tokenizer.column });
                    showErrorContext(source, tokenizer.line, tokenizer.column);

                    return err;
                },
                error.UnterminatedString => {
                    std.debug.print("\nError: Unterminated string starting at line {d}, column {d}\n", .{ tokenizer.line, tokenizer.column });
                    showErrorContext(source, tokenizer.line, tokenizer.column);

                    return err;
                },
                else => return err,
            }
        };

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
