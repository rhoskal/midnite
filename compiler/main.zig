const std = @import("std");

const lexer = @import("frontend/lexer.zig");
const parser = @import("frontend/parser.zig");
const diagnostics = @import("frontend/diagnostics.zig");
const ast_printer = @import("frontend/ast_printer.zig");

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

    // First lexer instance for token printing
    var token_lexer = lexer.Lexer.init(source, file);
    const stderr = std.io.getStdErr().writer();
    const stdout = std.io.getStdOut().writer();

    try stdout.print("\n=== Tokens ===\n", .{});

    while (true) {
        const token = token_lexer.nextToken() catch |err| {
            const report = diagnostics.Diagnostic.create(err, token_lexer.loc, token_lexer.source);
            try report.format(stderr);
            std.process.exit(1);
        };

        if (std.meta.eql(token.kind, .{ .special = .Eof })) break;

        try stdout.print("Token: {s} ({any})\n", .{ token.lexeme, token.kind });
    }

    // Second lexer instance for parsing
    var parse_lexer = lexer.Lexer.init(source, file);
    const p = try parser.Parser.init(allocator, &parse_lexer);
    const ast = try p.parseProgram();
    defer ast.deinit(allocator);

    try stdout.print("\n=== AST ===\n", .{});

    var printer = ast_printer.AstPrinter.init(allocator, std.io.getStdOut().writer());
    try printer.printNode(ast);

    // try frontend.semantic_analyzer.analyze(ast);
    // try backend.type_checker.check(ast);
    // try backend.codegen.generate(ast);
}
