const std = @import("std");

pub const frontend = struct {
    pub const ast = @import("frontend/ast.zig");
    pub const lexer = @import("frontend/lexer.zig");
    pub const parser = @import("frontend/parser.zig");
};

const ast_printer = @import("frontend/ast_printer.zig");

pub const CompileError = error{
    FileNotFound,
    OutOfMemory,
} || std.fs.File.OpenError || std.fs.File.ReadError || frontend.lexer.LexerError || frontend.parser.ParserError;

pub fn compile(allocator: std.mem.Allocator, writer: anytype, filepath: []const u8) !void {
    const source = try std.fs.cwd().readFileAlloc(allocator, filepath, 1024 * 1024);
    defer allocator.free(source);

    {
        // First pass: print tokens
        var lexer = frontend.lexer.Lexer.init(source, filepath);
        try writer.print("\n=== Tokens ===\n", .{});

        while (true) {
            const token = try lexer.nextToken();

            if (std.meta.eql(token.kind, .{ .special = .Eof })) break;

            try writer.print("{any}\n", .{token});
        }
    }

    {
        // Second pass: parse and print AST
        var lexer = frontend.lexer.Lexer.init(source, filepath);
        const parser = try frontend.parser.Parser.init(allocator, &lexer);
        defer parser.deinit();

        const ast = try parser.parseProgram();
        defer {
            ast.deinit(allocator);
            allocator.destroy(ast);
        }

        try writer.print("\n=== AST ===\n", .{});
        var printer = ast_printer.AstPrinter.init(allocator, writer);
        try printer.printNode(ast);
    }

    // try frontend.semantic_analyzer.analyze(ast);
    // try backend.type_checker.check(ast);
    // try backend.codegen.generate(ast);
}
