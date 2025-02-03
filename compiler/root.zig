const std = @import("std");

pub const frontend = struct {
    pub const ast = @import("frontend/ast.zig");
    pub const parser = @import("frontend/parser.zig");
};

const lexer = @import("frontend/lexer.zig");
const parser = @import("frontend/parser.zig");
const ast_printer = @import("frontend/ast_printer.zig");

pub const CompileError = error{
    FileNotFound,
    OutOfMemory,
} || std.fs.File.OpenError || std.fs.File.ReadError || lexer.LexerError || parser.ParserError;

pub fn compile(allocator: std.mem.Allocator, writer: anytype, filepath: []const u8) !void {
    const source = try std.fs.cwd().readFileAlloc(allocator, filepath, 1024 * 1024);
    defer allocator.free(source);

    // First pass: print tokens
    var token_lexer = lexer.Lexer.init(source, filepath);
    try writer.print("\n=== Tokens ===\n", .{});

    while (true) {
        const token = try token_lexer.nextToken();
        if (std.meta.eql(token.kind, .{ .special = .Eof })) break;
        try writer.print("Token: {s} ({any})\n", .{ token.lexeme, token.kind });
    }

    // Second pass: parse and print AST
    var parse_lexer = lexer.Lexer.init(source, filepath);
    const p = try parser.Parser.init(allocator, &parse_lexer);
    const ast = try p.parseProgram();
    defer ast.deinit(allocator);

    try writer.print("\n=== AST ===\n", .{});
    var printer = ast_printer.AstPrinter.init(allocator, writer);
    try printer.printNode(ast);

    // try frontend.semantic_analyzer.analyze(ast);
    // try backend.type_checker.check(ast);
    // try backend.codegen.generate(ast);
}
