const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");

const ParserError = error{
    UnexpectedToken,
} || std.mem.Allocator.Error || lexer.LexerError;

const Parser = struct {
    lex: *lexer.Lexer,
    allocator: std.mem.Allocator,
    current_token: lexer.Token,

    /// Initializes a new Parser instance.
    ///
    /// - `lex`: Pointer to the lexer instance.
    /// - `allocator`: Memory allocator for the parser.
    pub fn init(lex: *lexer.Lexer, allocator: std.mem.Allocator) !*Parser {
        const parser = try allocator.create(Parser);
        const first_token = try lex.nextToken();

        parser.* = .{
            .lex = lex,
            .allocator = allocator,
            .current_token = first_token,
        };

        return parser;
    }

    /// Frees resources associated with the parser instance.
    pub fn deinit(self: *Parser) void {
        self.allocator.destroy(self);
    }

    /// Advances to the next token in the input stream.
    fn advance(self: *Parser) ParserError!void {
        self.current_token = try self.lex.nextToken();
    }

    /// Checks whether the current token matches a specific kind without consuming it.
    ///
    /// - `kind`: The kind of token to check.
    fn check(self: *Parser, kind: lexer.TokenKind) bool {
        return self.current_token.kind == kind;
    }

    /// Checks and consumes if matches.
    ///
    /// - `kind`: The kind of token to check.
    fn match(self: *Parser, kind: lexer.TokenKind) ParserError!bool {
        if (self.check(kind)) {
            try self.advance();

            return true;
        }

        return false;
    }

    /// Ensures the current token matches a specific kind, consuming it if true.
    /// Errors if the token does not match.
    ///
    /// `kind`: The kind of token to check.
    fn expect(self: *Parser, kind: lexer.TokenKind) ParserError!lexer.Token {
        if (self.check(kind)) {
            const current_token = self.current_token;
            try self.advance();

            return current_token;
        }

        return error.UnexpectedToken;
    }

    /// Parses a regular comment node from the input.
    pub fn parseComment(self: *Parser) ParserError!ast.CommentNode {
        const token = try self.expect(lexer.TokenKind.Comment);

        return ast.CommentNode{
            .content = token.lexeme,
            .token = token,
        };
    }

    /// Parses a documentation comment node from the input.
    pub fn parseDocComment(self: *Parser) ParserError!ast.DocCommentNode {
        const token = try self.expect(lexer.TokenKind.DocComment);

        return ast.DocCommentNode{
            .content = token.lexeme,
            .token = token,
        };
    }
};

const testing = std.testing;

const TEST_FILE = "test.mox";

test "[comment]" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "# This is a regular comment";
    var lex = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(&lex, allocator);
    defer parser.deinit();

    const comment = try parser.parseComment();
    try testing.expectEqualStrings(source, comment.content);
    try testing.expectEqual(lexer.TokenKind.Comment, comment.token.kind);
    try testing.expectEqual(@as(usize, 1), comment.token.loc.src.line);
    try testing.expectEqual(@as(usize, 1), comment.token.loc.src.col);
}

test "[doc comment]" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "## This is a doc comment";
    var lex = lexer.Lexer.init(source, "test.mox");
    var parser = try Parser.init(&lex, allocator);
    defer parser.deinit();

    const comment = try parser.parseDocComment();
    try testing.expectEqualStrings(source, comment.content);
    try testing.expectEqual(lexer.TokenKind.DocComment, comment.token.kind);
    try testing.expectEqual(@as(usize, 1), comment.token.loc.src.line);
    try testing.expectEqual(@as(usize, 1), comment.token.loc.src.col);
}
