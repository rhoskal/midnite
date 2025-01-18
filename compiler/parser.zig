const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");

const ParserError =
    lexer.LexerError ||
    std.fmt.ParseIntError ||
    std.fmt.ParseFloatError ||
    std.mem.Allocator.Error ||
    error{
    InvalidCharLiteral,
    InvalidStrLiteral,
    UnexpectedToken,
};

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

    /// Parses an integer literal value into a structured node.
    /// The literal can be in decimal, hexadecimal (0x), binary (0b), or octal (0o) format.
    /// Underscores in numbers are allowed and ignored (e.g., 1_000_000).
    pub fn parseIntLiteral(self: *Parser) ParserError!ast.IntLiteralNode {
        const token = try self.expect(lexer.TokenKind.LitInt);
        const value = try std.fmt.parseInt(i64, token.lexeme, 0);

        return ast.IntLiteralNode{
            .value = value,
            .token = token,
        };
    }

    /// Parses a floating-point literal value into a structured node.
    /// The literal can include decimal points and optional scientific notation (e.g., 1.23e-4).
    /// Underscores in numbers are allowed and ignored (e.g., 3.141_592).
    pub fn parseFloatLiteral(self: *Parser) ParserError!ast.FloatLiteralNode {
        const token = try self.expect(lexer.TokenKind.LitFloat);
        const value = try std.fmt.parseFloat(f64, token.lexeme);

        return ast.FloatLiteralNode{
            .value = value,
            .token = token,
        };
    }

    /// Parses a string literal into a structured node.
    pub fn parseStrLiteral(self: *Parser) !ast.StrLiteralNode {
        const token = try self.expect(lexer.TokenKind.LitString);

        // Strip quotes
        const unquoted = token.lexeme[1 .. token.lexeme.len - 1];

        var result = std.ArrayList(u8).init(self.allocator);
        defer result.deinit();

        var i: usize = 0;
        while (i < unquoted.len) {
            if (unquoted[i] == '\\') {
                const sequence = try self.parseStrEscapeSequence(unquoted[i..]);
                try result.appendSlice(sequence.value);
                self.allocator.free(sequence.value);

                i += sequence.len;
            } else {
                try result.append(unquoted[i]);

                i += 1;
            }
        }

        return ast.StrLiteralNode{
            .value = try self.allocator.dupe(u8, result.items),
            .token = token,
        };
    }

    const EscapeSequence = struct {
        value: []const u8,
        len: usize,
    };

    fn parseStrEscapeSequence(self: *Parser, unquoted: []const u8) !EscapeSequence {
        return switch (unquoted[1]) {
            'n' => .{ .value = try self.allocator.dupe(u8, "\n"), .len = 2 },
            'r' => .{ .value = try self.allocator.dupe(u8, "\r"), .len = 2 },
            't' => .{ .value = try self.allocator.dupe(u8, "\t"), .len = 2 },
            '\\' => .{ .value = try self.allocator.dupe(u8, "\\"), .len = 2 },
            '\"' => .{ .value = try self.allocator.dupe(u8, "\""), .len = 2 },
            'u' => {
                // Handle Unicode escape \u{XXXXXX} where XXXXXX is 1-6 hex digits
                const brace_index = std.mem.indexOfScalar(u8, unquoted[3..], '}') orelse return error.InvalidUnicodeEscape;
                const hex = unquoted[3 .. 3 + brace_index];
                const code_point = try std.fmt.parseInt(u21, hex, 16);

                var utf8_buffer: [4]u8 = undefined;
                const utf8_len = try std.unicode.utf8Encode(code_point, &utf8_buffer);

                // Length is: \u{ + hex digits + }
                const sequence_len = 3 + hex.len + 1;

                return .{
                    .value = try self.allocator.dupe(u8, utf8_buffer[0..utf8_len]),
                    .len = sequence_len,
                };
            },
            else => unreachable,
        };
    }

    /// Parses a character literal into its Unicode codepoint value.
    /// Handles standard characters ('a'), escape sequences ('\n', '\t', etc.),
    /// and Unicode escape sequences ('\u{0061}').
    pub fn parseCharLiteral(self: *Parser) ParserError!ast.CharLiteralNode {
        const token = try self.expect(lexer.TokenKind.LitChar);

        // Lexeme includes the quotes, so it's at least 3 chars: 'x'
        std.debug.assert(token.lexeme.len >= 3);

        const unquoted = token.lexeme[1 .. token.lexeme.len - 1];

        var value: u21 = undefined;
        if (unquoted[0] == '\\') {
            value = try parseCharEscapeSequence(unquoted);
        } else {
            value = std.unicode.utf8Decode(unquoted) catch unreachable;
        }

        return ast.CharLiteralNode{
            .value = value,
            .token = token,
        };
    }

    fn parseCharEscapeSequence(unquoted: []const u8) !u21 {
        return switch (unquoted[1]) {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\',
            '\'' => '\'',
            'u' => {
                // Handle Unicode escape \u{XXXXXX} where XXXXXX is 1-6 hex digits
                // Skip the \u{ and }
                const hex = unquoted[3 .. unquoted.len - 1];

                return try std.fmt.parseInt(u21, hex, 16);
            },
            else => unreachable,
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
    try testing.expectEqual(lexer.TokenKind.Comment, comment.token.kind);
    try testing.expectEqualStrings(source, comment.content);
    try testing.expectEqual(@as(usize, 1), comment.token.loc.src.line);
    try testing.expectEqual(@as(usize, 1), comment.token.loc.src.col);
}

test "[doc comment]" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "## This is a doc comment";
    var lex = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(&lex, allocator);
    defer parser.deinit();

    const comment = try parser.parseDocComment();
    try testing.expectEqual(lexer.TokenKind.DocComment, comment.token.kind);
    try testing.expectEqualStrings(source, comment.content);
    try testing.expectEqual(@as(usize, 1), comment.token.loc.src.line);
    try testing.expectEqual(@as(usize, 1), comment.token.loc.src.col);
}

test "[int literal]" {
    const TestCase = struct {
        source: []const u8,
        expected: i64,
    };

    const cases = [_]TestCase{
        // Base 16 (hex)
        .{ .source = "0xFF", .expected = 255 },
        .{ .source = "0xff", .expected = 255 },
        .{ .source = "0xDEAD_BEEF", .expected = 0xDEADBEEF },
        // Base 10 (decimal)
        .{ .source = "42", .expected = 42 },
        .{ .source = "1_234_567", .expected = 1234567 },
        // Base 8 (octal)
        .{ .source = "0o52", .expected = 42 },
        .{ .source = "0o755", .expected = 493 },
        // Base 2 (binary)
        .{ .source = "0b1010", .expected = 10 },
        .{ .source = "0b1010_1010", .expected = 170 },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (cases) |case| {
        var lex = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(&lex, allocator);
        defer parser.deinit();

        const int = try parser.parseIntLiteral();
        try testing.expectEqual(lexer.TokenKind.LitInt, int.token.kind);
        try testing.expectEqualStrings(case.source, int.token.lexeme);
        try testing.expectEqual(case.expected, int.value);
        try testing.expectEqual(@as(usize, 1), int.token.loc.src.line);
        try testing.expectEqual(@as(usize, 1), int.token.loc.src.col);
    }
}

test "[float literal]" {
    const TestCase = struct {
        source: []const u8,
        expected: f64,
    };

    const cases = [_]TestCase{
        .{ .source = "3.14", .expected = 3.14 },
        .{ .source = "42.0", .expected = 42.0 },
        .{ .source = "1.23e4", .expected = 12300.0 },
        .{ .source = "1.23e-4", .expected = 0.000123 },
        .{ .source = "1_234.567_89", .expected = 1234.56789 },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (cases) |case| {
        var lex = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(&lex, allocator);
        defer parser.deinit();

        const float = try parser.parseFloatLiteral();
        try testing.expectEqual(lexer.TokenKind.LitFloat, float.token.kind);
        try testing.expectEqualStrings(case.source, float.token.lexeme);
        try testing.expectEqual(case.expected, float.value);
        try testing.expectEqual(@as(usize, 1), float.token.loc.src.line);
        try testing.expectEqual(@as(usize, 1), float.token.loc.src.col);
    }
}

test "[str literal]" {
    const TestCase = struct {
        source: []const u8,
        expected: []const u8,
    };

    const cases = [_]TestCase{
        // Empty string
        .{ .source = "\"\"", .expected = "" },

        // Regular strings
        .{ .source = "\"hello\"", .expected = "hello" },
        .{ .source = "\"Hello, World!\"", .expected = "Hello, World!" },
        .{ .source = "\"123\"", .expected = "123" },
        .{ .source = "\"!@#$%^&*()\"", .expected = "!@#$%^&*()" },

        // Strings with spaces
        .{ .source = "\"   \"", .expected = "   " },
        .{ .source = "\"a b c\"", .expected = "a b c" },
        .{ .source = "\"  leading and trailing  \"", .expected = "  leading and trailing  " },

        // Escape sequences
        .{ .source = "\"\\n\"", .expected = "\n" },
        .{ .source = "\"\\r\"", .expected = "\r" },
        .{ .source = "\"\\t\"", .expected = "\t" },
        .{ .source = "\"\\\\\"", .expected = "\\" },
        .{ .source = "\"\\\"\"", .expected = "\"" },

        // Mixed escape sequences
        .{ .source = "\"line1\\nline2\"", .expected = "line1\nline2" },
        .{ .source = "\"tab\\there\"", .expected = "tab\there" },
        .{ .source = "\"quotes: \\\"nested\\\"\"", .expected = "quotes: \"nested\"" },
        .{ .source = "\"\\t\\r\\n\\\\\\\"\"", .expected = "\t\r\n\\\"" },
        .{ .source = "\"line1\\r\\nline2\"", .expected = "line1\r\nline2" },
        .{ .source = "\"line1\\n\\rline2\"", .expected = "line1\n\rline2" },

        // Unicode escape sequences
        .{ .source = "\"\\u{0061}\"", .expected = "a" },
        .{ .source = "\"\\u{1F600}\"", .expected = "😀" },
        .{ .source = "\"hello \\u{1F600} world\"", .expected = "hello 😀 world" },

        // Direct Unicode characters
        .{ .source = "\"Hello 世界\"", .expected = "Hello 世界" },
        .{ .source = "\"café\"", .expected = "café" },
        .{ .source = "\"🌟✨\"", .expected = "🌟✨" },

        // Mixed content
        .{ .source = "\"Hello\\n\\tWorld\\u{1F600}!\"", .expected = "Hello\n\tWorld😀!" },
        .{ .source = "\"Unicode \\u{2764} and emoji ❤\"", .expected = "Unicode ❤ and emoji ❤" },
        .{ .source = "\"Escaped\\tand\\u{1F496}direct💖mixed\"", .expected = "Escaped\tand💖direct💖mixed" },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (cases) |case| {
        var lex = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(&lex, allocator);
        defer parser.deinit();

        const str = try parser.parseStrLiteral();
        try testing.expectEqual(lexer.TokenKind.LitString, str.token.kind);
        try testing.expectEqualStrings(case.source, str.token.lexeme);
        try testing.expectEqualStrings(case.expected, str.value);
        try testing.expectEqual(@as(usize, 1), str.token.loc.src.line);
        try testing.expectEqual(@as(usize, 1), str.token.loc.src.col);
    }
}

test "[char literal]" {
    const TestCase = struct {
        source: []const u8,
        expected: u21,
    };

    const cases = [_]TestCase{
        // Regular characters
        .{ .source = "'a'", .expected = 'a' },
        .{ .source = "'Z'", .expected = 'Z' },
        .{ .source = "'0'", .expected = '0' },
        .{ .source = "'!'", .expected = '!' },

        // Escape sequences
        .{ .source = "'\\n'", .expected = '\n' },
        .{ .source = "'\\r'", .expected = '\r' },
        .{ .source = "'\\t'", .expected = '\t' },
        .{ .source = "'\\\\'", .expected = '\\' },
        .{ .source = "'\\''", .expected = '\'' },

        // Unicode escape sequences
        .{ .source = "'\\u{0061}'", .expected = 0x0061 }, // 'a'
        .{ .source = "'\\u{1F600}'", .expected = 0x1F600 }, // 😀
        .{ .source = "'😀'", .expected = 0x1F600 },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (cases) |case| {
        var lex = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(&lex, allocator);
        defer parser.deinit();

        const char = try parser.parseCharLiteral();
        try testing.expectEqual(lexer.TokenKind.LitChar, char.token.kind);
        try testing.expectEqualStrings(case.source, char.token.lexeme);
        try testing.expectEqual(case.expected, char.value);
        try testing.expectEqual(@as(usize, 1), char.token.loc.src.line);
        try testing.expectEqual(@as(usize, 1), char.token.loc.src.col);
    }
}
