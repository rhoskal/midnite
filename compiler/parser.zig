const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");

pub const ParseError = error{
    UnexpectedToken,
    InvalidIntLiteral,
    InvalidFloatLiteral,
    InvalidCharLiteral,
    InvalidStrLiteral,
};

pub const ParserError = ParseError || lexer.LexerError || std.mem.Allocator.Error;

pub const Parser = struct {
    lex: *lexer.Lexer,
    allocator: std.mem.Allocator,
    current_token: lexer.Token,

    const Associativity = enum {
        Left,
        None,
        Right,
    };

    const OperatorInfo = struct {
        precedence: u8,
        associativity: Associativity,
    };

    fn getOperatorInfo(kind: lexer.TokenKind) ?OperatorInfo {
        return switch (kind) {
            // Highest precedence (tightest binding)
            .OpExp => .{
                .precedence = 0,
                .associativity = .Left,
            },
            .OpIntMul, .OpIntDiv, .OpFloatMul, .OpFloatDiv => .{
                .precedence = 1,
                .associativity = .Left,
            },
            .OpIntAdd, .OpIntSub, .OpFloatAdd, .OpFloatSub => .{
                .precedence = 2,
                .associativity = .Left,
            },
            .OpComposeRight => .{
                .precedence = 3,
                .associativity = .Right,
            },
            .OpComposeLeft => .{
                .precedence = 3,
                .associativity = .Left,
            },
            .OpCons => .{
                .precedence = 4,
                .associativity = .Right,
            },
            .OpDoubleDot => .{
                .precedence = 5,
                .associativity = .Right,
            },
            .OpStrConcat => .{
                .precedence = 6,
                .associativity = .Right,
            },
            .OpListConcat => .{
                .precedence = 7,
                .associativity = .Right,
            },
            .OpEquality, .OpNotEqual, .OpLessThan, .OpGreaterThan, .OpLessThanEqual, .OpGreaterThanEqual, .OpDoubleDot => .{
                .precedence = 8,
                .associativity = .None,
            },
            .OpLogicalAnd => .{
                .precedence = 9,
                .associativity = .Right,
            },
            .OpLogicalOr => .{
                .precedence = 10,
                .associativity = .Right,
            },
            .OpPipeRight => .{
                .precedence = 11,
                .associativity = .Left,
            },
            .OpPipeLeft => .{
                .precedence = 11,
                .associativity = .Right,
            },
            .SymDoubleArrowRight => .{
                .precedence = 12,
                .associativity = .Right,
            },
            // Lowest precedence (loosest binding)
            .OpAssign => .{
                .precedence = 13,
                .associativity = .None,
            },
            else => null,
        };
    }

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
    fn parseComment(self: *Parser) ParserError!ast.CommentNode {
        const token = try self.expect(lexer.TokenKind.Comment);

        return ast.CommentNode{
            .content = token.lexeme,
            .token = token,
        };
    }

    /// Parses a documentation comment node from the input.
    fn parseDocComment(self: *Parser) ParserError!ast.DocCommentNode {
        const token = try self.expect(lexer.TokenKind.DocComment);

        return ast.DocCommentNode{
            .content = token.lexeme,
            .token = token,
        };
    }

    /// Parses an integer literal value into a structured node.
    /// The literal can be in decimal, hexadecimal (0x), binary (0b), or octal (0o) format.
    /// Underscores in numbers are allowed and ignored (e.g., 1_000_000).
    fn parseIntLiteral(self: *Parser) ParserError!ast.IntLiteralNode {
        const token = try self.expect(lexer.TokenKind.LitInt);
        const value = std.fmt.parseInt(i64, token.lexeme, 0) catch {
            return error.InvalidIntLiteral;
        };

        return ast.IntLiteralNode{
            .value = value,
            .token = token,
        };
    }

    /// Parses a floating-point literal value into a structured node.
    /// The literal can include decimal points and optional scientific notation (e.g., 1.23e-4).
    /// Underscores in numbers are allowed and ignored (e.g., 3.141_592).
    fn parseFloatLiteral(self: *Parser) ParserError!ast.FloatLiteralNode {
        const token = try self.expect(lexer.TokenKind.LitFloat);
        const value = std.fmt.parseFloat(f64, token.lexeme) catch {
            return error.InvalidFloatLiteral;
        };

        return ast.FloatLiteralNode{
            .value = value,
            .token = token,
        };
    }

    /// Parses a string literal into a structured node.
    fn parseStrLiteral(self: *Parser) ParserError!ast.StrLiteralNode {
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

                if (unquoted[i + 1] == 'u') {
                    self.allocator.free(sequence.value);
                }

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

    fn parseStrEscapeSequence(self: *Parser, unquoted: []const u8) ParserError!EscapeSequence {
        return switch (unquoted[1]) {
            'n' => .{ .value = "\n", .len = 2 },
            'r' => .{ .value = "\r", .len = 2 },
            't' => .{ .value = "\t", .len = 2 },
            '\\' => .{ .value = "\\", .len = 2 },
            '\"' => .{ .value = "\"", .len = 2 },
            'u' => {
                // Handle Unicode escape \u{XXXXXX} where XXXXXX is 1-6 hex digits
                const brace_index = std.mem.indexOfScalar(u8, unquoted[3..], '}') orelse return error.InvalidStrLiteral;
                const hex = unquoted[3 .. 3 + brace_index];
                const code_point = std.fmt.parseInt(u21, hex, 16) catch {
                    return error.InvalidStrLiteral;
                };

                var utf8_buffer: [4]u8 = undefined;
                const utf8_len = std.unicode.utf8Encode(code_point, &utf8_buffer) catch {
                    return error.InvalidStrLiteral;
                };

                // Length is: \u{ + hex digits + }
                const sequence_len = 3 + hex.len + 1;

                return .{
                    .value = try self.allocator.dupe(u8, utf8_buffer[0..utf8_len]),
                    .len = sequence_len,
                };
            },
            else => return error.InvalidStrLiteral,
        };
    }

    /// Parses a character literal into its Unicode codepoint value.
    /// Handles standard characters ('a'), escape sequences ('\n', '\t', etc.),
    /// and Unicode escape sequences ('\u{0061}').
    fn parseCharLiteral(self: *Parser) ParserError!ast.CharLiteralNode {
        const token = try self.expect(lexer.TokenKind.LitChar);

        // Lexeme includes the quotes, so it's at least 3 chars: 'x'
        std.debug.assert(token.lexeme.len >= 3);

        const unquoted = token.lexeme[1 .. token.lexeme.len - 1];

        var value: u21 = undefined;
        if (unquoted[0] == '\\') {
            value = try parseCharEscapeSequence(unquoted);
        } else {
            value = std.unicode.utf8Decode(unquoted) catch {
                return error.InvalidCharLiteral;
            };
        }

        return ast.CharLiteralNode{
            .value = value,
            .token = token,
        };
    }

    fn parseCharEscapeSequence(unquoted: []const u8) ParserError!u21 {
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

                return std.fmt.parseInt(u21, hex, 16) catch {
                    return error.InvalidCharLiteral;
                };
            },
            else => error.InvalidCharLiteral,
        };
    }

    /// Parses a lower-case identifier into a structured node.
    fn parseLowerIdentifier(self: *Parser) ParserError!ast.LowerIdentifierNode {
        const token = try self.expect(lexer.TokenKind.LowerIdent);

        return ast.LowerIdentifierNode{
            .name = token.lexeme,
            .token = token,
        };
    }

    /// Parses a upper-case identifier into a structured node.
    fn parseUpperIdentifier(self: *Parser) ParserError!ast.UpperIdentifierNode {
        const token = try self.expect(lexer.TokenKind.UpperIdent);

        return ast.UpperIdentifierNode{
            .name = token.lexeme,
            .token = token,
        };
    }

    /// Parses a primary expression, such as literals, identifiers, or parenthesized expressions.
    /// Handles cases for basic literals (int, float, string, char), identifiers (lower and upper),
    /// and parenthesized expressions.
    fn parsePrimaryExpr(self: *Parser) ParserError!*ast.Node {
        const node = try self.allocator.create(ast.Node);

        switch (self.current_token.kind) {
            .LitInt => {
                const int_literal = try self.parseIntLiteral();

                node.* = .{ .int_literal = int_literal };
            },
            .LitFloat => {
                const float_literal = try self.parseFloatLiteral();

                node.* = .{ .float_literal = float_literal };
            },
            .LitString => {
                const str_literal = try self.parseStrLiteral();

                node.* = .{ .str_literal = str_literal };
            },
            .LitChar => {
                const char_literal = try self.parseCharLiteral();

                node.* = .{ .char_literal = char_literal };
            },
            .LowerIdent => {
                const identifier = try self.parseLowerIdentifier();

                node.* = .{ .lower_identifier = identifier };
            },
            .UpperIdent => {
                const identifier = try self.parseUpperIdentifier();

                node.* = .{ .upper_identifier = identifier };
            },
            .DelLParen => {
                try self.advance();

                const expr = try self.parseExpression();

                _ = try self.expect(lexer.TokenKind.DelRParen);

                return expr;
            },
            else => {
                self.allocator.destroy(node);

                return error.UnexpectedToken;
            },
        }

        return node;
    }

    /// Parses a complete expression by starting the precedence climbing algorithm
    /// at the lowest precedence level (0). This is the main entry point for
    /// parsing any expression.
    fn parseExpression(self: *Parser) ParserError!*ast.Node {
        // Start at highest precedence level
        return self.parseBinaryExpr(0);
    }

    /// Parses binary expressions using precedence climbing. Recursively builds an
    /// expression tree that respects operator precedence and associativity rules.
    fn parseBinaryExpr(self: *Parser, min_precedence: i8) ParserError!*ast.Node {
        const left = try self.parseSimpleExpr();

        while (true) {
            const op_info = if (self.getOperatorInfo(self.current_token.kind)) |info| info else break;

            if (op_info.precedence >= min_precedence) break;

            const operator = self.current_token;
            try self.advance();

            // Determine minimum precedence for right side
            const next_min = switch (op_info.associativity) {
                .Left => op_info.precedence + 1,
                .Right => op_info.precedence,
                .None => op_info.precedence + 1,
            };

            const right = try self.parseBinaryExpr(next_min);

            const node = try self.allocator.create(ast.Node);
            node.* = switch (operator.kind) {
                .OpIntAdd,
                .OpIntSub,
                .OpIntMul,
                .OpIntDiv,
                .OpFloatAdd,
                .OpFloatSub,
                .OpFloatMul,
                .OpFloatDiv,
                .OpExp,
                => .{
                    .arithmetic_expr = .{
                        .left = left,
                        .operator = operator,
                        .right = right,
                    },
                },
                .OpLogicalAnd,
                .OpLogicalOr,
                => .{
                    .logical_expr = .{
                        .left = left,
                        .operator = operator,
                        .right = right,
                    },
                },
                .OpEquality,
                .OpNotEqual,
                .OpLessThan,
                .OpGreaterThan,
                .OpLessThanEqual,
                .OpGreaterThanEqual,
                => .{
                    .comparison_expr = .{
                        .left = left,
                        .operator = operator,
                        .right = right,
                    },
                },
                else => unreachable,
            };

            left = node;
        }

        return left;
    }

    /// Identify if the current token _could_ be used as a unary operator.
    fn isUnaryOp(self: *Parser) bool {
        return self.check(lexer.TokenKind.OpIntSub);
    }

    /// Parses a simple expression, such as a unary operation or a primary expression.
    /// If the current token represents a unary operator, it parses the operator and its operand.
    /// Otherwise, delegates to `parsePrimaryExpr` for parsing primary expressions.
    fn parseSimpleExpr(self: *Parser) ParserError!*ast.Node {
        if (self.isUnaryOp()) {
            const node = try self.allocator.create(ast.Node);
            errdefer self.allocator.destroy(node);

            const operator = self.current_token;
            self.advance();

            const operand = try self.parseSimpleExpr();

            node.* = .{
                .operator = operator,
                .operand = operand,
            };

            return node;
        }

        return self.parsePrimaryExpr();
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

test "[lower identifier]" {
    const TestCase = struct {
        source: []const u8,
        expected: []const u8,
    };

    const cases = [_]TestCase{
        .{ .source = "x", .expected = "x" },
        .{ .source = "foo", .expected = "foo" },
        .{ .source = "valid?", .expected = "valid?" },
        .{ .source = "foo_bar", .expected = "foo_bar" },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (cases) |case| {
        var lex = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(&lex, allocator);
        defer parser.deinit();

        const ident = try parser.parseLowerIdentifier();
        try testing.expectEqual(lexer.TokenKind.LowerIdent, ident.token.kind);
        try testing.expectEqualStrings(case.source, ident.token.lexeme);
        try testing.expectEqualStrings(case.expected, ident.name);
        try testing.expectEqual(@as(usize, 1), ident.token.loc.src.line);
        try testing.expectEqual(@as(usize, 1), ident.token.loc.src.col);
    }
}

test "[upper identifier]" {
    const TestCase = struct {
        source: []const u8,
        expected: []const u8,
    };

    const cases = [_]TestCase{
        .{ .source = "X", .expected = "X" },
        .{ .source = "Foo", .expected = "Foo" },
        .{ .source = "FooBar", .expected = "FooBar" },
        .{ .source = "FooBar42", .expected = "FooBar42" },
        .{ .source = "Foo_Bar", .expected = "Foo_Bar" },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    for (cases) |case| {
        var lex = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(&lex, allocator);
        defer parser.deinit();

        const ident = try parser.parseUpperIdentifier();
        try testing.expectEqual(lexer.TokenKind.UpperIdent, ident.token.kind);
        try testing.expectEqualStrings(case.source, ident.token.lexeme);
        try testing.expectEqualStrings(case.expected, ident.name);
        try testing.expectEqual(@as(usize, 1), ident.token.loc.src.line);
        try testing.expectEqual(@as(usize, 1), ident.token.loc.src.col);
    }
}
