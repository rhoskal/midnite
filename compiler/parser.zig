const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");

pub const ParseError = error{
    UnexpectedToken,
    EmptyLambdaParams,
    InvalidCharLiteral,
    InvalidFloatLiteral,
    InvalidIntLiteral,
    InvalidStrLiteral,
};

pub const ParserError = ParseError || lexer.LexerError || std.mem.Allocator.Error;

pub const Parser = struct {
    lex: *lexer.Lexer,
    allocator: std.mem.Allocator,
    current_token: lexer.Token,

    /// Represents the possible associativity rules for operators.
    /// Determines how operators of the same precedence are grouped.
    const Associativity = enum {
        /// Left-associative operators group from left to right
        /// Example: a - b - c is parsed as (a - b) - c
        Left,

        /// Non-associative operators cannot be chained
        /// Example: a == b == c is not allowed
        None,

        /// Right-associative operators group from right to left
        /// Example: a :: b :: c is parsed as a :: (b :: c)
        Right,
    };

    /// Holds the precedence and associativity information for an operator.
    /// Used to implement the precedence climbing algorithm for expression parsing.
    const OperatorInfo = struct {
        /// Lower numbers indicate higher precedence.
        precedence: u8,
        associativity: Associativity,
    };

    /// Retrieves the operator information for a given token kind.
    /// Returns precedence and associativity rules used by the parser
    /// to correctly structure operator expressions.
    fn getOperatorInfo(kind: lexer.TokenKind) ?OperatorInfo {
        return switch (kind) {
            // Highest precedence (tightest binding)
            .OpExp => .{
                .precedence = 13,
                .associativity = .Left,
            },
            .OpIntMul, .OpIntDiv, .OpFloatMul, .OpFloatDiv => .{
                .precedence = 12,
                .associativity = .Left,
            },
            .OpIntAdd, .OpIntSub, .OpFloatAdd, .OpFloatSub => .{
                .precedence = 11,
                .associativity = .Left,
            },
            .OpComposeRight => .{
                .precedence = 10,
                .associativity = .Right,
            },
            .OpComposeLeft => .{
                .precedence = 10,
                .associativity = .Left,
            },
            .OpCons => .{
                .precedence = 9,
                .associativity = .Right,
            },
            .OpDoubleDot => .{
                .precedence = 8,
                .associativity = .Right,
            },
            .OpStrConcat => .{
                .precedence = 7,
                .associativity = .Right,
            },
            .OpListConcat => .{
                .precedence = 6,
                .associativity = .Right,
            },
            .OpEquality, .OpNotEqual, .OpLessThan, .OpGreaterThan, .OpLessThanEqual, .OpGreaterThanEqual => .{
                .precedence = 5,
                .associativity = .None,
            },
            .OpLogicalAnd => .{
                .precedence = 4,
                .associativity = .Right,
            },
            .OpLogicalOr => .{
                .precedence = 3,
                .associativity = .Right,
            },
            .OpPipeRight => .{
                .precedence = 2,
                .associativity = .Left,
            },
            .OpPipeLeft => .{
                .precedence = 2,
                .associativity = .Right,
            },
            .SymDoubleArrowRight => .{
                .precedence = 1,
                .associativity = .Right,
            },
            // Lowest precedence (loosest binding)
            .OpAssign => .{
                .precedence = 0,
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

    //==========================================================================
    // Component Parsers
    //==========================================================================
    // These methods parse building blocks of larger language constructs.
    // They return specific node structs rather than allocated Nodes.
    //==========================================================================

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

    //==========================================================================
    // Language Construct Parsers
    //==========================================================================
    // These methods parse complete language constructs.
    // They handle allocation and return *ast.Node.
    //==========================================================================

    /// Parses a regular comment node from the input.
    fn parseComment(self: *Parser) ParserError!*ast.Node {
        const token = try self.expect(lexer.TokenKind.Comment);

        const node = try self.allocator.create(ast.Node);
        errdefer self.allocator.destroy(node);

        node.* = .{
            .comment = .{
                .content = token.lexeme,
                .token = token,
            },
        };

        return node;
    }

    /// Parses a documentation comment node from the input.
    fn parseDocComment(self: *Parser) ParserError!*ast.Node {
        const token = try self.expect(lexer.TokenKind.DocComment);

        const node = try self.allocator.create(ast.Node);
        errdefer self.allocator.destroy(node);

        node.* = .{
            .doc_comment = .{
                .content = token.lexeme,
                .token = token,
            },
        };

        return node;
    }

    /// Parses a primary expression, such as literals, identifiers, or parenthesized expressions.
    /// Handles cases for basic literals (int, float, string, char), identifiers (lower and upper),
    /// and parenthesized expressions.
    fn parsePrimaryExpr(self: *Parser) ParserError!*ast.Node {
        const node = try self.allocator.create(ast.Node);
        errdefer self.allocator.destroy(node);

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
            .OpLambda => {
                const lambda = try self.parseLambdaExpr();

                return lambda;
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
        const expr = try self.parseBinaryExpr(0);
        errdefer {
            expr.deinit(self.allocator);
            self.allocator.destroy(expr);
        }

        return expr;
    }

    /// Parses binary expressions using precedence climbing. Recursively builds an
    /// expression tree that respects operator precedence and associativity rules.
    fn parseBinaryExpr(self: *Parser, min_precedence: u8) ParserError!*ast.Node {
        var left = try self.parseSimpleExpr();

        while (true) {
            const op_info = if (Parser.getOperatorInfo(self.current_token.kind)) |info| info else break;

            if (op_info.precedence < min_precedence) break;

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
            try self.advance();

            const operand = try self.parseSimpleExpr();
            errdefer {
                operand.deinit(self.allocator);
                self.allocator.destroy(operand);
            }

            node.* = .{
                .unary_expr = .{
                    .operator = operator,
                    .operand = operand,
                },
            };

            return node;
        }

        return self.parsePrimaryExpr();
    }

    /// Parses a function declaration with an optional type annotation.
    ///
    /// Examples:
    /// - `let add = \x y => x + y`
    /// - `let factorial : Int -> Int = \n => if n == 0 then 1 else n * factorial (n - 1)`
    /// - `let const : a -> b -> a = \x y => x`
    fn parseFunctionDecl(self: *Parser) ParserError!*ast.Node {
        const token = try self.expect(lexer.TokenKind.KwLet);
        const name = try self.expect(lexer.TokenKind.LowerIdent);

        var type_annotation: ?*ast.Node = null;
        if (self.check(lexer.TokenKind.DelColon)) {
            try self.advance();

            type_annotation = try self.parseFunctionType();
        }

        _ = try self.expect(lexer.TokenKind.OpAssign);
        const value = try self.parseExpression();

        const node = try self.allocator.create(ast.Node);
        errdefer {
            value.deinit(self.allocator);

            if (type_annotation) |ta| {
                ta.deinit(self.allocator);
                self.allocator.destroy(ta);
            }
        }

        node.* = .{
            .function_decl = .{
                .name = name.lexeme,
                .type_annotation = type_annotation,
                .value = value,
                .token = token,
            },
        };

        return node;
    }

    /// Parses a lambda expression of the form: \param1 param2 => expr
    ///
    /// A lambda expression consists of:
    /// - A lambda symbol (\)
    /// - One or more parameters (lowercase identifiers)
    /// - A double arrow (=>)
    /// - An expression body
    ///
    /// Examples:
    /// - `\x => x + 1`
    /// - `\x y => x * y`
    /// - `\a b c => a + b + c`
    fn parseLambdaExpr(self: *Parser) ParserError!*ast.Node {
        const token = try self.expect(lexer.TokenKind.OpLambda);

        var params = std.ArrayList([]const u8).init(self.allocator);
        errdefer params.deinit();

        while (self.check(lexer.TokenKind.LowerIdent)) {
            const param = try self.expect(lexer.TokenKind.LowerIdent);
            try params.append(param.lexeme);
        }

        if (params.items.len == 0) return error.EmptyLambdaParams;

        _ = try self.expect(lexer.TokenKind.SymDoubleArrowRight);
        const body = try self.parseExpression();

        const node = try self.allocator.create(ast.Node);
        errdefer {
            body.deinit(self.allocator);
            self.allocator.destroy(body);
        }

        node.* = .{
            .lambda_expr = .{
                .params = params,
                .body = body,
                .token = token,
            },
        };

        return node;
    }

    /// Parses a function type signature consisting of types separated by arrows.
    /// The final type is considered the return type.
    ///
    /// Examples:
    /// - `Int -> Int`
    /// - `String -> Int -> Bool`
    /// - `(List a) -> Maybe b -> Result Error c`
    fn parseFunctionType(self: *Parser) ParserError!*ast.Node {
        const token = self.current_token;

        var param_types = std.ArrayList(*ast.Node).init(self.allocator);
        errdefer {
            for (param_types.items) |param| {
                param.deinit(self.allocator);
                self.allocator.destroy(param);
            }

            param_types.deinit();
        }

        const first_type = try self.parseTypeExpression();
        try param_types.append(first_type);

        while (try self.match(lexer.TokenKind.SymArrowRight)) {
            const param_type = try self.parseTypeExpression();
            try param_types.append(param_type);
        }

        const node = try self.allocator.create(ast.Node);
        errdefer self.allocator.destroy(node);

        node.* = .{
            .function_type = .{
                .param_types = param_types,
                .token = token,
            },
        };

        return node;
    }

    /// Parses a type expression, which can be a simple type name or a more complex
    /// parameterized type.
    ///
    /// Examples:
    /// - `Int`
    /// - `Maybe a`
    /// - `List String`
    fn parseTypeExpression(self: *Parser) ParserError!*ast.Node {
        // For now just handle simple types
        if (self.check(lexer.TokenKind.UpperIdent)) {
            const type_name = try self.expect(lexer.TokenKind.UpperIdent);

            const node = try self.allocator.create(ast.Node);
            node.* = .{
                .upper_identifier = .{
                    .name = type_name.lexeme,
                    .token = type_name,
                },
            };

            return node;
        }

        // Handle type variables
        if (self.check(lexer.TokenKind.LowerIdent)) {
            const type_var = try self.expect(lexer.TokenKind.LowerIdent);

            const node = try self.allocator.create(ast.Node);
            node.* = .{
                .lower_identifier = .{
                    .name = type_var.lexeme,
                    .token = type_var,
                },
            };

            return node;
        }

        // Handle parenthesized type expressions
        if (try self.match(lexer.TokenKind.DelLParen)) {
            const inner_type = try self.parseTypeExpression();
            _ = try self.expect(lexer.TokenKind.DelRParen);

            return inner_type;
        }

        return error.UnexpectedToken;
    }

    /// Parses a conditional expression with a required 'then' and 'else' branch.
    /// The condition must evaluate to a boolean value. If true, the 'then' branch
    /// is evaluated; otherwise, the 'else' branch is evaluated.
    ///
    /// Examples:
    /// - `if x > 0 then "positive" else "non-positive"`
    /// - `if empty? list then None else head list`
    /// - `if x == 0 then
    ///      "zero"
    ///    else if x < 0 then
    ///      "negative"
    ///    else
    ///      "positive"`
    fn parseIfThenElse(self: *Parser) ParserError!*ast.Node {
        _ = try self.expect(lexer.TokenKind.KwIf);
        const condition = try self.parseExpression();

        _ = try self.expect(lexer.TokenKind.KwThen);
        const then_branch = try self.parseExpression();

        _ = try self.expect(lexer.TokenKind.KwElse);
        const else_branch = try self.parseExpression();

        const node = try self.allocator.create(ast.Node);
        errdefer {
            condition.deinit(self.allocator);
            then_branch.deinit(self.allocator);
            else_branch.deinit(self.allocator);
            self.allocator.destroy(node);
        }

        node.* = .{
            .if_then_else_stmt = .{
                .condition = condition,
                .then_branch = then_branch,
                .else_branch = else_branch,
            },
        };

        return node;
    }

    /// Parses a complete program, which consists of a sequence of top-level declarations.
    pub fn parseProgram(self: *Parser) ParserError!*ast.Node {
        var statements = std.ArrayList(*ast.Node).init(self.allocator);
        errdefer {
            for (statements.items) |stmt| {
                stmt.deinit(self.allocator);
                self.allocator.destroy(stmt);
            }

            statements.deinit();
        }

        while (self.current_token.kind != .Eof) {
            const stmt = try self.parseTopLevel();
            try statements.append(stmt);
        }

        const node = try self.allocator.create(ast.Node);
        errdefer self.allocator.destroy(node);

        node.* = .{
            .program = .{
                .statements = statements,
                .token = self.current_token,
            },
        };

        return node;
    }

    /// Parses a single top-level declaration based on the current token.
    fn parseTopLevel(self: *Parser) ParserError!*ast.Node {
        switch (self.current_token.kind) {
            .KwLet => return self.parseFunctionDecl(),
            // .KwForeign => return self.parseForeignFunctionDecl(),
            // .KwType => {
            //     try self.advance();

            //     if (self.check(.KwAlias)) {
            //         return self.parseTypeAlias();
            //     } else {
            //         return self.parseVariantType();
            //     }
            // },
            // .KwModule => return self.parseModuleDecl(),
            // .KwOpen => return self.parseImportSpec(),
            // .KwInclude => return self.parseInclude(),
            .Comment => return self.parseComment(),
            .DocComment => return self.parseDocComment(),
            else => return error.UnexpectedToken,
        }
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

    const node = try parser.parseComment();
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    try testing.expect(node.* == .comment);

    const comment = node.comment;
    try testing.expectEqual(lexer.TokenKind.Comment, comment.token.kind);
    try testing.expectEqualStrings(source, comment.content);
}

test "[doc_comment]" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "## This is a doc comment";
    var lex = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(&lex, allocator);
    defer parser.deinit();

    const node = try parser.parseDocComment();
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    try testing.expect(node.* == .doc_comment);

    const comment = node.doc_comment;
    try testing.expectEqual(lexer.TokenKind.DocComment, comment.token.kind);
    try testing.expectEqualStrings(source, comment.content);

    try testing.expectEqual(lexer.TokenKind.DocComment, comment.token.kind);
    try testing.expectEqualStrings(source, comment.content);
}

test "[int_literal]" {
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

        const lit = try parser.parseIntLiteral();

        try testing.expectEqual(lexer.TokenKind.LitInt, lit.token.kind);
        try testing.expectEqualStrings(case.source, lit.token.lexeme);
        try testing.expectEqual(case.expected, lit.value);
    }
}

test "[float_literal]" {
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

        const lit = try parser.parseFloatLiteral();

        try testing.expectEqual(lexer.TokenKind.LitFloat, lit.token.kind);
        try testing.expectEqualStrings(case.source, lit.token.lexeme);
        try testing.expectEqual(case.expected, lit.value);
    }
}

test "[str_literal]" {
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

        const lit = try parser.parseStrLiteral();
        defer allocator.free(lit.value);

        try testing.expectEqual(lexer.TokenKind.LitString, lit.token.kind);
        try testing.expectEqualStrings(case.source, lit.token.lexeme);
        try testing.expectEqualStrings(case.expected, lit.value);
    }
}

test "[char_literal]" {
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

        const lit = try parser.parseCharLiteral();

        try testing.expectEqual(lexer.TokenKind.LitChar, lit.token.kind);
        try testing.expectEqualStrings(case.source, lit.token.lexeme);
        try testing.expectEqual(case.expected, lit.value);
    }
}

test "[lower_identifier]" {
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
    }
}

test "[upper_identifier]" {
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
    }
}

test "[unary_expr]" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "-42";
    var lex = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(&lex, allocator);
    defer parser.deinit();

    const expr = try parser.parseSimpleExpr();
    defer {
        expr.deinit(allocator);
        allocator.destroy(expr);
    }

    try testing.expect(expr.* == .unary_expr);
    try testing.expectEqual(lexer.TokenKind.OpIntSub, expr.unary_expr.operator.kind);
    try testing.expectEqualStrings("-", expr.unary_expr.operator.lexeme);

    const operand = expr.unary_expr.operand;
    try testing.expect(operand.* == .int_literal);
    try testing.expect(operand.int_literal.value == 42);
}

test "[arithmetic_expr]" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "42 + 24";
    var lex = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(&lex, allocator);
    defer parser.deinit();

    const expr = try parser.parseExpression();
    defer {
        expr.deinit(allocator);
        allocator.destroy(expr);
    }

    try testing.expect(expr.* == .arithmetic_expr);
    try testing.expectEqual(lexer.TokenKind.OpIntAdd, expr.arithmetic_expr.operator.kind);

    const left = expr.arithmetic_expr.left;
    try testing.expect(left.* == .int_literal);
    try testing.expect(left.int_literal.value == 42);

    const right = expr.arithmetic_expr.right;
    try testing.expect(right.* == .int_literal);
    try testing.expect(right.int_literal.value == 24);
}

test "[comparison_expr]" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const TestCase = struct {
        source: []const u8,
        op: lexer.TokenKind,
        left_val: i64,
        right_val: i64,
    };

    const cases = [_]TestCase{
        .{ .source = "42 == 24", .op = .OpEquality, .left_val = 42, .right_val = 24 },
        .{ .source = "42 /= 24", .op = .OpNotEqual, .left_val = 42, .right_val = 24 },
        .{ .source = "42 < 24", .op = .OpLessThan, .left_val = 42, .right_val = 24 },
        .{ .source = "42 > 24", .op = .OpGreaterThan, .left_val = 42, .right_val = 24 },
        .{ .source = "42 <= 24", .op = .OpLessThanEqual, .left_val = 42, .right_val = 24 },
        .{ .source = "42 >= 24", .op = .OpGreaterThanEqual, .left_val = 42, .right_val = 24 },
    };

    for (cases) |case| {
        var lex = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(&lex, allocator);
        defer parser.deinit();

        const expr = try parser.parseExpression();
        defer {
            expr.deinit(allocator);
            allocator.destroy(expr);
        }

        try testing.expect(expr.* == .comparison_expr);
        try testing.expectEqual(case.op, expr.comparison_expr.operator.kind);

        const left = expr.comparison_expr.left;
        try testing.expect(left.* == .int_literal);
        try testing.expectEqual(case.left_val, left.int_literal.value);

        const right = expr.comparison_expr.right;
        try testing.expect(right.* == .int_literal);
        try testing.expectEqual(case.right_val, right.int_literal.value);
    }
}

test "[logical_expr]" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const TestCase = struct {
        source: []const u8,
        op: lexer.TokenKind,
    };

    const cases = [_]TestCase{
        .{ .source = "true && false", .op = .OpLogicalAnd },
        .{ .source = "true || false", .op = .OpLogicalOr },
    };

    for (cases) |case| {
        var lex = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(&lex, allocator);
        defer parser.deinit();

        const expr = try parser.parseExpression();
        defer {
            expr.deinit(allocator);
            allocator.destroy(expr);
        }

        try testing.expect(expr.* == .logical_expr);
        try testing.expectEqual(case.op, expr.logical_expr.operator.kind);

        const left = expr.logical_expr.left;
        try testing.expect(left.* == .lower_identifier);
        try testing.expectEqualStrings("true", left.lower_identifier.name);

        const right = expr.logical_expr.right;
        try testing.expect(right.* == .lower_identifier);
        try testing.expectEqualStrings("false", right.lower_identifier.name);
    }
}

test "[operator precedence]" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const TestCase = struct {
        source: []const u8,
        root_type: std.meta.Tag(ast.Node),
        root_op: lexer.TokenKind,
    };

    const cases = [_]TestCase{
        // Arithmetic precedence
        .{
            .source = "1 + 2 * 3", // Should be 1 + (2 * 3)
            .root_type = .arithmetic_expr,
            .root_op = .OpIntAdd,
        },
        .{
            .source = "1 * 2 + 3", // Should be (1 * 2) + 3
            .root_type = .arithmetic_expr,
            .root_op = .OpIntAdd,
        },
        .{
            .source = "2 ** 3 * 4", // Should be (2 ** 3) * 4
            .root_type = .arithmetic_expr,
            .root_op = .OpIntMul,
        },

        // Comparison and arithmetic
        .{
            .source = "1 + 2 == 3 * 4", // Should be (1 + 2) == (3 * 4)
            .root_type = .comparison_expr,
            .root_op = .OpEquality,
        },

        // Logical operators
        .{
            .source = "a && b || c", // Should be (a && b) || c
            .root_type = .logical_expr,
            .root_op = .OpLogicalOr,
        },
        .{
            .source = "a || b && c", // Should be a || (b && c)
            .root_type = .logical_expr,
            .root_op = .OpLogicalOr,
        },

        // Mixed operators
        .{
            .source = "1 + 2 * 3 == 4 && 5 * 6 == 7", // Should be ((1 + (2 * 3)) == 4) && ((5 * 6) == 7)
            .root_type = .logical_expr,
            .root_op = .OpLogicalAnd,
        },
        .{
            .source = "2 ** 3 ** 4", // Should be 2 ** (3 ** 4) due to right associativity
            .root_type = .arithmetic_expr,
            .root_op = .OpExp,
        },
        .{
            .source = "a && b && c || d && e", // Should be ((a && b) && c) || (d && e)
            .root_type = .logical_expr,
            .root_op = .OpLogicalOr,
        },
        .{
            .source = "1 * 2 + 3 * 4 + 5", // Should be ((1 * 2) + (3 * 4)) + 5
            .root_type = .arithmetic_expr,
            .root_op = .OpIntAdd,
        },
        .{
            .source = "1 + 2 * 3 ** 4 * 5 + 6", // Should be 1 + ((2 * (3 ** 4)) * 5) + 6
            .root_type = .arithmetic_expr,
            .root_op = .OpIntAdd,
        },
        .{
            .source = "a || b && c && d || e", // Should be (a || ((b && c) && d)) || e
            .root_type = .logical_expr,
            .root_op = .OpLogicalOr,
        },
        .{
            .source = "1 * 2 + 3 == 4 + 5 * 6 && 7 + 8 == 9", // Should be (((1 * 2) + 3) == (4 + (5 * 6))) && ((7 + 8) == 9)
            .root_type = .logical_expr,
            .root_op = .OpLogicalAnd,
        },
        .{
            .source = "a <= b + c * d && e > f ** g || h == i", // Should be ((a <= (b + (c * d))) && (e > (f ** g))) || (h == i)
            .root_type = .logical_expr,
            .root_op = .OpLogicalOr,
        },
    };

    for (cases) |case| {
        var lex = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(&lex, allocator);
        defer parser.deinit();

        const expr = try parser.parseExpression();
        defer {
            expr.deinit(allocator);
            allocator.destroy(expr);
        }

        try testing.expectEqual(case.root_type, std.meta.activeTag(expr.*));

        switch (expr.*) {
            .arithmetic_expr => |aexpr| {
                try testing.expectEqual(case.root_op, aexpr.operator.kind);
            },
            .logical_expr => |lexpr| {
                try testing.expectEqual(case.root_op, lexpr.operator.kind);
            },
            .comparison_expr => |cexpr| {
                try testing.expectEqual(case.root_op, cexpr.operator.kind);
            },
            else => unreachable,
        }
    }
}

test "[operator precedence] (structural)" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    {
        var lex = lexer.Lexer.init("1 + 2 * 3", TEST_FILE);
        var parser = try Parser.init(&lex, allocator);
        defer parser.deinit();

        const expr = try parser.parseExpression();
        defer {
            expr.deinit(allocator);
            allocator.destroy(expr);
        }

        // Should be structured as: (1 + (2 * 3))
        try testing.expect(expr.* == .arithmetic_expr);
        try testing.expectEqual(lexer.TokenKind.OpIntAdd, expr.arithmetic_expr.operator.kind);

        const left = expr.arithmetic_expr.left;
        try testing.expect(left.* == .int_literal);
        try testing.expectEqual(@as(i64, 1), left.int_literal.value);

        const right = expr.arithmetic_expr.right;
        try testing.expect(right.* == .arithmetic_expr);
        try testing.expectEqual(lexer.TokenKind.OpIntMul, right.arithmetic_expr.operator.kind);

        const mul_left = right.arithmetic_expr.left;
        try testing.expect(mul_left.* == .int_literal);
        try testing.expectEqual(@as(i64, 2), mul_left.int_literal.value);

        const mul_right = right.arithmetic_expr.right;
        try testing.expect(mul_right.* == .int_literal);
        try testing.expectEqual(@as(i64, 3), mul_right.int_literal.value);
    }

    {
        var lex = lexer.Lexer.init("1 * 2 + 3 == 4", TEST_FILE);
        var parser = try Parser.init(&lex, allocator);
        defer parser.deinit();

        const expr = try parser.parseExpression();
        defer {
            expr.deinit(allocator);
            allocator.destroy(expr);
        }

        // Should be structured as: ((1 * 2 + 3) == 4)
        try testing.expect(expr.* == .comparison_expr);
        try testing.expectEqual(lexer.TokenKind.OpEquality, expr.comparison_expr.operator.kind);

        const right = expr.comparison_expr.right;
        try testing.expect(right.* == .int_literal);
        try testing.expectEqual(@as(i64, 4), right.int_literal.value);

        const left = expr.comparison_expr.left;
        try testing.expect(left.* == .arithmetic_expr);
        try testing.expectEqual(lexer.TokenKind.OpIntAdd, left.arithmetic_expr.operator.kind);

        const add_right = left.arithmetic_expr.right;
        try testing.expect(add_right.* == .int_literal);
        try testing.expectEqual(@as(i64, 3), add_right.int_literal.value);

        const add_left = left.arithmetic_expr.left;
        try testing.expect(add_left.* == .arithmetic_expr);
        try testing.expectEqual(lexer.TokenKind.OpIntMul, add_left.arithmetic_expr.operator.kind);

        const mul_left = add_left.arithmetic_expr.left;
        try testing.expect(mul_left.* == .int_literal);
        try testing.expectEqual(@as(i64, 1), mul_left.int_literal.value);

        const mul_right = add_left.arithmetic_expr.right;
        try testing.expect(mul_right.* == .int_literal);
        try testing.expectEqual(@as(i64, 2), mul_right.int_literal.value);
    }
}

test "[lambda_expr]" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "\\x => x + 1";
    var lex = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(&lex, allocator);
    defer parser.deinit();

    const expr = try parser.parseExpression();
    defer {
        expr.deinit(allocator);
        allocator.destroy(expr);
    }

    try testing.expect(expr.* == .lambda_expr);
    try testing.expectEqual(@as(usize, 1), expr.lambda_expr.params.items.len);
    try testing.expectEqualStrings("x", expr.lambda_expr.params.items[0]);
    try testing.expect(expr.lambda_expr.body.* == .arithmetic_expr);
}

test "[if_then_else_stmt]" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const source = "if x > 0 then 1 else -1";
    var lex = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(&lex, allocator);
    defer parser.deinit();

    const node = try parser.parseIfThenElse();
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    try testing.expect(node.* == .if_then_else_stmt);
    const stmt = node.if_then_else_stmt;

    try testing.expect(stmt.condition.* == .comparison_expr);
    const condition = stmt.condition.comparison_expr;
    try testing.expectEqual(lexer.TokenKind.OpGreaterThan, condition.operator.kind);

    // Check left side of condition (x)
    try testing.expect(condition.left.* == .lower_identifier);
    try testing.expectEqualStrings("x", condition.left.lower_identifier.name);

    // Check right side of condition (0)
    try testing.expect(condition.right.* == .int_literal);
    try testing.expectEqual(@as(i64, 0), condition.right.int_literal.value);

    // Verify then branch (1)
    try testing.expect(stmt.then_branch.* == .int_literal);
    try testing.expectEqual(@as(i64, 1), stmt.then_branch.int_literal.value);

    // Verify else branch (-1)
    try testing.expect(stmt.else_branch.* == .unary_expr);
    const else_expr = stmt.else_branch.unary_expr;
    try testing.expectEqual(lexer.TokenKind.OpIntSub, else_expr.operator.kind);
    try testing.expect(else_expr.operand.* == .int_literal);
    try testing.expectEqual(@as(i64, 1), else_expr.operand.int_literal.value);
}

test "[function_decl] (simple)" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Test a simple function declaration without type annotation
    const source = "let add = \\x y => x + y";
    var lex = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(&lex, allocator);
    defer parser.deinit();

    const node = try parser.parseFunctionDecl();
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    // Verify the node is a function declaration
    try testing.expect(node.* == .function_decl);
    const decl = node.function_decl;

    // Check function name
    try testing.expectEqualStrings("add", decl.name);

    // Verify type annotation is null for simple declaration
    try testing.expect(decl.type_annotation == null);

    // Verify function value is a lambda expression
    try testing.expect(decl.value.* == .lambda_expr);
    const lambda = decl.value.lambda_expr;

    // Check lambda parameters
    try testing.expectEqual(@as(usize, 2), lambda.params.items.len);
    try testing.expectEqualStrings("x", lambda.params.items[0]);
    try testing.expectEqualStrings("y", lambda.params.items[1]);

    // Verify lambda body is an arithmetic expression
    try testing.expect(lambda.body.* == .arithmetic_expr);
    const body = lambda.body.arithmetic_expr;
    try testing.expectEqual(lexer.TokenKind.OpIntAdd, body.operator.kind);
    try testing.expect(body.left.* == .lower_identifier);
    try testing.expect(body.right.* == .lower_identifier);
    try testing.expectEqualStrings("x", body.left.lower_identifier.name);
    try testing.expectEqualStrings("y", body.right.lower_identifier.name);
}

test "[function_decl] (w/ type annotation)" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Test function declaration with type annotation
    const source = "let inc : Int -> Int = \\x => x + 1";
    var lex = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(&lex, allocator);
    defer parser.deinit();

    const node = try parser.parseFunctionDecl();
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    // Verify the node is a function declaration
    try testing.expect(node.* == .function_decl);
    const decl = node.function_decl;

    // Check function name
    try testing.expectEqualStrings("inc", decl.name);

    // Verify type annotation exists and is correct
    try testing.expect(decl.type_annotation != null);
    try testing.expect(decl.type_annotation.?.* == .function_type);

    const type_annotation = decl.type_annotation.?.function_type;
    try testing.expectEqual(@as(usize, 2), type_annotation.param_types.items.len);

    // Check param types are both Int
    try testing.expect(type_annotation.param_types.items[0].* == .upper_identifier);
    try testing.expect(type_annotation.param_types.items[1].* == .upper_identifier);
    try testing.expectEqualStrings("Int", type_annotation.param_types.items[0].upper_identifier.name);
    try testing.expectEqualStrings("Int", type_annotation.param_types.items[1].upper_identifier.name);

    // Verify function value is a lambda expression
    try testing.expect(decl.value.* == .lambda_expr);
    const lambda = decl.value.lambda_expr;

    // Check lambda parameters
    try testing.expectEqual(@as(usize, 1), lambda.params.items.len);
    try testing.expectEqualStrings("x", lambda.params.items[0]);

    // Verify lambda body is an arithmetic expression
    try testing.expect(lambda.body.* == .arithmetic_expr);
    const body = lambda.body.arithmetic_expr;
    try testing.expectEqual(lexer.TokenKind.OpIntAdd, body.operator.kind);
    try testing.expect(body.left.* == .lower_identifier);
    try testing.expect(body.right.* == .int_literal);
    try testing.expectEqualStrings("x", body.left.lower_identifier.name);
    try testing.expectEqual(@as(i64, 1), body.right.int_literal.value);
}
