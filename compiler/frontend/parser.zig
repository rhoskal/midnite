const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");

pub const ParseError = error{
    EmptyLambdaParams,
    InvalidCharLiteral,
    InvalidFloatLiteral,
    InvalidIntLiteral,
    InvalidStrLiteral,
    UnexpectedToken,
};

pub const ParserError = ParseError || lexer.LexerError || std.mem.Allocator.Error;

pub const Parser = struct {
    /// The lexer instance used to obtain tokens.
    lexer: *lexer.Lexer,

    /// The current token being processed by the parser.
    current_token: lexer.Token,

    /// Memory allocator used for parser operations.
    allocator: std.mem.Allocator,

    /// Represents the possible associativity rules for operators.
    /// Determines how operators of the same precedence are grouped.
    const Associativity = enum {
        /// Left-associative operators group from left to right
        ///
        /// Example: `a - b - c` is parsed as `(a - b) - c`
        Left,

        /// Non-associative operators cannot be chained
        ///
        /// Example: `a == b == c` is not allowed
        None,

        /// Right-associative operators group from right to left
        ///
        /// Example: `a :: b :: c` is parsed as `a :: (b :: c)`
        Right,
    };

    /// Operator metadata used by the precedence climbing parser to correctly structure
    /// expressions involving multiple operators with different precedence levels.
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
            .operator => |op| switch (op) {
                .Exp => .{
                    .precedence = 13,
                    .associativity = .Left,
                },
                .FloatDiv,
                .FloatMul,
                .IntDiv,
                .IntMul,
                => .{
                    .precedence = 12,
                    .associativity = .Left,
                },
                .FloatAdd,
                .FloatSub,
                .IntAdd,
                .IntSub,
                => .{
                    .precedence = 11,
                    .associativity = .Left,
                },
                .ComposeRight => .{
                    .precedence = 10,
                    .associativity = .Right,
                },
                .ComposeLeft => .{
                    .precedence = 10,
                    .associativity = .Left,
                },
                .Cons => .{
                    .precedence = 9,
                    .associativity = .Right,
                },
                .Expand => .{
                    .precedence = 8,
                    .associativity = .Right,
                },
                .StrConcat => .{
                    .precedence = 7,
                    .associativity = .Right,
                },
                .ListConcat => .{
                    .precedence = 6,
                    .associativity = .Right,
                },
                .Equality,
                .GreaterThan,
                .GreaterThanEqual,
                .LessThan,
                .LessThanEqual,
                .NotEqual,
                => .{
                    .precedence = 5,
                    .associativity = .None,
                },
                .LogicalAnd => .{
                    .precedence = 4,
                    .associativity = .Right,
                },
                .LogicalOr => .{
                    .precedence = 3,
                    .associativity = .Right,
                },
                .PipeRight => .{
                    .precedence = 2,
                    .associativity = .Left,
                },
                .PipeLeft => .{
                    .precedence = 2,
                    .associativity = .Right,
                },
                .Equal => .{
                    .precedence = 0,
                    .associativity = .None,
                },
                else => null,
            },
            .symbol => |sym| switch (sym) {
                .DoubleArrowRight => .{
                    .precedence = 1,
                    .associativity = .Right,
                },
                else => null,
            },
            else => null,
            // Lowest precedence (loosest binding)
        };
    }

    pub fn init(allocator: std.mem.Allocator, l: *lexer.Lexer) !*Parser {
        const parser = try allocator.create(Parser);
        const first_token = try l.nextToken();

        parser.* = .{
            .lexer = l,
            .current_token = first_token,
            .allocator = allocator,
        };

        return parser;
    }

    /// Frees resources associated with the parser instance.
    pub fn deinit(self: *Parser) void {
        self.allocator.destroy(self);
    }

    /// Advances to the next token in the input stream.
    fn advance(self: *Parser) ParserError!void {
        self.current_token = try self.lexer.nextToken();
    }

    /// Checks whether the current token matches a specific kind without consuming it.
    ///
    /// - `kind`: The kind of token to check.
    fn check(self: *Parser, kind: lexer.TokenKind) bool {
        const current = self.current_token.kind;

        return std.meta.activeTag(current) == std.meta.activeTag(kind) and
            std.meta.eql(current, kind);
    }

    /// Returns true and advances parser if current token matches kind.
    /// Returns false if no match, token remains unconsumed.
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

    /// Parses a lower-case identifier into a structured node.
    fn parseLowerIdentifier(self: *Parser) ParserError!ast.LowerIdentifierNode {
        const token = try self.expect(lexer.TokenKind{ .identifier = .Lower });

        return ast.LowerIdentifierNode{
            .name = token.lexeme,
            .token = token,
        };
    }

    /// Parses a upper-case identifier into a structured node.
    fn parseUpperIdentifier(self: *Parser) ParserError!ast.UpperIdentifierNode {
        const token = try self.expect(lexer.TokenKind{ .identifier = .Upper });

        return ast.UpperIdentifierNode{
            .name = token.lexeme,
            .token = token,
        };
    }

    /// Parses an integer literal value into a structured node.
    /// The literal can be in decimal, hexadecimal (0x), binary (0b), or octal (0o) format.
    /// Underscores in numbers are allowed and ignored (e.g., 1_000_000).
    fn parseIntLiteral(self: *Parser) ParserError!ast.IntLiteralNode {
        const token = try self.expect(lexer.TokenKind{ .literal = .Int });
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
        const token = try self.expect(lexer.TokenKind{ .literal = .Float });
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
        const token = try self.expect(lexer.TokenKind{ .literal = .String });

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

    /// Parses a character literal into its Unicode codepoint value.
    /// Handles standard characters ('a'), escape sequences ('\n', '\t', etc.),
    /// and Unicode escape sequences ('\u{0061}').
    fn parseCharLiteral(self: *Parser) ParserError!ast.CharLiteralNode {
        const token = try self.expect(lexer.TokenKind{ .literal = .Char });

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

    //==========================================================================
    // Language Construct Parsers
    //==========================================================================
    // These methods parse complete language constructs.
    // They handle allocation and return *ast.Node.
    //==========================================================================

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

        while (!self.check(lexer.TokenKind{ .special = .Eof })) {
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
            .keyword => |kw| switch (kw) {
                .Foreign => return self.parseForeignFunctionDecl(),
                .Include => return self.parseInclude(),
                .Let => return self.parseFunctionDecl(),
                // .Module => return self.parseModuleDecl(),
                // .Open => return self.parseImportSpec(),
                .Type => return self.parseTypeDecl(),
                else => return error.UnexpectedToken,
            },
            .comment => |c| switch (c) {
                .Doc => return self.parseDocComment(),
                .Regular => return self.parseComment(),
            },
            else => return error.UnexpectedToken,
        }
    }

    /// Parses a type declaration, which can be a type alias, record type, or variant type.
    /// Handles parsing of type parameters that may be used in the type definition.
    ///
    /// Syntax:
    /// TypeDecl = "type" "alias" UpperIdent TypeParams? "=" Type
    ///          | "type" UpperIdent TypeParams? "=" "{" FieldList "}"
    ///          | "type" UpperIdent TypeParams? "=" "|"? Constructor ("|" Constructor)*
    /// TypeParams = LowerIdent+
    ///
    /// Examples:
    /// - `type alias Reader r a = r -> a`
    /// - `type Dict k v = { keys: List k, values: List v }`
    /// - `type Maybe a = | None | Some a`
    fn parseTypeDecl(self: *Parser) ParserError!*ast.Node {
        const type_token = try self.expect(lexer.TokenKind{ .keyword = .Type });

        if (try self.match(lexer.TokenKind{ .keyword = .Alias })) {
            const type_ident = try self.parseUpperIdentifier();

            var type_params = std.ArrayList([]const u8).init(self.allocator);
            errdefer {
                for (type_params.items) |param| {
                    self.allocator.free(param);
                }

                type_params.deinit();
            }

            while (self.check(lexer.TokenKind{ .identifier = .Lower })) {
                const param = try self.parseLowerIdentifier();

                const duped = try self.allocator.dupe(u8, param.name);
                errdefer self.allocator.free(duped);

                try type_params.append(duped);
            }

            _ = try self.expect(lexer.TokenKind{ .operator = .Equal });

            const value = try self.parseTypeExpr();
            errdefer {
                value.deinit(self.allocator);
                self.allocator.destroy(value);
            }

            const node = try self.allocator.create(ast.Node);
            errdefer self.allocator.destroy(node);

            node.* = .{
                .type_alias = .{
                    .name = try self.allocator.dupe(u8, type_ident.name),
                    .type_params = type_params,
                    .value = value,
                    .token = type_token,
                },
            };

            return node;
        }

        // Handle record and variant types - parse common prefix first
        const type_ident = try self.parseUpperIdentifier();

        var type_params = std.ArrayList([]const u8).init(self.allocator);
        errdefer {
            for (type_params.items) |param| {
                self.allocator.free(param);
            }

            type_params.deinit();
        }

        while (self.check(lexer.TokenKind{ .identifier = .Lower })) {
            const param = try self.parseLowerIdentifier();

            const duped = try self.allocator.dupe(u8, param.name);
            errdefer self.allocator.free(duped);

            try type_params.append(duped);
        }

        _ = try self.expect(lexer.TokenKind{ .operator = .Equal });

        // Branch based on what follows the equals sign
        if (try self.match(lexer.TokenKind{ .delimiter = .LeftBrace })) {
            // Parse record type fields
            var fields = std.ArrayList(ast.RecordFieldNode).init(self.allocator);
            errdefer {
                for (fields.items) |field| {
                    field.type.deinit(self.allocator);
                    self.allocator.destroy(field.type);
                }

                fields.deinit();
            }

            // Parse fields until we hit closing brace
            while (!self.check(lexer.TokenKind{ .delimiter = .RightBrace })) {
                const field_name = try self.parseLowerIdentifier();
                _ = try self.expect(lexer.TokenKind{ .delimiter = .Colon });

                const field_type = try self.parseTypeExpr();
                errdefer {
                    field_type.deinit(self.allocator);
                    self.allocator.destroy(field_type);
                }

                try fields.append(.{
                    .name = try self.allocator.dupe(u8, field_name.name),
                    .type = field_type,
                    .token = field_name.token,
                });

                // Handle comma between fields
                if (!self.check(lexer.TokenKind{ .delimiter = .RightBrace })) {
                    _ = try self.expect(lexer.TokenKind{ .delimiter = .Comma });
                }
            }

            _ = try self.expect(lexer.TokenKind{ .delimiter = .RightBrace });

            const node = try self.allocator.create(ast.Node);
            errdefer self.allocator.destroy(node);

            node.* = .{
                .record_type = .{
                    .name = try self.allocator.dupe(u8, type_ident.name),
                    .type_params = type_params,
                    .fields = fields,
                    .token = type_token,
                },
            };

            return node;
        }

        if (try self.match(lexer.TokenKind{ .symbol = .Pipe })) {
            // Parse variant type constructors
            var constructors = std.ArrayList(ast.VariantConstructorNode).init(self.allocator);
            errdefer {
                for (constructors.items) |*constructor| {
                    constructor.params.deinit();
                }

                constructors.deinit();
            }

            // Parse first constructor
            const constructor = try self.parseUpperIdentifier();

            var params = std.ArrayList(*ast.Node).init(self.allocator);
            errdefer {
                for (params.items) |param| {
                    param.deinit(self.allocator);
                    self.allocator.destroy(param);
                }

                params.deinit();
            }

            // Parse any parameters that follow this constructor
            while (!self.check(lexer.TokenKind{ .symbol = .Pipe }) and
                !self.check(lexer.TokenKind{ .special = .Eof }))
            {
                const param = try self.parseTypeExpr();
                try params.append(param);
            }

            try constructors.append(.{
                .name = constructor.name,
                .params = params,
                .token = constructor.token,
            });

            // Parse remaining constructors
            while (try self.match(lexer.TokenKind{ .symbol = .Pipe })) {
                const next_constructor = try self.parseUpperIdentifier();

                var next_params = std.ArrayList(*ast.Node).init(self.allocator);
                errdefer {
                    for (next_params.items) |param| {
                        param.deinit(self.allocator);
                        self.allocator.destroy(param);
                    }

                    next_params.deinit();
                }

                // Parse any parameters that follow this constructor
                while (!self.check(lexer.TokenKind{ .symbol = .Pipe }) and
                    !self.check(lexer.TokenKind{ .special = .Eof }))
                {
                    const param = try self.parseTypeExpr();
                    try next_params.append(param);
                }

                try constructors.append(.{
                    .name = next_constructor.name,
                    .params = next_params,
                    .token = next_constructor.token,
                });
            }

            const node = try self.allocator.create(ast.Node);
            errdefer self.allocator.destroy(node);

            node.* = .{
                .variant_type = .{
                    .name = try self.allocator.dupe(u8, type_ident.name),
                    .type_params = type_params,
                    .constructors = constructors,
                    .token = type_token,
                },
            };

            return node;
        }

        return error.UnexpectedToken;
    }

    /// Parses a function declaration with an optional type annotation.
    ///
    /// Examples:
    /// - `let add = \x y => x + y`
    /// - `let factorial : Int -> Int = \n => if n == 0 then 1 else n * factorial (n - 1)`
    /// - `let const : a -> b -> a = \x y => x`
    fn parseFunctionDecl(self: *Parser) ParserError!*ast.Node {
        const token = try self.expect(lexer.TokenKind{ .keyword = .Let });
        const name = try self.expect(lexer.TokenKind{ .identifier = .Lower });

        var type_annotation: ?*ast.Node = null;
        if (self.check(lexer.TokenKind{ .delimiter = .Colon })) {
            try self.advance();

            type_annotation = try self.parseTypeExpr();
            errdefer {
                type_annotation.deinit(self.allocator);
                self.allocator.destroy(type_annotation);
            }
        }

        _ = try self.expect(lexer.TokenKind{ .operator = .Equal });

        const value = try self.parseExpression();

        const node = try self.allocator.create(ast.Node);
        errdefer {
            value.deinit(self.allocator);

            if (type_annotation) |ta| {
                ta.deinit(self.allocator);
                self.allocator.destroy(ta);
            }

            self.allocator.destroy(node);
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

    /// Parses a foreign function declaration that links to external code.
    /// The declaration specifies an internal function name and type signature
    /// along with the external symbol name to link against.
    ///
    /// Format:
    /// foreign <name> : <type> = "<external_name>"
    ///
    /// Examples:
    /// - `foreign sqrt : Float -> Float = "c_sqrt"`
    /// - `foreign print : String -> Unit = "c_print"`
    fn parseForeignFunctionDecl(self: *Parser) ParserError!*ast.Node {
        const start_token = self.current_token;

        _ = try self.expect(lexer.TokenKind{ .keyword = .Foreign });
        const name = try self.parseLowerIdentifier();

        _ = try self.expect(lexer.TokenKind{ .delimiter = .Colon });
        const type_annotation = try self.parseTypeExpr();
        errdefer type_annotation.deinit(self.allocator);

        _ = try self.expect(lexer.TokenKind{ .operator = .Equal });
        const external_name = try self.parseStrLiteral();

        const node = try self.allocator.create(ast.Node);
        errdefer self.allocator.destroy(node);

        node.* = .{
            .foreign_function_decl = .{
                .name = name.name,
                .type_annotation = type_annotation,
                .external_name = external_name.value,
                .token = start_token,
            },
        };

        return node;
    }

    /// Parses an include declaration which imports and re-exports all contents from a module.
    ///
    /// Syntax:
    /// include ModulePath
    ///
    /// Examples:
    /// - `include MyModule`
    /// - `include Std.List`
    /// - `include Parser.Internal.Utils`
    fn parseInclude(self: *Parser) ParserError!*ast.Node {
        const token = try self.expect(lexer.TokenKind{ .keyword = .Include });

        const path_node = try self.parseModulePath();
        errdefer {
            path_node.deinit(self.allocator);
            self.allocator.destroy(path_node);
        }

        const node = try self.allocator.create(ast.Node);
        errdefer self.allocator.destroy(node);

        node.* = .{
            .include = .{
                .path = path_node.module_path,
                .token = token,
            },
        };

        self.allocator.destroy(path_node);

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

    /// Implements precedence climbing to parse binary operator expressions.
    /// Uses operator precedence and associativity from getOperatorInfo() to build
    /// correctly structured expression trees.
    fn parseBinaryExpr(self: *Parser, min_precedence: u8) ParserError!*ast.Node {
        var left = try self.parseSimpleExpr();
        errdefer {
            left.deinit(self.allocator);
            self.allocator.destroy(left);
        }

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
            errdefer {
                right.deinit(self.allocator);
                self.allocator.destroy(right);
            }

            const node = try self.allocator.create(ast.Node);
            errdefer {
                right.deinit(self.allocator);
                self.allocator.destroy(right);
                self.allocator.destroy(node);
            }

            node.* = switch (operator.kind) {
                .operator => |op| switch (op) {
                    .Exp,
                    .FloatAdd,
                    .FloatDiv,
                    .FloatMul,
                    .FloatSub,
                    .IntAdd,
                    .IntDiv,
                    .IntMul,
                    .IntSub,
                    => .{
                        .arithmetic_expr = .{
                            .left = left,
                            .operator = operator,
                            .right = right,
                        },
                    },
                    .LogicalAnd,
                    .LogicalOr,
                    => .{
                        .logical_expr = .{
                            .left = left,
                            .operator = operator,
                            .right = right,
                        },
                    },
                    .Equality,
                    .GreaterThan,
                    .GreaterThanEqual,
                    .LessThan,
                    .LessThanEqual,
                    .NotEqual,
                    => .{
                        .comparison_expr = .{
                            .left = left,
                            .operator = operator,
                            .right = right,
                        },
                    },
                    .StrConcat => .{
                        .str_concat_expr = .{
                            .left = left,
                            .operator = operator,
                            .right = right,
                        },
                    },
                    else => unreachable,
                },
                else => unreachable,
            };

            left = node;
        }

        return left;
    }

    /// Identify if the current token _could_ be used as a unary operator.
    fn isUnaryOp(self: *Parser) bool {
        return self.check(lexer.TokenKind{ .operator = .IntSub });
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

    /// Parses a primary expression, such as literals, identifiers, or parenthesized expressions.
    /// Handles cases for basic literals (int, float, string, char), identifiers (lower and upper),
    /// and parenthesized expressions.
    fn parsePrimaryExpr(self: *Parser) ParserError!*ast.Node {
        switch (self.current_token.kind) {
            .delimiter => |delim| switch (delim) {
                .LeftParen => {
                    try self.advance();

                    const expr = try self.parseExpression();

                    _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

                    return expr;
                },
                else => return error.UnexpectedToken,
            },
            .identifier => |ident| {
                const node = try self.allocator.create(ast.Node);
                errdefer self.allocator.destroy(node);

                switch (ident) {
                    .Lower => {
                        const identifier = try self.parseLowerIdentifier();
                        node.* = .{ .lower_identifier = identifier };
                    },
                    .Upper => {
                        const identifier = try self.parseUpperIdentifier();
                        node.* = .{ .upper_identifier = identifier };
                    },
                }

                return node;
            },
            .keyword => |kw| switch (kw) {
                .If => return try self.parseIfThenElse(),
                else => return error.UnexpectedToken,
            },
            .literal => |lit| {
                const node = try self.allocator.create(ast.Node);
                errdefer self.allocator.destroy(node);

                switch (lit) {
                    .Char => {
                        const char_literal = try self.parseCharLiteral();

                        node.* = .{
                            .char_literal = char_literal,
                        };
                    },
                    .Float => {
                        const float_literal = try self.parseFloatLiteral();

                        node.* = .{
                            .float_literal = float_literal,
                        };
                    },
                    .Int => {
                        const int_literal = try self.parseIntLiteral();

                        node.* = .{
                            .int_literal = int_literal,
                        };
                    },
                    .String => {
                        const str_literal = try self.parseStrLiteral();

                        node.* = .{
                            .str_literal = str_literal,
                        };
                    },
                    .MultilineString => return error.UnexpectedToken,
                }

                return node;
            },
            .operator => |op| switch (op) {
                .Lambda => return try self.parseLambdaExpr(),
                else => return error.UnexpectedToken,
            },
            else => return error.UnexpectedToken,
        }
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
        const token = try self.expect(lexer.TokenKind{ .operator = .Lambda });

        var params = std.ArrayList([]const u8).init(self.allocator);
        errdefer params.deinit();

        while (self.check(lexer.TokenKind{ .identifier = .Lower })) {
            const param = try self.expect(lexer.TokenKind{ .identifier = .Lower });
            try params.append(param.lexeme);
        }

        if (params.items.len == 0) return error.EmptyLambdaParams;

        _ = try self.expect(lexer.TokenKind{ .symbol = .DoubleArrowRight });
        const body = try self.parseExpression();
        errdefer {
            params.deinit();
            body.deinit(self.allocator);
            self.allocator.destroy(body);
        }

        const node = try self.allocator.create(ast.Node);
        errdefer {
            params.deinit();
            body.deinit(self.allocator);
            self.allocator.destroy(body);
            self.allocator.destroy(node);
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
        _ = try self.expect(lexer.TokenKind{ .keyword = .If });
        const condition = try self.parseExpression();

        _ = try self.expect(lexer.TokenKind{ .keyword = .Then });
        const then_branch = try self.parseExpression();

        _ = try self.expect(lexer.TokenKind{ .keyword = .Else });
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

    /// Parses a type expression, which can be a simple type name or a more complex
    /// parameterized type.
    ///
    /// Examples:
    /// - `Int`
    /// - `Maybe a`
    /// - `List String`
    fn parseTypeExpr(self: *Parser) ParserError!*ast.Node {
        const start_token = self.current_token;
        var first_type = try self.parseSimpleType();
        errdefer {
            first_type.deinit(self.allocator);
            self.allocator.destroy(first_type);
        }

        if (!try self.match(lexer.TokenKind{ .symbol = .ArrowRight })) {
            return first_type;
        }

        const second_type = try self.parseTypeExpr();
        errdefer {
            second_type.deinit(self.allocator);
            self.allocator.destroy(second_type);
        }

        var param_types = std.ArrayList(*ast.Node).init(self.allocator);
        errdefer {
            for (param_types.items) |param| {
                param.deinit(self.allocator);
                self.allocator.destroy(param);
            }

            param_types.deinit();
        }

        try param_types.append(first_type);

        if (second_type.* == .function_type) {
            for (second_type.function_type.param_types.items) |param| {
                try param_types.append(param);
            }

            second_type.function_type.param_types.deinit();
            self.allocator.destroy(second_type);
        } else {
            try param_types.append(second_type);
        }

        const node = try self.allocator.create(ast.Node);
        errdefer self.allocator.destroy(node);

        node.* = .{
            .function_type = .{
                .param_types = param_types,
                .token = start_token,
            },
        };

        return node;
    }

    /// Helper function to parse non-function types
    fn parseSimpleType(self: *Parser) ParserError!*ast.Node {
        if (self.check(lexer.TokenKind{ .identifier = .Upper })) {
            const type_token = self.current_token;
            const base_type = try self.parseUpperIdentifier();

            const base_node = try self.allocator.create(ast.Node);
            errdefer self.allocator.destroy(base_node);

            base_node.* = .{
                .upper_identifier = base_type,
            };

            // Check for type arguments
            var type_args = std.ArrayList(*ast.Node).init(self.allocator);
            errdefer {
                for (type_args.items) |arg| {
                    arg.deinit(self.allocator);
                    self.allocator.destroy(arg);
                }

                type_args.deinit();
            }

            var has_args = false;

            // Parse sequence of type arguments which can be:
            // 1. Lower identifiers (type variables like 'a')
            // 2. Upper identifiers (concrete types like 'String')
            // 3. Parenthesized type expressions (like '(Maybe a)')
            while (true) {
                if (self.check(lexer.TokenKind{ .identifier = .Lower })) {
                    // Handle type variables (e.g., 'a' in 'List a')
                    has_args = true;

                    const arg_node = try self.allocator.create(ast.Node);
                    errdefer self.allocator.destroy(arg_node);

                    const arg = try self.parseLowerIdentifier();
                    arg_node.* = .{
                        .lower_identifier = arg,
                    };

                    try type_args.append(arg_node);
                } else if (self.check(lexer.TokenKind{ .identifier = .Upper })) {
                    // Handle concrete types (e.g., 'String' in 'List String')
                    has_args = true;

                    const arg_node = try self.allocator.create(ast.Node);
                    errdefer self.allocator.destroy(arg_node);

                    const arg = try self.parseUpperIdentifier();
                    arg_node.* = .{
                        .upper_identifier = arg,
                    };

                    try type_args.append(arg_node);
                } else if (try self.match(lexer.TokenKind{ .delimiter = .LeftParen })) {
                    // Handle complex type expressions (e.g., '(Maybe a)' in 'List (Maybe a)')
                    has_args = true;

                    const inner_type = try self.parseTypeExpr();
                    errdefer {
                        inner_type.deinit(self.allocator);
                        self.allocator.destroy(inner_type);
                    }

                    _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

                    try type_args.append(inner_type);
                } else {
                    break;
                }
            }

            if (!has_args) return base_node;

            const node = try self.allocator.create(ast.Node);
            errdefer {
                base_node.deinit(self.allocator);
                self.allocator.destroy(base_node);

                for (type_args.items) |arg| {
                    arg.deinit(self.allocator);
                    self.allocator.destroy(arg);
                }

                type_args.deinit();
                self.allocator.destroy(node);
            }

            node.* = .{
                .type_application = .{
                    .base = base_node,
                    .args = type_args,
                    .token = type_token,
                },
            };

            return node;
        }

        // Handle type variables
        if (self.check(lexer.TokenKind{ .identifier = .Lower })) {
            const type_var = try self.expect(lexer.TokenKind{ .identifier = .Lower });

            const node = try self.allocator.create(ast.Node);
            errdefer self.allocator.destroy(node);

            node.* = .{
                .lower_identifier = .{
                    .name = type_var.lexeme,
                    .token = type_var,
                },
            };

            return node;
        }

        // Handle parenthesized type expressions
        if (try self.match(lexer.TokenKind{ .delimiter = .LeftParen })) {
            const inner_type = try self.parseTypeExpr();
            errdefer {
                inner_type.deinit(self.allocator);
                self.allocator.destroy(inner_type);
            }

            _ = try self.expect(lexer.TokenKind{ .delimiter = .RightParen });

            return inner_type;
        }

        return error.UnexpectedToken;
    }

    /// Parses a module path consisting of one or more uppercase identifiers separated by dots.
    /// Used by module declarations, includes, and imports.
    ///
    /// Syntax:
    /// ModulePath = UpperIdent ("." UpperIdent)*
    ///
    /// Examples:
    /// - `MyModule`
    /// - `Std.List`
    /// - `Parser.Internal.Utils`
    fn parseModulePath(self: *Parser) ParserError!*ast.Node {
        var segments = std.ArrayList([]const u8).init(self.allocator);
        errdefer {
            for (segments.items) |seg| {
                self.allocator.free(seg);
            }

            segments.deinit();
        }

        const first_segment = try self.parseUpperIdentifier();
        const first_duped = try self.allocator.dupe(u8, first_segment.name);
        try segments.append(first_duped);

        while (try self.match(lexer.TokenKind{ .delimiter = .Dot })) {
            const next_segment = try self.parseUpperIdentifier();
            const next_duped = try self.allocator.dupe(u8, next_segment.name);
            try segments.append(next_duped);
        }

        const node = try self.allocator.create(ast.Node);
        errdefer self.allocator.destroy(node);

        node.* = .{
            .module_path = .{
                .segments = segments,
                .token = first_segment.token,
            },
        };

        return node;
    }

    /// Parses a regular comment node from the input.
    fn parseComment(self: *Parser) ParserError!*ast.Node {
        const token = try self.expect(lexer.TokenKind{ .comment = .Regular });

        const node = try self.allocator.create(ast.Node);
        errdefer self.allocator.destroy(node);

        // Content starts after '# ' prefix
        var content_start: usize = 1;
        while (content_start < token.lexeme.len and std.ascii.isWhitespace(token.lexeme[content_start])) {
            content_start += 1;
        }
        const trimmed = std.mem.trimRight(u8, token.lexeme[content_start..], &std.ascii.whitespace);

        node.* = .{
            .comment = .{
                .content = trimmed,
                .token = token,
            },
        };

        return node;
    }

    /// Parses a documentation comment node from the input.
    fn parseDocComment(self: *Parser) ParserError!*ast.Node {
        const token = try self.expect(lexer.TokenKind{ .comment = .Doc });

        const node = try self.allocator.create(ast.Node);
        errdefer self.allocator.destroy(node);

        // Content starts after '## ' prefix
        var content_start: usize = 2;
        while (content_start < token.lexeme.len and std.ascii.isWhitespace(token.lexeme[content_start])) {
            content_start += 1;
        }
        const trimmed = std.mem.trimRight(u8, token.lexeme[content_start..], &std.ascii.whitespace);

        node.* = .{
            .doc_comment = .{
                .content = trimmed,
                .token = token,
            },
        };

        return node;
    }
};

const testing = std.testing;

const TEST_FILE = "test.mox";

test "[comment]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const source = "# This is a regular comment";
    const expected_content = "This is a regular comment";
    var l = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(allocator, &l);
    defer parser.deinit();

    // Action
    const node = try parser.parseComment();
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    // Assertions
    // Check the node type is correctly identified as a comment
    try testing.expect(node.* == .comment);

    // Validate the comment token and its properties
    const comment = node.comment;
    try testing.expectEqual(lexer.TokenKind{ .comment = .Regular }, comment.token.kind);

    // Ensure the content of the comment matches the source
    try testing.expectEqualStrings(expected_content, comment.content);
}

test "[doc_comment]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const source = "## This is a doc comment";
    const expected_content = "This is a doc comment";
    var l = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(allocator, &l);
    defer parser.deinit();

    // Action
    const node = try parser.parseDocComment();
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    // Assertions
    // Check the node type is correctly identified as a doc comment
    try testing.expect(node.* == .doc_comment);

    // Validate the comment token and its properties
    const comment = node.doc_comment;
    try testing.expectEqual(lexer.TokenKind{ .comment = .Doc }, comment.token.kind);

    // Ensure the content of the comment matches the source
    try testing.expectEqualStrings(expected_content, comment.content);
}

test "[int_literal]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const lit = try parser.parseIntLiteral();

        // Assertions
        // Validate the literal token and its properties
        try testing.expectEqual(lexer.TokenKind{ .literal = .Int }, lit.token.kind);

        // Ensure the lexeme matches the source string
        try testing.expectEqualStrings(case.source, lit.token.lexeme);

        // Verify the parsed integer value matches the expected value
        try testing.expectEqual(case.expected, lit.value);
    }
}

test "[float_literal]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const lit = try parser.parseFloatLiteral();

        // Assertions
        // Validate the literal token and its properties
        try testing.expectEqual(lexer.TokenKind{ .literal = .Float }, lit.token.kind);

        // Ensure the lexeme matches the source string
        try testing.expectEqualStrings(case.source, lit.token.lexeme);

        // Verify the parsed float value matches the expected value
        try testing.expectEqual(case.expected, lit.value);
    }
}

test "[str_literal]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const lit = try parser.parseStrLiteral();
        defer allocator.free(lit.value);

        // Assertions
        // Validate the literal token and its properties
        try testing.expectEqual(lexer.TokenKind{ .literal = .String }, lit.token.kind);

        // Ensure the lexeme matches the source string
        try testing.expectEqualStrings(case.source, lit.token.lexeme);

        // Verify the parsed string value matches the expected value
        try testing.expectEqualStrings(case.expected, lit.value);
    }
}

test "[char_literal]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const lit = try parser.parseCharLiteral();

        // Assertions
        // Validate the literal token and its properties
        try testing.expectEqual(lexer.TokenKind{ .literal = .Char }, lit.token.kind);

        // Ensure the lexeme matches the source string
        try testing.expectEqualStrings(case.source, lit.token.lexeme);

        // Verify the parsed char value matches the expected value
        try testing.expectEqual(case.expected, lit.value);
    }
}

test "[lower_identifier]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const ident = try parser.parseLowerIdentifier();

        // Assertions
        // Verify the token is recognized as a lower identifier
        try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, ident.token.kind);

        // Ensure the lexeme matches the source string
        try testing.expectEqualStrings(case.source, ident.token.lexeme);

        // Verify the parsed identifier name matches the expected name
        try testing.expectEqualStrings(case.expected, ident.name);
    }
}

test "[upper_identifier]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const ident = try parser.parseUpperIdentifier();

        // Assertions
        // Verify the token is recognized as an upper identifier
        try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, ident.token.kind);

        // Ensure the lexeme matches the source string
        try testing.expectEqualStrings(case.source, ident.token.lexeme);

        // Verify the parsed identifier name matches the expected name
        try testing.expectEqualStrings(case.expected, ident.name);
    }
}

test "[unary_expr]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const source = "-42";
    var l = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(allocator, &l);
    defer parser.deinit();

    // Action
    const expr = try parser.parseSimpleExpr();
    defer {
        expr.deinit(allocator);
        allocator.destroy(expr);
    }

    // Assertions
    // Ensure the expression is identified as a unary expression
    try testing.expect(expr.* == .unary_expr);

    // Validate the operator in the unary expression
    try testing.expectEqual(lexer.TokenKind{ .operator = .IntSub }, expr.unary_expr.operator.kind);
    try testing.expectEqualStrings("-", expr.unary_expr.operator.lexeme);

    const operand = expr.unary_expr.operand;

    // Ensure the operand is an integer literal
    try testing.expect(operand.* == .int_literal);

    // Verify the integer value of the operand is correct
    try testing.expect(operand.int_literal.value == 42);
}

test "[arithmetic_expr]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const source = "42 + 24";
    var l = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(allocator, &l);
    defer parser.deinit();

    // Action
    const expr = try parser.parseExpression();
    defer {
        expr.deinit(allocator);
        allocator.destroy(expr);
    }

    // Assertions
    // Ensure the expression is identified as an arithmetic expression
    try testing.expect(expr.* == .arithmetic_expr);

    // Validate the operator in the arithmetic expression
    try testing.expectEqual(lexer.TokenKind{ .operator = .IntAdd }, expr.arithmetic_expr.operator.kind);

    const left = expr.arithmetic_expr.left;

    // Ensure the left operand is an integer literal
    try testing.expect(left.* == .int_literal);

    // Verify the integer value of the left operand is correct
    try testing.expect(left.int_literal.value == 42);

    const right = expr.arithmetic_expr.right;

    // Ensure the right operand is an integer literal
    try testing.expect(right.* == .int_literal);

    // Verify the integer value of the right operand is correct
    try testing.expect(right.int_literal.value == 24);
}

test "[comparison_expr]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const TestCase = struct {
        source: []const u8,
        op: lexer.TokenKind,
        left_val: i64,
        right_val: i64,
    };

    const cases = [_]TestCase{
        .{
            .source = "42 == 24",
            .op = lexer.TokenKind{ .operator = .Equality },
            .left_val = 42,
            .right_val = 24,
        },
        .{
            .source = "42 /= 24",
            .op = lexer.TokenKind{ .operator = .NotEqual },
            .left_val = 42,
            .right_val = 24,
        },
        .{
            .source = "42 < 24",
            .op = lexer.TokenKind{ .operator = .LessThan },
            .left_val = 42,
            .right_val = 24,
        },
        .{
            .source = "42 > 24",
            .op = lexer.TokenKind{ .operator = .GreaterThan },
            .left_val = 42,
            .right_val = 24,
        },
        .{
            .source = "42 <= 24",
            .op = lexer.TokenKind{ .operator = .LessThanEqual },
            .left_val = 42,
            .right_val = 24,
        },
        .{
            .source = "42 >= 24",
            .op = lexer.TokenKind{ .operator = .GreaterThanEqual },
            .left_val = 42,
            .right_val = 24,
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const expr = try parser.parseExpression();
        defer {
            expr.deinit(allocator);
            allocator.destroy(expr);
        }

        // Assertions
        // Ensure the expression is identified as a comparison expression
        try testing.expect(expr.* == .comparison_expr);

        // Validate that the operator in the comparison expression matches the expected operator
        try testing.expectEqual(case.op, expr.comparison_expr.operator.kind);

        const left = expr.comparison_expr.left;

        // Ensure the left operand is an integer literal
        try testing.expect(left.* == .int_literal);

        // Verify the value of the left operand matches the expected value
        try testing.expectEqual(case.left_val, left.int_literal.value);

        const right = expr.comparison_expr.right;

        // Ensure the right operand is an integer literal
        try testing.expect(right.* == .int_literal);

        // Verify the value of the right operand matches the expected value
        try testing.expectEqual(case.right_val, right.int_literal.value);
    }
}

test "[logical_expr]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const TestCase = struct {
        source: []const u8,
        op: lexer.TokenKind,
    };

    const cases = [_]TestCase{
        .{
            .source = "true && false",
            .op = lexer.TokenKind{ .operator = .LogicalAnd },
        },
        .{
            .source = "true || false",
            .op = lexer.TokenKind{ .operator = .LogicalOr },
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const expr = try parser.parseExpression();
        defer {
            expr.deinit(allocator);
            allocator.destroy(expr);
        }

        // Assertions
        // Ensure the expression is identified as a logical expression
        try testing.expect(expr.* == .logical_expr);

        // Validate that the operator in the logical expression matches the expected operator
        try testing.expectEqual(case.op, expr.logical_expr.operator.kind);

        const left = expr.logical_expr.left;

        // Ensure the left operand is a lower identifier
        try testing.expect(left.* == .lower_identifier);

        // Verify the name of the left operand matches the expected value
        try testing.expectEqualStrings("true", left.lower_identifier.name);

        const right = expr.logical_expr.right;

        // Ensure the right operand is a lower identifier
        try testing.expect(right.* == .lower_identifier);

        // Verify the name of the right operand matches the expected value
        try testing.expectEqualStrings("false", right.lower_identifier.name);
    }
}

test "[operator precedence]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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
            .root_op = lexer.TokenKind{ .operator = .IntAdd },
        },
        .{
            .source = "1 * 2 + 3", // Should be (1 * 2) + 3
            .root_type = .arithmetic_expr,
            .root_op = lexer.TokenKind{ .operator = .IntAdd },
        },
        .{
            .source = "2 ** 3 * 4", // Should be (2 ** 3) * 4
            .root_type = .arithmetic_expr,
            .root_op = lexer.TokenKind{ .operator = .IntMul },
        },

        // Comparison and arithmetic
        .{
            .source = "1 + 2 == 3 * 4", // Should be (1 + 2) == (3 * 4)
            .root_type = .comparison_expr,
            .root_op = lexer.TokenKind{ .operator = .Equality },
        },

        // Logical operators
        .{
            .source = "a && b || c", // Should be (a && b) || c
            .root_type = .logical_expr,
            .root_op = lexer.TokenKind{ .operator = .LogicalOr },
        },
        .{
            .source = "a || b && c", // Should be a || (b && c)
            .root_type = .logical_expr,
            .root_op = lexer.TokenKind{ .operator = .LogicalOr },
        },

        // Mixed operators
        .{
            .source = "1 + 2 * 3 == 4 && 5 * 6 == 7", // Should be ((1 + (2 * 3)) == 4) && ((5 * 6) == 7)
            .root_type = .logical_expr,
            .root_op = lexer.TokenKind{ .operator = .LogicalAnd },
        },
        .{
            .source = "2 ** 3 ** 4", // Should be 2 ** (3 ** 4) due to right associativity
            .root_type = .arithmetic_expr,
            .root_op = lexer.TokenKind{ .operator = .Exp },
        },
        .{
            .source = "a && b && c || d && e", // Should be ((a && b) && c) || (d && e)
            .root_type = .logical_expr,
            .root_op = lexer.TokenKind{ .operator = .LogicalOr },
        },
        .{
            .source = "1 * 2 + 3 * 4 + 5", // Should be ((1 * 2) + (3 * 4)) + 5
            .root_type = .arithmetic_expr,
            .root_op = lexer.TokenKind{ .operator = .IntAdd },
        },
        .{
            .source = "1 + 2 * 3 ** 4 * 5 + 6", // Should be 1 + ((2 * (3 ** 4)) * 5) + 6
            .root_type = .arithmetic_expr,
            .root_op = lexer.TokenKind{ .operator = .IntAdd },
        },
        .{
            .source = "a || b && c && d || e", // Should be (a || ((b && c) && d)) || e
            .root_type = .logical_expr,
            .root_op = lexer.TokenKind{ .operator = .LogicalOr },
        },
        .{
            .source = "1 * 2 + 3 == 4 + 5 * 6 && 7 + 8 == 9", // Should be (((1 * 2) + 3) == (4 + (5 * 6))) && ((7 + 8) == 9)
            .root_type = .logical_expr,
            .root_op = lexer.TokenKind{ .operator = .LogicalAnd },
        },
        .{
            .source = "a <= b + c * d && e > f ** g || h == i", // Should be ((a <= (b + (c * d))) && (e > (f ** g))) || (h == i)
            .root_type = .logical_expr,
            .root_op = lexer.TokenKind{ .operator = .LogicalOr },
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(case.source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const expr = try parser.parseExpression();
        defer {
            expr.deinit(allocator);
            allocator.destroy(expr);
        }

        // Assertions
        // Ensure the root type of the expression matches the expected type from the test case
        try testing.expectEqual(case.root_type, std.meta.activeTag(expr.*));

        switch (expr.*) {
            .arithmetic_expr => |aexpr| {
                // Validate the operator in an arithmetic expression
                try testing.expectEqual(case.root_op, aexpr.operator.kind);
            },
            .logical_expr => |lexpr| {
                // Validate the operator in a logical expression
                try testing.expectEqual(case.root_op, lexpr.operator.kind);
            },
            .comparison_expr => |cexpr| {
                // Validate the operator in a comparison expression
                try testing.expectEqual(case.root_op, cexpr.operator.kind);
            },
            else => unreachable,
        }
    }
}

test "[operator precedence] (structural)" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        var l = lexer.Lexer.init("1 + 2 * 3", TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const expr = try parser.parseExpression();
        defer {
            expr.deinit(allocator);
            allocator.destroy(expr);
        }

        // Assertions
        // The expression should be structured as: (1 + (2 * 3))

        // Validate the root expression is an arithmetic expression with addition as the operator
        try testing.expect(expr.* == .arithmetic_expr);
        try testing.expectEqual(lexer.TokenKind{ .operator = .IntAdd }, expr.arithmetic_expr.operator.kind);

        const left = expr.arithmetic_expr.left;

        // Ensure the left operand is an integer literal with value 1
        try testing.expect(left.* == .int_literal);
        try testing.expectEqual(@as(i64, 1), left.int_literal.value);

        const right = expr.arithmetic_expr.right;

        // Ensure the right operand is an arithmetic expression with multiplication as the operator
        try testing.expect(right.* == .arithmetic_expr);
        try testing.expectEqual(lexer.TokenKind{ .operator = .IntMul }, right.arithmetic_expr.operator.kind);

        const mul_left = right.arithmetic_expr.left;

        // Ensure the left operand of the multiplication is an integer literal with value 2
        try testing.expect(mul_left.* == .int_literal);
        try testing.expectEqual(@as(i64, 2), mul_left.int_literal.value);

        const mul_right = right.arithmetic_expr.right;

        // Ensure the right operand of the multiplication is an integer literal with value 3
        try testing.expect(mul_right.* == .int_literal);
        try testing.expectEqual(@as(i64, 3), mul_right.int_literal.value);
    }

    {
        var l = lexer.Lexer.init("1 * 2 + 3 == 4", TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const expr = try parser.parseExpression();
        defer {
            expr.deinit(allocator);
            allocator.destroy(expr);
        }

        // Assertions
        // The expression should be structured as: ((1 * 2 + 3) == 4)

        // Validate the root expression is a comparison expression with equality as the operator
        try testing.expect(expr.* == .comparison_expr);
        try testing.expectEqual(lexer.TokenKind{ .operator = .Equality }, expr.comparison_expr.operator.kind);

        const right = expr.comparison_expr.right;

        // Ensure the right operand is an integer literal with value 4
        try testing.expect(right.* == .int_literal);
        try testing.expectEqual(@as(i64, 4), right.int_literal.value);

        const left = expr.comparison_expr.left;

        // Ensure the left operand is an arithmetic expression with addition as the operator
        try testing.expect(left.* == .arithmetic_expr);
        try testing.expectEqual(lexer.TokenKind{ .operator = .IntAdd }, left.arithmetic_expr.operator.kind);

        const add_right = left.arithmetic_expr.right;

        // Ensure the right operand of the addition is an integer literal with value 3
        try testing.expect(add_right.* == .int_literal);
        try testing.expectEqual(@as(i64, 3), add_right.int_literal.value);

        const add_left = left.arithmetic_expr.left;

        // Ensure the left operand of the addition is an arithmetic expression with multiplication as the operator
        try testing.expect(add_left.* == .arithmetic_expr);
        try testing.expectEqual(lexer.TokenKind{ .operator = .IntMul }, add_left.arithmetic_expr.operator.kind);

        const mul_left = add_left.arithmetic_expr.left;

        // Ensure the left operand of the multiplication is an integer literal with value 1
        try testing.expect(mul_left.* == .int_literal);
        try testing.expectEqual(@as(i64, 1), mul_left.int_literal.value);

        const mul_right = add_left.arithmetic_expr.right;

        // Ensure the right operand of the multiplication is an integer literal with value 2
        try testing.expect(mul_right.* == .int_literal);
        try testing.expectEqual(@as(i64, 2), mul_right.int_literal.value);
    }
}

test "[lambda_expr]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const source = "\\x => x + 1";
    var l = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(allocator, &l);
    defer parser.deinit();

    // Action
    const expr = try parser.parseExpression();
    defer {
        expr.deinit(allocator);
        allocator.destroy(expr);
    }

    // Assertions
    // Ensure the expression is identified as a lambda expression
    try testing.expect(expr.* == .lambda_expr);

    // Validate that the lambda expression has exactly one parameter
    try testing.expectEqual(@as(usize, 1), expr.lambda_expr.params.items.len);

    // Ensure the parameter name matches the expected value ("x")
    try testing.expectEqualStrings("x", expr.lambda_expr.params.items[0]);

    // Validate that the body of the lambda expression is an arithmetic expression
    try testing.expect(expr.lambda_expr.body.* == .arithmetic_expr);
}

test "[if_then_else_stmt]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const source = "if x > 0 then 1 else -1";
    var l = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(allocator, &l);
    defer parser.deinit();

    // Action
    const node = try parser.parseIfThenElse();
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    // Assertions
    // Ensure the node is identified as an if-then-else statement
    try testing.expect(node.* == .if_then_else_stmt);

    const stmt = node.if_then_else_stmt;

    // Validate the condition of the if-then-else statement
    try testing.expect(stmt.condition.* == .comparison_expr);

    const condition = stmt.condition.comparison_expr;

    // Ensure the condition's operator is a "greater than" comparison
    try testing.expectEqual(lexer.TokenKind{ .operator = .GreaterThan }, condition.operator.kind);

    // Check the left-hand side of the condition (should be the identifier "x")
    try testing.expect(condition.left.* == .lower_identifier);
    try testing.expectEqualStrings("x", condition.left.lower_identifier.name);

    // Check the right-hand side of the condition (should be the integer literal 0)
    try testing.expect(condition.right.* == .int_literal);
    try testing.expectEqual(@as(i64, 0), condition.right.int_literal.value);

    // Validate the "then" branch (should evaluate to integer literal 1)
    try testing.expect(stmt.then_branch.* == .int_literal);
    try testing.expectEqual(@as(i64, 1), stmt.then_branch.int_literal.value);

    // Validate the "else" branch (should evaluate to the unary expression -1)
    try testing.expect(stmt.else_branch.* == .unary_expr);

    const else_expr = stmt.else_branch.unary_expr;

    // Ensure the unary operator in the else branch is subtraction
    try testing.expectEqual(lexer.TokenKind{ .operator = .IntSub }, else_expr.operator.kind);

    // Ensure the operand of the unary expression is an integer literal 1
    try testing.expect(else_expr.operand.* == .int_literal);
    try testing.expectEqual(@as(i64, 1), else_expr.operand.int_literal.value);
}

test "[function_decl] (simple)" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Test a simple function declaration without type annotation
    const source = "let add = \\x y => x + y";
    var l = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(allocator, &l);
    defer parser.deinit();

    // Action
    const node = try parser.parseFunctionDecl();
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    // Assertions
    // Verify the node is a function declaration
    try testing.expect(node.* == .function_decl);

    const decl = node.function_decl;

    // Check the function name matches
    try testing.expectEqualStrings("add", decl.name);

    // Verify the type annotation is null for this simple function declaration
    try testing.expect(decl.type_annotation == null);

    // Verify the function's value is a lambda expression
    try testing.expect(decl.value.* == .lambda_expr);

    const lambda = decl.value.lambda_expr;

    // Check the lambda has exactly two parameters
    try testing.expectEqual(@as(usize, 2), lambda.params.items.len);

    // Verify the parameter names are "x" and "y"
    try testing.expectEqualStrings("x", lambda.params.items[0]);
    try testing.expectEqualStrings("y", lambda.params.items[1]);

    // Verify the lambda body is an arithmetic expression
    try testing.expect(lambda.body.* == .arithmetic_expr);

    const body = lambda.body.arithmetic_expr;

    // Ensure the operator in the arithmetic expression is addition
    try testing.expectEqual(lexer.TokenKind{ .operator = .IntAdd }, body.operator.kind);

    // Verify the left operand of the addition is a lower identifier with the name "x"
    try testing.expect(body.left.* == .lower_identifier);
    try testing.expectEqualStrings("x", body.left.lower_identifier.name);

    // Verify the right operand of the addition is a lower identifier with the name "y"
    try testing.expect(body.right.* == .lower_identifier);
    try testing.expectEqualStrings("y", body.right.lower_identifier.name);
}

test "[function_decl] (w/ type annotation)" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Test function declaration with type annotation
    const source = "let inc : Int -> Int = \\x => x + 1";
    var l = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(allocator, &l);
    defer parser.deinit();

    // Action
    const node = try parser.parseFunctionDecl();
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    // Assertions
    // Verify the node is a function declaration
    try testing.expect(node.* == .function_decl);

    const decl = node.function_decl;

    // Check the function name matches
    try testing.expectEqualStrings("inc", decl.name);

    // Verify type annotation exists and is correct
    try testing.expect(decl.type_annotation != null);
    try testing.expect(decl.type_annotation.?.* == .function_type);

    const type_annotation = decl.type_annotation.?.function_type;

    // Verify the function type has exactly two parameter types
    try testing.expectEqual(@as(usize, 2), type_annotation.param_types.items.len);

    // Check both parameter types are upper identifiers with the name "Int"
    try testing.expect(type_annotation.param_types.items[0].* == .upper_identifier);
    try testing.expectEqualStrings("Int", type_annotation.param_types.items[0].upper_identifier.name);
    try testing.expect(type_annotation.param_types.items[1].* == .upper_identifier);
    try testing.expectEqualStrings("Int", type_annotation.param_types.items[1].upper_identifier.name);

    // Verify the function's value is a lambda expression
    try testing.expect(decl.value.* == .lambda_expr);

    const lambda = decl.value.lambda_expr;

    // Check the lambda has exactly one parameter
    try testing.expectEqual(@as(usize, 1), lambda.params.items.len);

    // Verify the parameter name matches
    try testing.expectEqualStrings("x", lambda.params.items[0]);

    // Verify the body of the lambda is an arithmetic expression
    try testing.expect(lambda.body.* == .arithmetic_expr);

    const body = lambda.body.arithmetic_expr;

    // Ensure the operator in the arithmetic expression is addition
    try testing.expectEqual(lexer.TokenKind{ .operator = .IntAdd }, body.operator.kind);

    // Check the left operand of the addition is a lower identifier with the name "x"
    try testing.expect(body.left.* == .lower_identifier);
    try testing.expectEqualStrings("x", body.left.lower_identifier.name);

    // Check the right operand of the addition is an integer literal with the value 1
    try testing.expect(body.right.* == .int_literal);
    try testing.expectEqual(@as(i64, 1), body.right.int_literal.value);
}

test "[foreign_function_decl]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // foreign sqrt : Float -> Float = "c_sqrt"

    const source = "foreign sqrt : Float -> Float = \"c_sqrt\"";
    var l = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(allocator, &l);
    defer parser.deinit();

    // Action
    const node = try parser.parseForeignFunctionDecl();
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    // Assertions
    // Verify the node is a foreign function declaration
    try testing.expect(node.* == .foreign_function_decl);

    const decl = node.foreign_function_decl;

    // Verify the function name matches
    try testing.expectEqualStrings("sqrt", decl.name);

    // Verify the external name matches
    try testing.expectEqualStrings("c_sqrt", decl.external_name);

    // Ensure the type annotation exists and is a function type
    try testing.expect(decl.type_annotation.* == .function_type);

    const type_annotation = decl.type_annotation.function_type;

    // Verify the function type has exactly two parameter types
    try testing.expectEqual(@as(usize, 2), type_annotation.param_types.items.len);

    // Check both parameter types are upper identifiers with the name "Float"
    try testing.expect(type_annotation.param_types.items[0].* == .upper_identifier);
    try testing.expect(type_annotation.param_types.items[1].* == .upper_identifier);
    try testing.expectEqualStrings("Float", type_annotation.param_types.items[0].upper_identifier.name);
    try testing.expectEqualStrings("Float", type_annotation.param_types.items[1].upper_identifier.name);
}

test "[module_path]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const source = "Std.List";
    var l = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(allocator, &l);
    defer parser.deinit();

    // Action
    const node = try parser.parseModulePath();
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    // Assertions
    // Verify the node is a path to a module
    try testing.expect(node.* == .module_path);

    const module_path = node.module_path;

    // Check the module path has exactly two segments
    try testing.expectEqual(@as(usize, 2), module_path.segments.items.len);

    // Verify the segments
    try testing.expectEqualStrings("Std", module_path.segments.items[0]);
    try testing.expectEqualStrings("List", module_path.segments.items[1]);
}

test "[include]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const source = "include Std.List";
    var l = lexer.Lexer.init(source, TEST_FILE);
    var parser = try Parser.init(allocator, &l);
    defer parser.deinit();

    // Action
    const node = try parser.parseInclude();
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    // Assertions
    // Verify the node is an include statement
    try testing.expect(node.* == .include);

    const include = node.include;

    // Check the include path has exactly two segments
    try testing.expectEqual(@as(usize, 2), include.path.segments.items.len);

    // Verify the segments
    try testing.expectEqualStrings("Std", include.path.segments.items[0]);
    try testing.expectEqualStrings("List", include.path.segments.items[1]);
}

test "[type_alias]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        // Basic type
        const source = "type alias UserId = String";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is a type alias declaration
        try testing.expect(node.* == .type_alias);

        const type_alias = node.type_alias;

        // Verify the name of the type alias is "UserId"
        try testing.expectEqualStrings("UserId", type_alias.name);

        // Verify the value of the type alias is an upper identifier
        try testing.expect(type_alias.value.* == .upper_identifier);

        // Verify the name of the upper identifier is "String"
        try testing.expectEqualStrings("String", type_alias.value.upper_identifier.name);
    }

    {
        // Type parameters
        const source = "type alias Dict k v = Map k v";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify the node is a type alias declaration
        try testing.expect(node.* == .type_alias);

        const type_alias = node.type_alias;

        // Verify the name of the type alias is "Dict"
        try testing.expectEqualStrings("Dict", type_alias.name);

        // Verify the type alias has exactly two type parameters
        try testing.expectEqual(@as(usize, 2), type_alias.type_params.items.len);

        // Verify the type alias type parameters are: "k" and "v"
        try testing.expectEqualStrings("k", type_alias.type_params.items[0]);
        try testing.expectEqualStrings("v", type_alias.type_params.items[1]);

        // Check type application
        try testing.expect(type_alias.value.* == .type_application);

        const app = type_alias.value.type_application;

        // Verify the base type of the type application is "Map"
        try testing.expect(app.base.* == .upper_identifier);
        try testing.expectEqualStrings("Map", app.base.upper_identifier.name);

        // Verify the type application has exactly two arguments
        try testing.expectEqual(@as(usize, 2), app.args.items.len);

        // Verify the first argument is a lower identifier with the name "k"
        try testing.expect(app.args.items[0].* == .lower_identifier);
        try testing.expectEqualStrings("k", app.args.items[0].lower_identifier.name);

        // Verify the second argument is a lower identifier with the name "v"
        try testing.expect(app.args.items[1].* == .lower_identifier);
        try testing.expectEqualStrings("v", app.args.items[1].lower_identifier.name);
    }

    {
        // Function type
        const source = "type alias Reducer a b = a -> b -> b";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify that the parsed node represents a type alias declaration
        try testing.expect(node.* == .type_alias);

        const type_alias = node.type_alias;

        // Check the name and type parameters of the alias
        try testing.expectEqualStrings("Reducer", type_alias.name);
        try testing.expectEqual(@as(usize, 2), type_alias.type_params.items.len);
        try testing.expectEqualStrings("a", type_alias.type_params.items[0]);
        try testing.expectEqualStrings("b", type_alias.type_params.items[1]);

        // Ensure the type alias represents a function type
        try testing.expect(type_alias.value.* == .function_type);

        const func_type = type_alias.value.function_type;

        // The function type should have three type entries: 'a -> b -> b'
        try testing.expectEqual(@as(usize, 3), func_type.param_types.items.len);

        // Ensure the first parameter type is 'a'
        try testing.expect(func_type.param_types.items[0].* == .lower_identifier);
        try testing.expectEqualStrings("a", func_type.param_types.items[0].lower_identifier.name);

        // Ensure the second parameter type is 'b'
        try testing.expect(func_type.param_types.items[1].* == .lower_identifier);
        try testing.expectEqualStrings("b", func_type.param_types.items[1].lower_identifier.name);

        // Ensure the return type is also 'b'
        try testing.expect(func_type.param_types.items[2].* == .lower_identifier);
        try testing.expectEqualStrings("b", func_type.param_types.items[2].lower_identifier.name);
    }

    {
        // Complex nested type
        const source = "type alias TreeMap k v = Tree (Pair k v) (Compare k)";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Verify that the parsed node represents a type alias declaration
        try testing.expect(node.* == .type_alias);

        const type_alias = node.type_alias;

        // Verify the name and type parameters of the alias
        try testing.expectEqualStrings("TreeMap", type_alias.name);
        try testing.expectEqual(@as(usize, 2), type_alias.type_params.items.len);
        try testing.expectEqualStrings("k", type_alias.type_params.items[0]);
        try testing.expectEqualStrings("v", type_alias.type_params.items[1]);

        // Ensure the alias represents a type application (Tree with arguments)
        try testing.expect(type_alias.value.* == .type_application);

        const tree_app = type_alias.value.type_application;

        // Validate that the base type is 'Tree'
        try testing.expect(tree_app.base.* == .upper_identifier);
        try testing.expectEqualStrings("Tree", tree_app.base.upper_identifier.name);

        // Ensure 'Tree' has exactly two type arguments: (Pair k v) and (Compare k)
        try testing.expectEqual(@as(usize, 2), tree_app.args.items.len);

        // Check first argument (Pair k v)
        {
            const pair_arg = tree_app.args.items[0];

            // Ensure it's a type application (Pair applied to k and v)
            try testing.expect(pair_arg.* == .type_application);

            const pair_app = pair_arg.type_application;

            // Verify 'Pair' as the base type
            try testing.expect(pair_app.base.* == .upper_identifier);
            try testing.expectEqualStrings("Pair", pair_app.base.upper_identifier.name);

            // Ensure 'Pair' takes two arguments: 'k' and 'v'
            try testing.expectEqual(@as(usize, 2), pair_app.args.items.len);

            // Validate first argument of Pair: 'k'
            try testing.expect(pair_app.args.items[0].* == .lower_identifier);
            try testing.expectEqualStrings("k", pair_app.args.items[0].lower_identifier.name);

            // Validate second argument of Pair: 'v'
            try testing.expect(pair_app.args.items[1].* == .lower_identifier);
            try testing.expectEqualStrings("v", pair_app.args.items[1].lower_identifier.name);
        }

        // Check second argument (Compare k)
        {
            const compare_arg = tree_app.args.items[1];

            // Ensure it's a type application (Compare applied to k)
            try testing.expect(compare_arg.* == .type_application);

            const compare_app = compare_arg.type_application;

            // Verify 'Compare' as the base type
            try testing.expect(compare_app.base.* == .upper_identifier);
            try testing.expectEqualStrings("Compare", compare_app.base.upper_identifier.name);

            // Ensure 'Compare' takes exactly one argument: 'k'
            try testing.expectEqual(@as(usize, 1), compare_app.args.items.len);

            // Validate argument of Compare: 'k'
            try testing.expect(compare_app.args.items[0].* == .lower_identifier);
            try testing.expectEqualStrings("k", compare_app.args.items[0].lower_identifier.name);
        }
    }
}

test "[record_type]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "type User = { name: String, age: Int }";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the parsed node represents a record type
        try testing.expect(node.* == .record_type);

        const record = node.record_type;

        // Validate the name of the record type
        try testing.expectEqualStrings("User", record.name);

        // Ensure the record has no type parameters
        try testing.expectEqual(@as(usize, 0), record.type_params.items.len);

        // Verify the record has exactly two fields
        try testing.expectEqual(@as(usize, 2), record.fields.items.len);

        // Check first field (name)
        {
            const name_field = record.fields.items[0];

            // Ensure the field name is 'name'
            try testing.expectEqualStrings("name", name_field.name);

            // Ensure the field type is 'String'
            try testing.expect(name_field.type.* == .upper_identifier);
            try testing.expectEqualStrings("String", name_field.type.upper_identifier.name);
        }

        // Check second field (age)
        {
            const age_field = record.fields.items[1];

            // Ensure the field name is 'age'
            try testing.expectEqualStrings("age", age_field.name);

            // Ensure the field type is 'Int'
            try testing.expect(age_field.type.* == .upper_identifier);
            try testing.expectEqualStrings("Int", age_field.type.upper_identifier.name);
        }
    }

    {
        const source = "type Point a = { x: a, y: a }";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the parsed node represents a record type
        try testing.expect(node.* == .record_type);

        const record = node.record_type;

        // Validate the name of the record type
        try testing.expectEqualStrings("Point", record.name);

        // Ensure the record has exactly one type parameter
        try testing.expectEqual(@as(usize, 1), record.type_params.items.len);
        try testing.expectEqualStrings("a", record.type_params.items[0]);

        // Verify the record has exactly two fields
        try testing.expectEqual(@as(usize, 2), record.fields.items.len);

        // Check first field (x)
        {
            const x_field = record.fields.items[0];

            // Ensure the field name is 'x'
            try testing.expectEqualStrings("x", x_field.name);

            // Ensure the field type is the generic type parameter 'a'
            try testing.expect(x_field.type.* == .lower_identifier);
            try testing.expectEqualStrings("a", x_field.type.lower_identifier.name);
        }

        // Check second field (y)
        {
            const y_field = record.fields.items[1];

            // Ensure the field name is 'y'
            try testing.expectEqualStrings("y", y_field.name);

            // Ensure the field type is the generic type parameter 'a'
            try testing.expect(y_field.type.* == .lower_identifier);
            try testing.expectEqualStrings("a", y_field.type.lower_identifier.name);
        }
    }

    {
        const source = "type Dict k v = { keys: List k, values: List v }";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the parsed node represents a record type
        try testing.expect(node.* == .record_type);

        const record = node.record_type;

        // Validate the name of the record type
        try testing.expectEqualStrings("Dict", record.name);

        // Ensure the record has exactly two type parameters
        try testing.expectEqual(@as(usize, 2), record.type_params.items.len);
        try testing.expectEqualStrings("k", record.type_params.items[0]);
        try testing.expectEqualStrings("v", record.type_params.items[1]);

        // Verify the record has exactly two fields
        try testing.expectEqual(@as(usize, 2), record.fields.items.len);

        // Check first field (keys)
        {
            const keys_field = record.fields.items[0];

            // Ensure the field name is 'keys'
            try testing.expectEqualStrings("keys", keys_field.name);

            // Ensure the field type is a type application (List k)
            try testing.expect(keys_field.type.* == .type_application);

            const app = keys_field.type.type_application;

            // Ensure the base type is 'List'
            try testing.expect(app.base.* == .upper_identifier);
            try testing.expectEqualStrings("List", app.base.upper_identifier.name);

            // Ensure 'List' has exactly one type argument: 'k'
            try testing.expectEqual(@as(usize, 1), app.args.items.len);
            try testing.expect(app.args.items[0].* == .lower_identifier);
            try testing.expectEqualStrings("k", app.args.items[0].lower_identifier.name);
        }

        // Check second field (values)
        {
            const values_field = record.fields.items[1];

            // Ensure the field name is 'values'
            try testing.expectEqualStrings("values", values_field.name);

            // Ensure the field type is a type application (List v)
            try testing.expect(values_field.type.* == .type_application);

            const app = values_field.type.type_application;

            // Ensure the base type is 'List'
            try testing.expect(app.base.* == .upper_identifier);
            try testing.expectEqualStrings("List", app.base.upper_identifier.name);

            // Ensure 'List' has exactly one type argument: 'v'
            try testing.expectEqual(@as(usize, 1), app.args.items.len);
            try testing.expect(app.args.items[0].* == .lower_identifier);
            try testing.expectEqualStrings("v", app.args.items[0].lower_identifier.name);
        }
    }

    {
        const source = "type Storage a = { items: List (Maybe a), count: Maybe Int, names: List String }";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the parsed node is a record type
        try testing.expect(node.* == .record_type);

        const record = node.record_type;

        // Validate the name of the record type
        try testing.expectEqualStrings("Storage", record.name);

        // Ensure 'Storage' has one type parameter: 'a'
        try testing.expectEqual(@as(usize, 1), record.type_params.items.len);
        try testing.expectEqualStrings("a", record.type_params.items[0]);

        // Ensure the record has exactly three fields
        try testing.expectEqual(@as(usize, 3), record.fields.items.len);

        // Check first field (items: List (Maybe a))
        {
            const items_field = record.fields.items[0];

            // Ensure the field name is 'items'
            try testing.expectEqualStrings("items", items_field.name);

            // Ensure the field type is a type application (List (Maybe a))
            try testing.expect(items_field.type.* == .type_application);

            const list_app = items_field.type.type_application;

            // Ensure the base type is 'List'
            try testing.expect(list_app.base.* == .upper_identifier);
            try testing.expectEqualStrings("List", list_app.base.upper_identifier.name);

            // Ensure List has exactly one type argument
            try testing.expectEqual(@as(usize, 1), list_app.args.items.len);

            // Ensure the argument is another type application (Maybe a)
            try testing.expect(list_app.args.items[0].* == .type_application);

            const maybe_app = list_app.args.items[0].type_application;

            // Ensure the base type is 'Maybe'
            try testing.expect(maybe_app.base.* == .upper_identifier);
            try testing.expectEqualStrings("Maybe", maybe_app.base.upper_identifier.name);

            // Ensure Maybe has one type argument: 'a'
            try testing.expectEqual(@as(usize, 1), maybe_app.args.items.len);
            try testing.expect(maybe_app.args.items[0].* == .lower_identifier);
            try testing.expectEqualStrings("a", maybe_app.args.items[0].lower_identifier.name);
        }

        // Check second field (count: Maybe Int)
        {
            const count_field = record.fields.items[1];

            // Ensure the field name is 'count'
            try testing.expectEqualStrings("count", count_field.name);

            // Ensure the field type is a type application (Maybe Int)
            try testing.expect(count_field.type.* == .type_application);

            const maybe_app = count_field.type.type_application;

            // Ensure the base type is 'Maybe'
            try testing.expect(maybe_app.base.* == .upper_identifier);
            try testing.expectEqualStrings("Maybe", maybe_app.base.upper_identifier.name);

            // Ensure Maybe has exactly one type argument: 'Int'
            try testing.expectEqual(@as(usize, 1), maybe_app.args.items.len);
            try testing.expect(maybe_app.args.items[0].* == .upper_identifier);
            try testing.expectEqualStrings("Int", maybe_app.args.items[0].upper_identifier.name);
        }

        // Check third field (names: List String)
        {
            const names_field = record.fields.items[2];

            // Ensure the field name is 'names'
            try testing.expectEqualStrings("names", names_field.name);

            // Ensure the field type is a type application (List String)
            try testing.expect(names_field.type.* == .type_application);

            const list_app = names_field.type.type_application;

            // Ensure the base type is 'List'
            try testing.expect(list_app.base.* == .upper_identifier);
            try testing.expectEqualStrings("List", list_app.base.upper_identifier.name);

            // Ensure List has exactly one type argument: 'String'
            try testing.expectEqual(@as(usize, 1), list_app.args.items.len);
            try testing.expect(list_app.args.items[0].* == .upper_identifier);
            try testing.expectEqualStrings("String", list_app.args.items[0].upper_identifier.name);
        }
    }
}

test "[variant_type]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        const source = "type Boolean = | True | False";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the parsed node is a variant type
        try testing.expect(node.* == .variant_type);

        const variant = node.variant_type;

        // Validate the name of the variant type
        try testing.expectEqualStrings("Boolean", variant.name);

        // Ensure 'Boolean' has no type parameters
        try testing.expectEqual(@as(usize, 0), variant.type_params.items.len);

        // Ensure the variant type has exactly two constructors (True, False)
        try testing.expectEqual(@as(usize, 2), variant.constructors.items.len);

        // Check first constructor (True)
        {
            const true_constructor = variant.constructors.items[0];

            // Ensure the constructor is named 'True'
            try testing.expectEqualStrings("True", true_constructor.name);

            // Ensure 'True' has no associated parameters
            try testing.expectEqual(@as(usize, 0), true_constructor.params.items.len);
        }

        // Check second constructor (False)
        {
            const false_constructor = variant.constructors.items[1];

            // Ensure the constructor is named 'False'
            try testing.expectEqualStrings("False", false_constructor.name);

            // Ensure 'False' has no associated parameters
            try testing.expectEqual(@as(usize, 0), false_constructor.params.items.len);
        }
    }

    {
        const source = "type Maybe a = | None | Some a";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the parsed node is a variant type
        try testing.expect(node.* == .variant_type);

        const variant = node.variant_type;

        // Validate the name of the variant type
        try testing.expectEqualStrings("Maybe", variant.name);

        // Ensure 'Maybe' has a single type parameter 'a'
        try testing.expectEqual(@as(usize, 1), variant.type_params.items.len);
        try testing.expectEqualStrings("a", variant.type_params.items[0]);

        // Ensure the variant type has exactly two constructors (None, Some)
        try testing.expectEqual(@as(usize, 2), variant.constructors.items.len);

        // Check first constructor (None)
        {
            const none_constructor = variant.constructors.items[0];

            // Ensure the constructor is named 'None'
            try testing.expectEqualStrings("None", none_constructor.name);

            // Ensure 'None' has no associated parameters
            try testing.expectEqual(@as(usize, 0), none_constructor.params.items.len);
        }

        // Check second constructor (Some a)
        {
            const some_constructor = variant.constructors.items[1];

            // Ensure the constructor is named 'Some'
            try testing.expectEqualStrings("Some", some_constructor.name);

            // Ensure 'Some' has exactly one associated parameter
            try testing.expectEqual(@as(usize, 1), some_constructor.params.items.len);

            const param = some_constructor.params.items[0];

            // Validate that the parameter is the type variable 'a'
            try testing.expect(param.* == .lower_identifier);
            try testing.expectEqualStrings("a", param.lower_identifier.name);
        }
    }

    {
        const source = "type Tree a = | Leaf | Branch (Tree a) a (Tree a)";
        var l = lexer.Lexer.init(source, TEST_FILE);
        var parser = try Parser.init(allocator, &l);
        defer parser.deinit();

        // Action
        const node = try parser.parseTypeDecl();
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        // Assertions
        // Ensure the parsed node is a variant type
        try testing.expect(node.* == .variant_type);

        const variant = node.variant_type;

        // Validate the name of the variant type
        try testing.expectEqualStrings("Tree", variant.name);

        // Ensure 'Tree' has a single type parameter 'a'
        try testing.expectEqual(@as(usize, 1), variant.type_params.items.len);
        try testing.expectEqualStrings("a", variant.type_params.items[0]);

        // Ensure the variant type has exactly two constructors (Leaf, Branch)
        try testing.expectEqual(@as(usize, 2), variant.constructors.items.len);

        // Check first constructor (Leaf)
        {
            const leaf_constructor = variant.constructors.items[0];

            // Ensure the constructor is named 'Leaf'
            try testing.expectEqualStrings("Leaf", leaf_constructor.name);

            // Ensure 'Leaf' has no associated parameters
            try testing.expectEqual(@as(usize, 0), leaf_constructor.params.items.len);
        }

        // Check second constructor (Branch (Tree a) a (Tree a))
        {
            const branch_constructor = variant.constructors.items[1];

            // Ensure the constructor is named 'Branch'
            try testing.expectEqualStrings("Branch", branch_constructor.name);

            // Ensure 'Branch' has exactly three parameters
            try testing.expectEqual(@as(usize, 3), branch_constructor.params.items.len);

            // First parameter (Tree a)
            {
                const param1 = branch_constructor.params.items[0];

                // Ensure 'param1' is a type application
                try testing.expect(param1.* == .type_application);

                const app = param1.type_application;

                // Ensure the base type is 'Tree'
                try testing.expect(app.base.* == .upper_identifier);
                try testing.expectEqualStrings("Tree", app.base.upper_identifier.name);

                // Ensure 'Tree' is applied to one argument ('a')
                try testing.expectEqual(@as(usize, 1), app.args.items.len);
                try testing.expect(app.args.items[0].* == .lower_identifier);
                try testing.expectEqualStrings("a", app.args.items[0].lower_identifier.name);
            }

            // Second parameter (a)
            {
                const param2 = branch_constructor.params.items[1];

                // Ensure 'param2' is the type variable 'a'
                try testing.expect(param2.* == .lower_identifier);
                try testing.expectEqualStrings("a", param2.lower_identifier.name);
            }

            // Third parameter (Tree a)
            {
                const param3 = branch_constructor.params.items[2];

                // Ensure 'param3' is a type application
                try testing.expect(param3.* == .type_application);

                const app = param3.type_application;

                // Ensure the base type is 'Tree'
                try testing.expect(app.base.* == .upper_identifier);
                try testing.expectEqualStrings("Tree", app.base.upper_identifier.name);

                // Ensure 'Tree' is applied to one argument ('a')
                try testing.expectEqual(@as(usize, 1), app.args.items.len);
                try testing.expect(app.args.items[0].* == .lower_identifier);
                try testing.expectEqualStrings("a", app.args.items[0].lower_identifier.name);
            }
        }
    }
}
