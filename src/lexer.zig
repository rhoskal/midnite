const std = @import("std");

pub const TokenKind = enum {
    // Comments
    Comment,
    DocComment,

    // Keywords
    KwAlias,
    KwAs,
    KwElse,
    KwEnd,
    KwExposing,
    KwForeign,
    KwHiding,
    KwIf,
    KwIn,
    KwInclude,
    KwInfixLeft,
    KwInfixNon,
    KwInfixRight,
    KwLet,
    KwMatch,
    KwModule,
    KwOn,
    KwOpen,
    KwRenaming,
    KwThen,
    KwTo,
    KwType,
    KwUsing,
    KwWhen,

    // Delimiters
    DelColon,
    DelComma,
    DelDot,
    DelLBrack,
    DelLCurly,
    DelLParen,
    DelRBrack,
    DelRCurly,
    DelRParen,

    // Symbols
    SymArrowRight,
    SymDollarSign,
    SymDoubleArrowRight,
    SymPipe,
    SymUnderscore,

    // Operators
    OpAppend,
    OpAssign,
    OpCons,
    OpLambda,
    OpDoubleDot,

    // Arithmetic Operators
    OpExp,
    OpFloatAdd,
    OpFloatDiv,
    OpFloatMul,
    OpFloatSub,
    OpIntAdd,
    OpIntDiv,
    OpIntMul,
    OpIntSub,

    // Comparison (Relational) Operators
    OpEquality,
    OpGreaterThan,
    OpGreaterThanEqual,
    OpLessThan,
    OpLessThanEqual,
    OpNotEqual,

    // Logical (Boolean) Operators
    OpLogicalAnd,
    OpLogicalOr,

    // Literals
    // LitChar,
    // LitFloat,
    LitInt,
    LitString,
    LitMultilineString,

    // Identifiers
    LowerIdent,
    UpperIdent,

    // Special
    Eof,
    Invalid,
    TypedHole,
};

/// A structure representing a token with a specific kind, lexeme, and position in the source code.
pub const Token = struct {
    kind: TokenKind,
    lexeme: []const u8,
    line: usize,
    column: usize,

    /// Initializes a new token with the given properties.
    ///
    /// - `kind`: The kind of token (e.g., literal, keyword).
    /// - `lexeme`: The string representation of the token.
    /// - `line`: The line number where the token is found.
    /// - `column`: The column number where the token is found.
    pub fn init(kind: TokenKind, lexeme: []const u8, line: usize, column: usize) Token {
        return Token{
            .kind = kind,
            .lexeme = lexeme,
            .line = line,
            .column = column,
        };
    }
};

pub const LexerError = error{
    InvalidCharacter,
    InvalidIdentifier,
    InvalidInteger,
    UnterminatedString,
};

/// A structure representing a lexer which processes source code and generates tokens.
/// It keeps track of the position in the source code, the current line and column, and performs
/// operations to extract tokens from the source.
pub const Lexer = struct {
    source: []const u8,
    position: usize,
    line: usize,
    column: usize,

    /// Initializes a new lexer with the given source code.
    ///
    /// - `source`: The source code to be lexed.
    pub fn init(source: []const u8) Lexer {
        return Lexer{
            .source = source,
            .position = 0,
            .line = 1,
            .column = 1,
        };
    }

    /// Peeks at the next character in the source code without advancing the position.
    ///
    /// Returns `null` if the end of the source is reached.
    fn peek(self: *Lexer) ?u8 {
        if (self.position >= self.source.len) return null;

        return self.source[self.position];
    }

    /// Advances the lexer by one position in the source code.
    /// Updates the current line and column numbers accordingly.
    fn advance(self: *Lexer) void {
        if (self.position >= self.source.len) return;

        if (self.source[self.position] == '\n') {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        self.position += 1;
    }

    fn skipWhitespace(self: *Lexer) void {
        while (self.peek()) |c| {
            switch (c) {
                ' ', '\t', '\r', '\n' => self.advance(),
                else => break,
            }
        }
    }

    /// Checks if there is an exact match for a keyword starting at a given position.
    ///
    /// - `start`: The starting position of the match in the source code.
    /// - `keyword`: The keyword to check for.
    /// - `kind`: The token kind that corresponds to the keyword.
    ///
    /// Returns a token if there is a match, or `null` if there is no match.
    fn checkExactMatch(self: *Lexer, start: usize, keyword: []const u8, kind: TokenKind) ?Token {
        const len = keyword.len;

        if (self.position - start == len and
            std.mem.eql(u8, self.source[start..self.position], keyword))
        {
            return Token.init(kind, self.source[start..self.position], self.line, self.column - len);
        }

        return null;
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isHexDigit(c: u8) bool {
        return isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
    }

    fn isBinDigit(c: u8) bool {
        return c == '0' or c == '1';
    }

    fn isOctDigit(c: u8) bool {
        return c >= '0' and c <= '7';
    }

    fn lexInteger(self: *Lexer, base: enum { Decimal, Hex, Octal, Binary }) !Token {
        const offset = if (base == .Decimal) @as(usize, 0) else 2;
        const start = self.position - offset;
        const start_line = self.line;
        const start_column = self.column - offset;

        // For other bases, prefix (0x, 0b, 0o) was already consumed
        if (base == .Decimal) {
            self.advance();
        }

        if (base != .Decimal) {
            if (self.peek()) |next| {
                if (next == '_') {
                    return error.InvalidInteger;
                }
            }
        }

        var found_valid_digit = true;
        var last_was_underscore = false;

        while (self.peek()) |c| {
            const is_valid_digit = switch (base) {
                .Decimal => isDigit(c),
                .Hex => isHexDigit(c),
                .Binary => isBinDigit(c),
                .Octal => isOctDigit(c),
            };

            if (is_valid_digit) {
                found_valid_digit = true;
                last_was_underscore = false;
                self.advance();
            } else if (c == '_') {
                if (!found_valid_digit or last_was_underscore) {
                    return error.InvalidInteger;
                }

                last_was_underscore = true;
                self.advance();
            } else {
                break;
            }
        }

        if (last_was_underscore) {
            return error.InvalidInteger;
        }

        return Token.init(
            TokenKind.LitInt,
            self.source[start..self.position],
            start_line,
            start_column,
        );
    }

    /// Retrieves the next token from the source code, advancing the lexer.
    ///
    /// - Returns: A token object corresponding to the next recognized token.
    pub fn nextToken(self: *Lexer) !Token {
        self.skipWhitespace();

        const start = self.position;
        const start_line = self.line;
        const start_column = self.column;

        const c = self.peek() orelse {
            return Token.init(
                TokenKind.Eof,
                "",
                start_line,
                start_column,
            );
        };

        switch (c) {
            '?' => {
                self.advance();

                return Token.init(
                    TokenKind.TypedHole,
                    "?",
                    start_line,
                    start_column,
                );
            },
            '#' => {
                const mark = self.position;
                self.advance();

                var is_doc = false;
                if (self.peek()) |next| {
                    if (next == '#') {
                        self.advance();
                        is_doc = true;
                    }
                }

                while (self.peek()) |next| {
                    if (next == '\n') break;

                    const utf8_len = std.unicode.utf8ByteSequenceLength(next) catch {
                        // Invalid UTF-8 sequence, treat as single byte
                        self.advance();
                        continue;
                    };

                    var i: usize = 0;
                    while (i < utf8_len) : (i += 1) {
                        self.advance();
                    }
                }

                const kind = if (is_doc) TokenKind.DocComment else TokenKind.Comment;

                return Token.init(
                    kind,
                    self.source[mark..self.position],
                    start_line,
                    start_column,
                );
            },
            '"' => {
                const mark = self.position;
                self.advance();

                if (self.peek()) |next1| {
                    if (next1 == '"') {
                        self.advance();

                        if (self.peek()) |next2| {
                            if (next2 == '"') {
                                self.advance();

                                var found_end = false;
                                while (self.peek()) |next| {
                                    if (next == '"') {
                                        self.advance();

                                        if (self.peek()) |quote2| {
                                            if (quote2 == '"') {
                                                self.advance();

                                                if (self.peek()) |quote3| {
                                                    if (quote3 == '"') {
                                                        self.advance();
                                                        found_end = true;
                                                        break;
                                                    }
                                                }
                                            }
                                        }

                                        if (!found_end) {
                                            const utf8_len = std.unicode.utf8ByteSequenceLength(next) catch {
                                                continue;
                                            };

                                            var i: usize = 1;
                                            while (i < utf8_len) : (i += 1) {
                                                self.advance();
                                            }
                                        }
                                    } else {
                                        const utf8_len = std.unicode.utf8ByteSequenceLength(next) catch {
                                            self.advance();
                                            continue;
                                        };

                                        var i: usize = 0;
                                        while (i < utf8_len) : (i += 1) {
                                            self.advance();
                                        }
                                    }
                                }

                                if (!found_end) {
                                    return LexerError.UnterminatedString;
                                }

                                return Token.init(
                                    TokenKind.LitMultilineString,
                                    self.source[mark..self.position],
                                    start_line,
                                    start_column,
                                );
                            }
                        }
                    }
                }

                // Not a multiline string, handle as invalid for now
                // (add support for regular strings here)
                self.advance();

                return Token.init(
                    TokenKind.Invalid,
                    self.source[mark..self.position],
                    start_line,
                    start_column,
                );
            },
            '+' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpFloatAdd,
                            "+.",
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.OpIntAdd,
                    "+",
                    start_line,
                    start_column,
                );
            },
            '-' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '>') {
                        self.advance();

                        return Token.init(
                            TokenKind.SymArrowRight,
                            "->",
                            start_line,
                            start_column,
                        );
                    }

                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpFloatSub,
                            "-.",
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.OpIntSub,
                    "-",
                    start_line,
                    start_column,
                );
            },
            '*' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '*') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpExp,
                            "**",
                            start_line,
                            start_column,
                        );
                    }

                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpFloatMul,
                            "*.",
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.OpIntMul,
                    "*",
                    start_line,
                    start_column,
                );
            },
            '/' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpNotEqual,
                            "/=",
                            start_line,
                            start_column,
                        );
                    }

                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpFloatDiv,
                            "/.",
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.OpIntDiv,
                    "/",
                    start_line,
                    start_column,
                );
            },
            '<' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpLessThanEqual,
                            "<=",
                            start_line,
                            start_column,
                        );
                    }

                    if (next == '>') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpAppend,
                            "<>",
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.OpLessThan,
                    "<",
                    start_line,
                    start_column,
                );
            },
            '>' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpGreaterThanEqual,
                            ">=",
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.OpGreaterThan,
                    ">",
                    start_line,
                    start_column,
                );
            },
            '&' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '&') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpLogicalAnd,
                            "&&",
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.Invalid,
                    "&",
                    start_line,
                    start_column,
                );
            },
            '|' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '|') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpLogicalOr,
                            "||",
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.SymPipe,
                    "|",
                    start_line,
                    start_column,
                );
            },
            '\\' => {
                self.advance();

                return Token.init(
                    TokenKind.OpLambda,
                    "\\",
                    start_line,
                    start_column,
                );
            },
            ':' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == ':') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpCons,
                            "::",
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.DelColon,
                    ":",
                    start_line,
                    start_column,
                );
            },
            ',' => {
                self.advance();

                return Token.init(
                    TokenKind.DelComma,
                    ",",
                    start_line,
                    start_column,
                );
            },
            '$' => {
                self.advance();

                return Token.init(
                    TokenKind.SymDollarSign,
                    "$",
                    start_line,
                    start_column,
                );
            },
            '.' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpDoubleDot,
                            "..",
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.DelDot,
                    ".",
                    start_line,
                    start_column,
                );
            },
            '{' => {
                self.advance();

                return Token.init(
                    TokenKind.DelLCurly,
                    "{",
                    start_line,
                    start_column,
                );
            },
            '}' => {
                self.advance();

                return Token.init(
                    TokenKind.DelRCurly,
                    "}",
                    start_line,
                    start_column,
                );
            },
            '=' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '>') {
                        self.advance();

                        return Token.init(
                            TokenKind.SymDoubleArrowRight,
                            "=>",
                            start_line,
                            start_column,
                        );
                    }

                    if (next == '=') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpEquality,
                            "==",
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.OpAssign,
                    "=",
                    start_line,
                    start_column,
                );
            },
            '(' => {
                self.advance();

                return Token.init(
                    TokenKind.DelLParen,
                    "(",
                    start_line,
                    start_column,
                );
            },
            ')' => {
                self.advance();

                return Token.init(
                    TokenKind.DelRParen,
                    ")",
                    start_line,
                    start_column,
                );
            },
            '[' => {
                self.advance();

                return Token.init(
                    TokenKind.DelLBrack,
                    "[",
                    start_line,
                    start_column,
                );
            },
            ']' => {
                self.advance();

                return Token.init(
                    TokenKind.DelRBrack,
                    "]",
                    start_line,
                    start_column,
                );
            },
            '0' => {
                self.advance();

                if (self.peek()) |next| {
                    switch (next) {
                        'b' => {
                            self.advance();

                            return self.lexInteger(.Binary);
                        },
                        'o' => {
                            self.advance();

                            return self.lexInteger(.Octal);
                        },
                        'x' => {
                            self.advance();

                            return self.lexInteger(.Hex);
                        },
                        else => return self.lexInteger(.Decimal),
                    }
                }

                return Token.init(
                    TokenKind.LitInt,
                    self.source[start..self.position],
                    start_line,
                    start_column,
                );
            },
            '1'...'9' => {
                return self.lexInteger(.Decimal);
            },
            'a'...'z', 'A'...'Z', '_' => {
                if (c == '_') {
                    self.advance();

                    if (self.peek() == null) {
                        return Token.init(
                            TokenKind.SymUnderscore,
                            self.source[start..self.position],
                            start_line,
                            start_column,
                        );
                    }

                    if (self.peek()) |next| {
                        if (next >= 'A' and next <= 'Z') {
                            return error.InvalidIdentifier;
                        }

                        if (isDigit(next)) {
                            return error.InvalidInteger;
                        }

                        switch (next) {
                            'a'...'z', '_' => {
                                while (self.peek()) |next_char| {
                                    switch (next_char) {
                                        'a'...'z', 'A'...'Z', '_', '0'...'9' => self.advance(),
                                        else => break,
                                    }
                                }

                                return Token.init(
                                    TokenKind.LowerIdent,
                                    self.source[start..self.position],
                                    start_line,
                                    start_column,
                                );
                            },
                            else => {
                                return Token.init(
                                    TokenKind.SymUnderscore,
                                    "_",
                                    start_line,
                                    start_column,
                                );
                            },
                        }
                    }

                    return Token.init(
                        TokenKind.SymUnderscore,
                        "_",
                        start_line,
                        start_column,
                    );
                }

                while (self.peek()) |next| {
                    switch (next) {
                        'a'...'z', 'A'...'Z', '_', '0'...'9' => self.advance(),
                        else => break,
                    }
                }

                if (self.checkExactMatch(start, "alias", TokenKind.KwAlias)) |token| return token;
                if (self.checkExactMatch(start, "as", TokenKind.KwAs)) |token| return token;
                if (self.checkExactMatch(start, "else", TokenKind.KwElse)) |token| return token;
                if (self.checkExactMatch(start, "end", TokenKind.KwEnd)) |token| return token;
                if (self.checkExactMatch(start, "exposing", TokenKind.KwExposing)) |token| return token;
                if (self.checkExactMatch(start, "foreign", TokenKind.KwForeign)) |token| return token;
                if (self.checkExactMatch(start, "hiding", TokenKind.KwHiding)) |token| return token;
                if (self.checkExactMatch(start, "if", TokenKind.KwIf)) |token| return token;
                if (self.checkExactMatch(start, "in", TokenKind.KwIn)) |token| return token;
                if (self.checkExactMatch(start, "include", TokenKind.KwInclude)) |token| return token;
                if (self.checkExactMatch(start, "infixl", TokenKind.KwInfixLeft)) |token| return token;
                if (self.checkExactMatch(start, "infixn", TokenKind.KwInfixNon)) |token| return token;
                if (self.checkExactMatch(start, "infixr", TokenKind.KwInfixRight)) |token| return token;
                if (self.checkExactMatch(start, "let", TokenKind.KwLet)) |token| return token;
                if (self.checkExactMatch(start, "match", TokenKind.KwMatch)) |token| return token;
                if (self.checkExactMatch(start, "module", TokenKind.KwModule)) |token| return token;
                if (self.checkExactMatch(start, "on", TokenKind.KwOn)) |token| return token;
                if (self.checkExactMatch(start, "open", TokenKind.KwOpen)) |token| return token;
                if (self.checkExactMatch(start, "renaming", TokenKind.KwRenaming)) |token| return token;
                if (self.checkExactMatch(start, "then", TokenKind.KwThen)) |token| return token;
                if (self.checkExactMatch(start, "to", TokenKind.KwTo)) |token| return token;
                if (self.checkExactMatch(start, "type", TokenKind.KwType)) |token| return token;
                if (self.checkExactMatch(start, "using", TokenKind.KwUsing)) |token| return token;
                if (self.checkExactMatch(start, "when", TokenKind.KwWhen)) |token| return token;

                const first_char = self.source[start];

                switch (first_char) {
                    'A'...'Z' => return Token.init(
                        TokenKind.UpperIdent,
                        self.source[start..self.position],
                        start_line,
                        start_column,
                    ),
                    'a'...'z', '_' => return Token.init(
                        TokenKind.LowerIdent,
                        self.source[start..self.position],
                        start_line,
                        start_column,
                    ),
                    else => return Token.init(
                        TokenKind.Invalid,
                        self.source[start..self.position],
                        start_line,
                        start_column,
                    ),
                }
            },
            else => {
                self.advance();

                return Token.init(
                    TokenKind.Invalid,
                    self.source[start..self.position],
                    start_line,
                    start_column,
                );
            },
        }
    }
};

const TestCase = struct {
    source: []const u8,
    kind: TokenKind,
    lexeme: []const u8,
};

test "keywords" {
    const cases = [_]TestCase{
        .{ .source = "alias", .kind = .KwAlias, .lexeme = "alias" },
        .{ .source = "as", .kind = .KwAs, .lexeme = "as" },
        .{ .source = "else", .kind = .KwElse, .lexeme = "else" },
        .{ .source = "end", .kind = .KwEnd, .lexeme = "end" },
        .{ .source = "exposing", .kind = .KwExposing, .lexeme = "exposing" },
        .{ .source = "foreign", .kind = .KwForeign, .lexeme = "foreign" },
        .{ .source = "hiding", .kind = .KwHiding, .lexeme = "hiding" },
        .{ .source = "if", .kind = .KwIf, .lexeme = "if" },
        .{ .source = "include", .kind = .KwInclude, .lexeme = "include" },
        .{ .source = "infixl", .kind = .KwInfixLeft, .lexeme = "infixl" },
        .{ .source = "infixn", .kind = .KwInfixNon, .lexeme = "infixn" },
        .{ .source = "infixr", .kind = .KwInfixRight, .lexeme = "infixr" },
        .{ .source = "let", .kind = .KwLet, .lexeme = "let" },
        .{ .source = "match", .kind = .KwMatch, .lexeme = "match" },
        .{ .source = "module", .kind = .KwModule, .lexeme = "module" },
        .{ .source = "on", .kind = .KwOn, .lexeme = "on" },
        .{ .source = "open", .kind = .KwOpen, .lexeme = "open" },
        .{ .source = "renaming", .kind = .KwRenaming, .lexeme = "renaming" },
        .{ .source = "then", .kind = .KwThen, .lexeme = "then" },
        .{ .source = "to", .kind = .KwTo, .lexeme = "to" },
        .{ .source = "type", .kind = .KwType, .lexeme = "type" },
        .{ .source = "using", .kind = .KwUsing, .lexeme = "using" },
        .{ .source = "when", .kind = .KwWhen, .lexeme = "when" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source);

        const token = try lexer.nextToken();
        try std.testing.expectEqual(case.kind, token.kind);
        try std.testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try std.testing.expectEqual(TokenKind.Eof, eof.kind);
    }
}

test "delimiters" {
    const cases = [_]TestCase{
        .{ .source = ":", .kind = .DelColon, .lexeme = ":" },
        .{ .source = ",", .kind = .DelComma, .lexeme = "," },
        .{ .source = ".", .kind = .DelDot, .lexeme = "." },
        .{ .source = "[", .kind = .DelLBrack, .lexeme = "[" },
        .{ .source = "{", .kind = .DelLCurly, .lexeme = "{" },
        .{ .source = "(", .kind = .DelLParen, .lexeme = "(" },
        .{ .source = "]", .kind = .DelRBrack, .lexeme = "]" },
        .{ .source = "}", .kind = .DelRCurly, .lexeme = "}" },
        .{ .source = ")", .kind = .DelRParen, .lexeme = ")" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source);

        const token = try lexer.nextToken();
        try std.testing.expectEqual(case.kind, token.kind);
        try std.testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try std.testing.expectEqual(TokenKind.Eof, eof.kind);
    }
}

test "symbols" {
    const cases = [_]TestCase{
        .{ .source = "->", .kind = .SymArrowRight, .lexeme = "->" },
        .{ .source = "$", .kind = .SymDollarSign, .lexeme = "$" },
        .{ .source = "=>", .kind = .SymDoubleArrowRight, .lexeme = "=>" },
        .{ .source = "|", .kind = .SymPipe, .lexeme = "|" },
        .{ .source = "_", .kind = .SymUnderscore, .lexeme = "_" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source);

        const token = try lexer.nextToken();
        try std.testing.expectEqual(case.kind, token.kind);
        try std.testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try std.testing.expectEqual(TokenKind.Eof, eof.kind);
    }
}

test "operators" {
    const cases = [_]TestCase{
        .{ .source = "<>", .kind = .OpAppend, .lexeme = "<>" },
        .{ .source = "=", .kind = .OpAssign, .lexeme = "=" },
        .{ .source = "::", .kind = .OpCons, .lexeme = "::" },
        .{ .source = "\\", .kind = .OpLambda, .lexeme = "\\" },
        .{ .source = "..", .kind = .OpDoubleDot, .lexeme = ".." },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source);

        const token = try lexer.nextToken();
        try std.testing.expectEqual(case.kind, token.kind);
        try std.testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try std.testing.expectEqual(TokenKind.Eof, eof.kind);
    }
}

test "arithmetic operators" {
    const cases = [_]TestCase{
        .{ .source = "**", .kind = .OpExp, .lexeme = "**" },
        .{ .source = "+.", .kind = .OpFloatAdd, .lexeme = "+." },
        .{ .source = "/.", .kind = .OpFloatDiv, .lexeme = "/." },
        .{ .source = "*.", .kind = .OpFloatMul, .lexeme = "*." },
        .{ .source = "-.", .kind = .OpFloatSub, .lexeme = "-." },
        .{ .source = "+", .kind = .OpIntAdd, .lexeme = "+" },
        .{ .source = "/", .kind = .OpIntDiv, .lexeme = "/" },
        .{ .source = "*", .kind = .OpIntMul, .lexeme = "*" },
        .{ .source = "-", .kind = .OpIntSub, .lexeme = "-" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source);

        const token = try lexer.nextToken();
        try std.testing.expectEqual(case.kind, token.kind);
        try std.testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try std.testing.expectEqual(TokenKind.Eof, eof.kind);
    }
}

test "comparison (relational) operators" {
    const cases = [_]TestCase{
        .{ .source = "==", .kind = .OpEquality, .lexeme = "==" },
        .{ .source = ">", .kind = .OpGreaterThan, .lexeme = ">" },
        .{ .source = ">=", .kind = .OpGreaterThanEqual, .lexeme = ">=" },
        .{ .source = "<", .kind = .OpLessThan, .lexeme = "<" },
        .{ .source = "<=", .kind = .OpLessThanEqual, .lexeme = "<=" },
        .{ .source = "/=", .kind = .OpNotEqual, .lexeme = "/=" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source);

        const token = try lexer.nextToken();
        try std.testing.expectEqual(case.kind, token.kind);
        try std.testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try std.testing.expectEqual(TokenKind.Eof, eof.kind);
    }
}

test "logical (boolean) operators" {
    const cases = [_]TestCase{
        .{ .source = "&&", .kind = .OpLogicalAnd, .lexeme = "&&" },
        .{ .source = "||", .kind = .OpLogicalOr, .lexeme = "||" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source);

        const token = try lexer.nextToken();
        try std.testing.expectEqual(case.kind, token.kind);
        try std.testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try std.testing.expectEqual(TokenKind.Eof, eof.kind);
    }
}

test "special" {
    const cases = [_]TestCase{
        .{ .source = "?", .kind = .TypedHole, .lexeme = "?" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source);

        const token = try lexer.nextToken();
        try std.testing.expectEqual(case.kind, token.kind);
        try std.testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try std.testing.expectEqual(TokenKind.Eof, eof.kind);
    }
}

test "comments" {
    const cases = [_]TestCase{
        .{ .source = "# this is a comment", .kind = .Comment, .lexeme = "# this is a comment" },
        .{ .source = "## this is a doc comment", .kind = .DocComment, .lexeme = "## this is a doc comment" },
        .{ .source = "# 你好，世界", .kind = .Comment, .lexeme = "# 你好，世界" },
        .{ .source = "## こんにちは", .kind = .DocComment, .lexeme = "## こんにちは" },
        .{ .source = "# 🚀 👽 💣", .kind = .Comment, .lexeme = "# 🚀 👽 💣" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source);

        const token = try lexer.nextToken();
        try std.testing.expectEqual(case.kind, token.kind);
        try std.testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try std.testing.expectEqual(TokenKind.Eof, eof.kind);
    }
}

test "multiline string" {
    const cases = [_]TestCase{
        .{ .source = 
        \\""" This is a
        \\multiline string with
        \\unicode: 你好, こんにちは
        \\"""
        , .kind = .LitMultilineString, .lexeme = 
        \\""" This is a
        \\multiline string with
        \\unicode: 你好, こんにちは
        \\"""
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source);

        const token = try lexer.nextToken();
        try std.testing.expectEqual(case.kind, token.kind);
        try std.testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try std.testing.expectEqual(TokenKind.Eof, eof.kind);
    }
}

test "unterminated multiline string" {
    const invalid_cases = [_][]const u8{
        \\""" This is an
        \\unterminated multiline string with
        \\unicode: 你好, こんにちは
        \\
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source);

        const result = lexer.nextToken();
        try std.testing.expectError(error.UnterminatedString, result);
    }
}

test "integer literals" {
    const cases = [_]TestCase{
        .{ .source = "42", .kind = .LitInt, .lexeme = "42" },
        .{ .source = "42_000_000", .kind = .LitInt, .lexeme = "42_000_000" },
        .{ .source = "0b101010", .kind = .LitInt, .lexeme = "0b101010" },
        .{ .source = "0b10_1010", .kind = .LitInt, .lexeme = "0b10_1010" },
        .{ .source = "0o52", .kind = .LitInt, .lexeme = "0o52" },
        .{ .source = "0o52_52", .kind = .LitInt, .lexeme = "0o52_52" },
        .{ .source = "0x2A", .kind = .LitInt, .lexeme = "0x2A" },
        .{ .source = "0x2A_2A", .kind = .LitInt, .lexeme = "0x2A_2A" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source);

        const token = try lexer.nextToken();
        try std.testing.expectEqual(case.kind, token.kind);
        try std.testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try std.testing.expectEqual(TokenKind.Eof, eof.kind);
    }
}

test "invalid integer literals" {
    const invalid_cases = [_][]const u8{
        "10__000", // consecutive underscores
        "_1000", // leading underscore
        "1000_", // trailing underscore
        "0x_1F", // underscore after prefix
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source);

        const result = lexer.nextToken();
        try std.testing.expectError(error.InvalidInteger, result);
    }
}

test "identifiers" {
    const cases = [_]TestCase{
        .{ .source = "Int", .kind = .UpperIdent, .lexeme = "Int" },
        .{ .source = "Float", .kind = .UpperIdent, .lexeme = "Float" },
        .{ .source = "Bool", .kind = .UpperIdent, .lexeme = "Bool" },
        .{ .source = "True", .kind = .UpperIdent, .lexeme = "True" },
        .{ .source = "False", .kind = .UpperIdent, .lexeme = "False" },
        .{ .source = "Unit", .kind = .UpperIdent, .lexeme = "Unit" },
        .{ .source = "foo", .kind = .LowerIdent, .lexeme = "foo" },
        .{ .source = "foo_bar", .kind = .LowerIdent, .lexeme = "foo_bar" },
        .{ .source = "_foo", .kind = .LowerIdent, .lexeme = "_foo" },
        .{ .source = "A", .kind = .UpperIdent, .lexeme = "A" },
        .{ .source = "a", .kind = .LowerIdent, .lexeme = "a" },
        .{ .source = "_", .kind = .SymUnderscore, .lexeme = "_" },
        .{ .source = "ABC123", .kind = .UpperIdent, .lexeme = "ABC123" },
        .{ .source = "abc123", .kind = .LowerIdent, .lexeme = "abc123" },
        .{ .source = "Foo_Bar", .kind = .UpperIdent, .lexeme = "Foo_Bar" },
        .{ .source = "_foo_BAR_123", .kind = .LowerIdent, .lexeme = "_foo_BAR_123" },
        .{ .source = "__foo", .kind = .LowerIdent, .lexeme = "__foo" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source);

        const token = try lexer.nextToken();
        try std.testing.expectEqual(case.kind, token.kind);
        try std.testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try std.testing.expectEqual(TokenKind.Eof, eof.kind);
    }
}

test "invalid identifiers" {
    const invalid_cases = [_][]const u8{
        "_Foo",
        "_Bar",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source);

        try std.testing.expectError(error.InvalidIdentifier, lexer.nextToken());
    }
}

test "type variant" {
    const source = "type FooBar = | Foo | Bar";

    const expected_tokens = [_]Token{
        Token.init(.KwType, "type", 1, 1),
        Token.init(.UpperIdent, "FooBar", 1, 6),
        Token.init(.OpAssign, "=", 1, 13),
        Token.init(.SymPipe, "|", 1, 15),
        Token.init(.UpperIdent, "Foo", 1, 17),
        Token.init(.SymPipe, "|", 1, 21),
        Token.init(.UpperIdent, "Bar", 1, 23),
        Token.init(.Eof, "", 1, 26),
    };

    var lexer = Lexer.init(source);

    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        try std.testing.expectEqual(expected.kind, token.kind);
        try std.testing.expectEqualStrings(expected.lexeme, token.lexeme);
        try std.testing.expectEqual(expected.line, token.line);
        try std.testing.expectEqual(expected.column, token.column);
    }
}

test "type alias" {
    const source = "type alias Seconds = Int";

    const expected_tokens = [_]Token{
        Token.init(.KwType, "type", 1, 1),
        Token.init(.KwAlias, "alias", 1, 6),
        Token.init(.UpperIdent, "Seconds", 1, 12),
        Token.init(.OpAssign, "=", 1, 20),
        Token.init(.UpperIdent, "Int", 1, 22),
        Token.init(.Eof, "", 1, 25),
    };

    var lexer = Lexer.init(source);

    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        try std.testing.expectEqual(expected.kind, token.kind);
        try std.testing.expectEqualStrings(expected.lexeme, token.lexeme);
        try std.testing.expectEqual(expected.line, token.line);
        try std.testing.expectEqual(expected.column, token.column);
    }
}

test "record type" {
    const source = "type FooBar = { foo : Int, bar : String }";

    const expected_tokens = [_]Token{
        Token.init(.KwType, "type", 1, 1),
        Token.init(.UpperIdent, "FooBar", 1, 6),
        Token.init(.OpAssign, "=", 1, 13),
        Token.init(.DelLCurly, "{", 1, 15),
        Token.init(.LowerIdent, "foo", 1, 17),
        Token.init(.DelColon, ":", 1, 21),
        Token.init(.UpperIdent, "Int", 1, 23),
        Token.init(.DelComma, ",", 1, 26),
        Token.init(.LowerIdent, "bar", 1, 28),
        Token.init(.DelColon, ":", 1, 32),
        Token.init(.UpperIdent, "String", 1, 34),
        Token.init(.DelRCurly, "}", 1, 41),
        Token.init(.Eof, "", 1, 42),
    };

    var lexer = Lexer.init(source);

    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        try std.testing.expectEqual(expected.kind, token.kind);
        try std.testing.expectEqualStrings(expected.lexeme, token.lexeme);
        try std.testing.expectEqual(expected.line, token.line);
        try std.testing.expectEqual(expected.column, token.column);
    }
}

test "module definition" {
    const source = "module Foo exposing (Foo(..), bar) end";

    const expected_tokens = [_]Token{
        Token.init(.KwModule, "module", 1, 1),
        Token.init(.UpperIdent, "Foo", 1, 8),
        Token.init(.KwExposing, "exposing", 1, 12),
        Token.init(.DelLParen, "(", 1, 21),
        Token.init(.UpperIdent, "Foo", 1, 22),
        Token.init(.DelLParen, "(", 1, 25),
        Token.init(.OpDoubleDot, "..", 1, 26),
        Token.init(.DelRParen, ")", 1, 28),
        Token.init(.DelComma, ",", 1, 29),
        Token.init(.LowerIdent, "bar", 1, 31),
        Token.init(.DelRParen, ")", 1, 34),
        Token.init(.KwEnd, "end", 1, 36),
        Token.init(.Eof, "", 1, 39),
    };

    var lexer = Lexer.init(source);

    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        try std.testing.expectEqual(expected.kind, token.kind);
        try std.testing.expectEqualStrings(expected.lexeme, token.lexeme);
        try std.testing.expectEqual(expected.line, token.line);
        try std.testing.expectEqual(expected.column, token.column);
    }
}

test "top level function defintion" {
    const source = "let add : Int -> Int -> Int = \\x y => x + y";

    const expected_tokens = [_]Token{
        Token.init(.KwLet, "let", 1, 1),
        Token.init(.LowerIdent, "add", 1, 5),
        Token.init(.DelColon, ":", 1, 9),
        Token.init(.UpperIdent, "Int", 1, 11),
        Token.init(.SymArrowRight, "->", 1, 15),
        Token.init(.UpperIdent, "Int", 1, 18),
        Token.init(.SymArrowRight, "->", 1, 22),
        Token.init(.UpperIdent, "Int", 1, 25),
        Token.init(.OpAssign, "=", 1, 29),
        Token.init(.OpLambda, "\\", 1, 31),
        Token.init(.LowerIdent, "x", 1, 32),
        Token.init(.LowerIdent, "y", 1, 34),
        Token.init(.SymDoubleArrowRight, "=>", 1, 36),
        Token.init(.LowerIdent, "x", 1, 39),
        Token.init(.OpIntAdd, "+", 1, 41),
        Token.init(.LowerIdent, "y", 1, 43),
        Token.init(.Eof, "", 1, 44),
    };

    var lexer = Lexer.init(source);

    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        try std.testing.expectEqual(expected.kind, token.kind);
        try std.testing.expectEqualStrings(expected.lexeme, token.lexeme);
        try std.testing.expectEqual(expected.line, token.line);
        try std.testing.expectEqual(expected.column, token.column);
    }
}

test "pattern matching" {
    const source = "match x on | Foo => 1 | Bar => 2 _ => 3";

    const expected_tokens = [_]Token{
        Token.init(.KwMatch, "match", 1, 1),
        Token.init(.LowerIdent, "x", 1, 7),
        Token.init(.KwOn, "on", 1, 9),
        Token.init(.SymPipe, "|", 1, 12),
        Token.init(.UpperIdent, "Foo", 1, 14),
        Token.init(.SymDoubleArrowRight, "=>", 1, 18),
        Token.init(.LitInt, "1", 1, 21),
        Token.init(.SymPipe, "|", 1, 23),
        Token.init(.UpperIdent, "Bar", 1, 25),
        Token.init(.SymDoubleArrowRight, "=>", 1, 29),
        Token.init(.LitInt, "2", 1, 32),
        Token.init(.SymUnderscore, "_", 1, 34),
        Token.init(.SymDoubleArrowRight, "=>", 1, 36),
        Token.init(.LitInt, "3", 1, 39),
        Token.init(.Eof, "", 1, 40),
    };

    var lexer = Lexer.init(source);

    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        try std.testing.expectEqual(expected.kind, token.kind);
        try std.testing.expectEqualStrings(expected.lexeme, token.lexeme);
        try std.testing.expectEqual(expected.line, token.line);
        try std.testing.expectEqual(expected.column, token.column);
    }
}

test "let/in block" {
    const source = "let x : Int = 42 in";

    const expected_tokens = [_]Token{
        Token.init(.KwLet, "let", 1, 1),
        Token.init(.LowerIdent, "x", 1, 5),
        Token.init(.DelColon, ":", 1, 7),
        Token.init(.UpperIdent, "Int", 1, 9),
        Token.init(.OpAssign, "=", 1, 13),
        Token.init(.LitInt, "42", 1, 15),
        Token.init(.KwIn, "in", 1, 18),
        Token.init(.Eof, "", 1, 20),
    };

    var lexer = Lexer.init(source);

    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        try std.testing.expectEqual(expected.kind, token.kind);
        try std.testing.expectEqualStrings(expected.lexeme, token.lexeme);
        try std.testing.expectEqual(expected.line, token.line);
        try std.testing.expectEqual(expected.column, token.column);
    }
}

test "if/then/else expression" {
    const source = "if x == 1 then True else False";

    const expected_tokens = [_]Token{
        Token.init(.KwIf, "if", 1, 1),
        Token.init(.LowerIdent, "x", 1, 4),
        Token.init(.OpEquality, "==", 1, 6),
        Token.init(.LitInt, "1", 1, 9),
        Token.init(.KwThen, "then", 1, 11),
        Token.init(.UpperIdent, "True", 1, 16),
        Token.init(.KwElse, "else", 1, 21),
        Token.init(.UpperIdent, "False", 1, 26),
        Token.init(.Eof, "", 1, 31),
    };

    var lexer = Lexer.init(source);

    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        try std.testing.expectEqual(expected.kind, token.kind);
        try std.testing.expectEqualStrings(expected.lexeme, token.lexeme);
        try std.testing.expectEqual(expected.line, token.line);
        try std.testing.expectEqual(expected.column, token.column);
    }
}
