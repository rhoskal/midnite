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
    Colon,
    Comma,
    Dot,
    LBrack,
    LCurly,
    LParen,
    RBrack,
    RCurly,
    RParen,

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
    OpRange,

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
    LitChar,
    LitFloat,
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

pub const Token = struct {
    kind: TokenKind,
    lexeme: []const u8,
    line: usize,
    column: usize,

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
    UnterminatedString,
};

pub const Lexer = struct {
    source: []const u8,
    position: usize,
    line: usize,
    column: usize,

    pub fn init(source: []const u8) Lexer {
        return Lexer{
            .source = source,
            .position = 0,
            .line = 1,
            .column = 1,
        };
    }

    fn peek(self: *Lexer) ?u8 {
        if (self.position >= self.source.len) return null;

        return self.source[self.position];
    }

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

    fn checkExactMatch(self: *Lexer, start: usize, keyword: []const u8, kind: TokenKind) ?Token {
        const len = keyword.len;

        if (self.position - start == len and
            std.mem.eql(u8, self.source[start..self.position], keyword))
        {
            return Token.init(kind, self.source[start..self.position], self.line, self.column - len);
        }

        return null;
    }

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
                const mark = self.position;
                self.advance();

                if (self.peek()) |next| {
                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpFloatAdd,
                            self.source[mark..self.position],
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.OpIntAdd,
                    self.source[mark..self.position],
                    start_line,
                    start_column,
                );
            },
            '-' => {
                const mark = self.position;
                self.advance();

                if (self.peek()) |next| {
                    if (next == '>') {
                        self.advance();

                        return Token.init(
                            TokenKind.SymArrowRight,
                            self.source[mark..self.position],
                            start_line,
                            start_column,
                        );
                    }

                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpFloatSub,
                            self.source[mark..self.position],
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.OpIntSub,
                    self.source[mark..self.position],
                    start_line,
                    start_column,
                );
            },
            '*' => {
                const mark = self.position;
                self.advance();

                if (self.peek()) |next| {
                    if (next == '*') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpExp,
                            self.source[mark..self.position],
                            start_line,
                            start_column,
                        );
                    }

                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpFloatMul,
                            self.source[mark..self.position],
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.OpIntMul,
                    self.source[mark..self.position],
                    start_line,
                    start_column,
                );
            },
            '/' => {
                const mark = self.position;
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpNotEqual,
                            self.source[mark..self.position],
                            start_line,
                            start_column,
                        );
                    }

                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpFloatDiv,
                            self.source[mark..self.position],
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.OpIntDiv,
                    self.source[mark..self.position],
                    start_line,
                    start_column,
                );
            },
            '<' => {
                const mark = self.position;
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpLessThanEqual,
                            self.source[mark..self.position],
                            start_line,
                            start_column,
                        );
                    }

                    if (next == '>') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpAppend,
                            self.source[mark..self.position],
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.OpLessThan,
                    self.source[mark..self.position],
                    start_line,
                    start_column,
                );
            },
            '>' => {
                const mark = self.position;
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpGreaterThanEqual,
                            self.source[mark..self.position],
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.OpGreaterThan,
                    self.source[mark..self.position],
                    start_line,
                    start_column,
                );
            },
            '&' => {
                const mark = self.position;
                self.advance();

                if (self.peek()) |next| {
                    if (next == '&') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpLogicalAnd,
                            self.source[mark..self.position],
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.Invalid,
                    self.source[mark..self.position],
                    start_line,
                    start_column,
                );
            },
            '|' => {
                const mark = self.position;
                self.advance();

                if (self.peek()) |next| {
                    if (next == '|') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpLogicalOr,
                            self.source[mark..self.position],
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.SymPipe,
                    self.source[mark..self.position],
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
                const mark = self.position;
                self.advance();

                if (self.peek()) |next| {
                    if (next == ':') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpCons,
                            self.source[mark..self.position],
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.Colon,
                    self.source[mark..self.position],
                    start_line,
                    start_column,
                );
            },
            ',' => {
                self.advance();

                return Token.init(
                    TokenKind.Comma,
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
                const mark = self.position;
                self.advance();

                if (self.peek()) |next| {
                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpRange,
                            self.source[mark..self.position],
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.Dot,
                    self.source[mark..self.position],
                    start_line,
                    start_column,
                );
            },
            '{' => {
                self.advance();

                return Token.init(
                    TokenKind.LCurly,
                    "{",
                    start_line,
                    start_column,
                );
            },
            '}' => {
                self.advance();

                return Token.init(
                    TokenKind.RCurly,
                    "}",
                    start_line,
                    start_column,
                );
            },
            '=' => {
                const mark = self.position;
                self.advance();

                if (self.peek()) |next| {
                    if (next == '>') {
                        self.advance();

                        return Token.init(
                            TokenKind.SymDoubleArrowRight,
                            self.source[mark..self.position],
                            start_line,
                            start_column,
                        );
                    }

                    if (next == '=') {
                        self.advance();

                        return Token.init(
                            TokenKind.OpEquality,
                            self.source[mark..self.position],
                            start_line,
                            start_column,
                        );
                    }
                }

                return Token.init(
                    TokenKind.OpAssign,
                    self.source[mark..self.position],
                    start_line,
                    start_column,
                );
            },
            '(' => {
                self.advance();

                return Token.init(
                    TokenKind.LParen,
                    "(",
                    start_line,
                    start_column,
                );
            },
            ')' => {
                self.advance();

                return Token.init(
                    TokenKind.RParen,
                    ")",
                    start_line,
                    start_column,
                );
            },
            '[' => {
                self.advance();

                return Token.init(
                    TokenKind.LBrack,
                    "[",
                    start_line,
                    start_column,
                );
            },
            ']' => {
                self.advance();

                return Token.init(
                    TokenKind.RBrack,
                    "]",
                    start_line,
                    start_column,
                );
            },
            'a'...'z', 'A'...'Z', '_' => {
                // Handle identifiers and keywords
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

                return Token.init(
                    TokenKind.Invalid,
                    self.source[start..self.position],
                    start_line,
                    start_column,
                );
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

test "keywords" {
    const source = "alias as else end exposing foreign hiding if include infixl infixn infixr let match module on open renaming then to type using when";
    var lexer = Lexer.init(source);

    const expected = [_]TokenKind{
        .KwAlias,
        .KwAs,
        .KwElse,
        .KwEnd,
        .KwExposing,
        .KwForeign,
        .KwHiding,
        .KwIf,
        .KwInclude,
        .KwInfixLeft,
        .KwInfixNon,
        .KwInfixRight,
        .KwLet,
        .KwMatch,
        .KwModule,
        .KwOn,
        .KwOpen,
        .KwRenaming,
        .KwThen,
        .KwTo,
        .KwType,
        .KwUsing,
        .KwWhen,
        .Eof,
    };

    for (expected) |expected_kind| {
        const token = try lexer.nextToken();
        std.debug.assert(token.kind == expected_kind);
    }
}

test "delimiters" {
    const source = ": , . [ { ( ] } )";
    var lexer = Lexer.init(source);

    const expected = [_]TokenKind{
        .Colon,
        .Comma,
        .Dot,
        .LBrack,
        .LCurly,
        .LParen,
        .RBrack,
        .RCurly,
        .RParen,
        .Eof,
    };

    for (expected) |expected_kind| {
        const token = try lexer.nextToken();
        std.debug.assert(token.kind == expected_kind);
    }
}

test "symbols" {
    const source = "-> $ => |";
    var lexer = Lexer.init(source);

    const expected = [_]TokenKind{
        .SymArrowRight,
        .SymDollarSign,
        .SymDoubleArrowRight,
        .SymPipe,
        // .Underscore,
        .Eof,
    };

    for (expected) |expected_kind| {
        const token = try lexer.nextToken();
        std.debug.assert(token.kind == expected_kind);
    }
}

test "operators" {
    const source = "<> = :: \\ ..";
    var lexer = Lexer.init(source);

    const expected = [_]TokenKind{
        .OpAppend,
        .OpAssign,
        .OpCons,
        .OpLambda,
        .OpRange,
        .Eof,
    };

    for (expected) |expected_kind| {
        const token = try lexer.nextToken();
        std.debug.assert(token.kind == expected_kind);
    }
}

test "arithmetic operators" {
    const source = "** +. /. *. -. + / * -";
    var lexer = Lexer.init(source);

    const expected = [_]TokenKind{
        .OpExp,
        .OpFloatAdd,
        .OpFloatDiv,
        .OpFloatMul,
        .OpFloatSub,
        .OpIntAdd,
        .OpIntDiv,
        .OpIntMul,
        .OpIntSub,
        .Eof,
    };

    for (expected) |expected_kind| {
        const token = try lexer.nextToken();
        std.debug.assert(token.kind == expected_kind);
    }
}

test "comparison (relational) operators" {
    const source = "== > >= < <= /=";
    var lexer = Lexer.init(source);

    const expected = [_]TokenKind{
        .OpEquality,
        .OpGreaterThan,
        .OpGreaterThanEqual,
        .OpLessThan,
        .OpLessThanEqual,
        .OpNotEqual,
        .Eof,
    };

    for (expected) |expected_kind| {
        const token = try lexer.nextToken();
        std.debug.assert(token.kind == expected_kind);
    }
}

test "logical (boolean) operators" {
    const source = "&& ||";
    var lexer = Lexer.init(source);

    const expected = [_]TokenKind{
        .OpLogicalAnd,
        .OpLogicalOr,
        .Eof,
    };

    for (expected) |expected_kind| {
        const token = try lexer.nextToken();
        std.debug.assert(token.kind == expected_kind);
    }
}

test "special" {
    const source = "?";
    var lexer = Lexer.init(source);

    const expected = [_]TokenKind{
        .TypedHole,
        .Eof,
    };

    for (expected) |expected_kind| {
        const token = try lexer.nextToken();
        std.debug.assert(token.kind == expected_kind);
    }
}

test "comments" {
    const source =
        \\# this is a comment
        \\## this is a doc comment
        \\# 你好，世界
        \\## こんにちは
        \\# 🚀 👽 💣
    ;
    var lexer = Lexer.init(source);

    const expected = [_]TokenKind{
        .Comment,
        .DocComment,
        .Comment,
        .DocComment,
        .Comment,
        .Eof,
    };

    for (expected) |expected_kind| {
        const token = try lexer.nextToken();
        std.debug.assert(token.kind == expected_kind);
    }
}

test "multiline string" {
    const source =
        \\""" This is a
        \\multiline string with
        \\unicode: 你好, こんにちは
        \\"""
    ;
    var lexer = Lexer.init(source);

    const expected = [_]TokenKind{
        .LitMultilineString,
        .Eof,
    };

    for (expected) |expected_kind| {
        const token = try lexer.nextToken();
        std.debug.assert(token.kind == expected_kind);
    }
}

test "invalid" {
    const source = "%";
    var lexer = Lexer.init(source);

    const expected = [_]TokenKind{
        .Invalid,
        .Eof,
    };

    for (expected) |expected_kind| {
        const token = try lexer.nextToken();
        std.debug.assert(token.kind == expected_kind);
    }
}
