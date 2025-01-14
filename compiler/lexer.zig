const std = @import("std");
const ascii = std.ascii;

pub const TokenKind = enum {
    // Special
    Eof,
    Unrecognized,
    TypedHole,

    // Comments
    Comment,
    DocComment,

    // Identifiers
    LowerIdent,
    UpperIdent,

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
    OpComposeLeft,
    OpComposeRight,
    OpCons,
    OpDoubleDot,
    OpLambda,
    OpPipeLeft,
    OpPipeRight,

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
    LitInt,
    LitFloat,
    LitChar,
    LitString,
    LitMultilineString,
};

/// Represents a location in the source buffer using raw byte indices.
pub const BufferLoc = struct {
    /// The start index of the lexeme in the source buffer (inclusive).
    start: usize,

    /// The end index of the lexeme in the source buffer (exclusive).
    end: usize,
};

/// Represents a human-readable location in the source file.
pub const SourceLoc = struct {
    /// The line number in the source file (1-based).
    line: usize,

    /// The column number in the source file (1-based).
    col: usize,
};

/// Represents the complete location of a token, combining buffer and source information.
pub const TokenLoc = struct {
    /// The name of the source file where the token originated.
    filename: []const u8,

    /// The raw buffer location of the token.
    buf: BufferLoc,

    /// The human-readable source location of the token.
    src: SourceLoc,
};

/// A structure representing a token with a specific kind, lexeme, and position in the source code.
pub const Token = struct {
    kind: TokenKind,
    lexeme: []const u8,
    loc: TokenLoc,

    /// Initializes a new token with the given properties.
    ///
    /// - `kind`: The kind of token (e.g., literal, keyword).
    /// - `lexeme`: The string representation of the token.
    /// - `filename`: The name of the source file where the token is located.
    /// - `buf_start`: The start index of the token in the source buffer (inclusive).
    /// - `buf_end`: The end index of the token in the source buffer (exclusive).
    /// - `src_line`: The line number where the token is found (1-based).
    /// - `src_col`: The column number where the token is found (1-based).
    pub fn init(
        kind: TokenKind,
        lexeme: []const u8,
        filename: []const u8,
        buf_start: usize,
        buf_end: usize,
        src_line: usize,
        src_col: usize,
    ) Token {
        return Token{
            .kind = kind,
            .lexeme = lexeme,
            .loc = .{
                .filename = filename,
                .buf = .{
                    .start = buf_start,
                    .end = buf_end,
                },
                .src = .{
                    .line = src_line,
                    .col = src_col,
                },
            },
        };
    }
};

pub const LexerError = error{
    CodePointOutOfRange,
    EmptyCharLiteral,
    InvalidIdentifier,
    InvalidIntLiteral,
    InvalidUnicodeEscapeSequence,
    MultipleCharsInLiteral,
    UnrecognizedEscapeSequence,
    UnterminatedCharLiteral,
    UnterminatedStrLiteral,
};

/// A structure representing a lexer which processes source code and generates tokens.
/// It keeps track of the position in the source code, the current line and column, and performs
/// operations to extract tokens from the source.
pub const Lexer = struct {
    source: []const u8,
    filename: []const u8,
    buf_pos: usize,
    src_line: usize,
    src_col: usize,

    /// Initializes a new lexer with the given source code.
    ///
    /// - `source`: The source code to be lexed.
    /// - `filename`: The name of the source file where the token is located.
    pub fn init(source: []const u8, filename: []const u8) Lexer {
        return Lexer{
            .source = source,
            .filename = filename,
            .buf_pos = 0,
            .src_line = 1,
            .src_col = 1,
        };
    }

    /// Peeks at the next character in the source code without advancing the position.
    ///
    /// Returns `null` if the end of the source is reached.
    fn peek(self: *Lexer) ?u8 {
        if (self.buf_pos >= self.source.len) return null;

        return self.source[self.buf_pos];
    }

    /// Advances the lexer by one position in the source code.
    /// Updates the current line and column numbers accordingly.
    fn advance(self: *Lexer) void {
        if (self.buf_pos >= self.source.len) return;

        if (self.source[self.buf_pos] == '\n') {
            self.src_line += 1;
            self.src_col = 1;
        } else {
            self.src_col += 1;
        }

        self.buf_pos += 1;
    }

    fn skipWhitespace(self: *Lexer) void {
        while (self.peek()) |c| {
            if (!ascii.isWhitespace(c)) break;

            self.advance();
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
        const lexeme = self.source[start..self.buf_pos];

        const is_exact_match = self.buf_pos - start == len and
            std.mem.eql(u8, lexeme, keyword);

        if (!is_exact_match) return null;

        return Token.init(
            kind,
            lexeme,
            self.filename,
            start,
            self.buf_pos,
            self.src_line,
            self.src_col - len,
        );
    }

    fn isBinDigit(c: u8) bool {
        return c == '0' or c == '1';
    }

    fn isOctDigit(c: u8) bool {
        return c >= '0' and c <= '7';
    }

    fn hexDigitToValue(digit: u8) u4 {
        return switch (digit) {
            '0'...'9' => @intCast(digit - '0'),
            'a'...'f' => @intCast(digit - 'a' + 10),
            'A'...'F' => @intCast(digit - 'A' + 10),
            else => unreachable,
        };
    }

    fn lexInteger(self: *Lexer, base: enum { Decimal, Hex, Octal, Binary }) !Token {
        const offset = if (base == .Decimal) @as(usize, 0) else 2;
        const start = self.buf_pos - offset;
        const start_line = self.src_line;
        const start_col = self.src_col - offset;

        // For other bases, prefix (0x, 0b, 0o) was already consumed
        if (base == .Decimal) {
            self.advance();
        }

        if (base != .Decimal) {
            if (self.peek()) |next| {
                if (next == '_') {
                    return error.InvalidIntLiteral;
                }
            }
        }

        var found_valid_digit = true;
        var last_was_underscore = false;

        while (self.peek()) |c| {
            const is_valid_digit = switch (base) {
                .Decimal => ascii.isDigit(c),
                .Hex => ascii.isHex(c),
                .Binary => isBinDigit(c),
                .Octal => isOctDigit(c),
            };

            if (is_valid_digit) {
                found_valid_digit = true;
                last_was_underscore = false;
                self.advance();
            } else if (c == '_') {
                if (!found_valid_digit or last_was_underscore) {
                    return error.InvalidIntLiteral;
                }

                last_was_underscore = true;
                self.advance();
            } else {
                break;
            }
        }

        if (last_was_underscore) {
            return error.InvalidIntLiteral;
        }

        return Token.init(
            .LitInt,
            self.source[start..self.buf_pos],
            self.filename,
            start,
            self.buf_pos,
            start_line,
            start_col,
        );
    }

    /// Retrieves the next token from the source code, advancing the lexer.
    ///
    /// - Returns: A token object corresponding to the next recognized token.
    pub fn nextToken(self: *Lexer) !Token {
        self.skipWhitespace();

        const start = self.buf_pos;
        const start_line = self.src_line;
        const start_col = self.src_col;

        const c = self.peek() orelse {
            return Token.init(
                .Eof,
                "",
                self.filename,
                start,
                start, // EOF has no length
                start_line,
                start_col,
            );
        };

        switch (c) {
            '?' => {
                self.advance();

                if (self.peek() == null) {
                    return Token.init(
                        .TypedHole,
                        "?",
                        self.filename,
                        start,
                        self.buf_pos,
                        start_line,
                        start_col,
                    );
                }

                return error.InvalidIdentifier;
            },
            '#' => {
                const mark = self.buf_pos;
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
                    self.source[mark..self.buf_pos],
                    self.filename,
                    mark,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            '"' => {
                const mark = self.buf_pos;
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
                                                // fail silently with invalid sequence
                                                continue;
                                            };

                                            var i: usize = 1;
                                            while (i < utf8_len) : (i += 1) {
                                                self.advance();
                                            }
                                        }
                                    } else {
                                        const utf8_len = std.unicode.utf8ByteSequenceLength(next) catch {
                                            // fail silently with invalid sequence
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
                                    return error.UnterminatedStrLiteral;
                                }

                                return Token.init(
                                    TokenKind.LitMultilineString,
                                    self.source[mark..self.buf_pos],
                                    self.filename,
                                    start,
                                    self.buf_pos,
                                    start_line,
                                    start_col,
                                );
                            }
                        }
                    }
                }

                while (self.peek()) |next| {
                    if (next == '"') {
                        self.advance();

                        return Token.init(
                            TokenKind.LitString,
                            self.source[start..self.buf_pos],
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }

                    if (next == '\\') {
                        self.advance();

                        const escaped_char = self.peek() orelse return error.UnterminatedStrLiteral;
                        switch (escaped_char) {
                            '\\', '"', 'n', 't', 'r', 'b' => {
                                self.advance();
                            },
                            'u' => {
                                self.advance();

                                var unicode_value: u21 = 0;
                                var digit_count: usize = 0;
                                while (digit_count < 6) : (digit_count += 1) {
                                    const hex = self.peek() orelse break;
                                    if (!ascii.isHex(hex)) break;

                                    const digit_value = hexDigitToValue(hex);
                                    unicode_value = (unicode_value << 4) | digit_value;

                                    self.advance();
                                }

                                if (digit_count == 0) return error.InvalidUnicodeEscapeSequence;

                                if (unicode_value > 0x10FFFF or
                                    (unicode_value >= 0xD800 and unicode_value <= 0xDFFF))
                                {
                                    return error.CodePointOutOfRange;
                                }
                            },
                            else => return error.UnrecognizedEscapeSequence,
                        }
                    } else {
                        const utf8_len = std.unicode.utf8ByteSequenceLength(next) catch {
                            // fail silently with invalid sequence
                            self.advance();

                            continue;
                        };

                        var i: usize = 0;
                        while (i < utf8_len) : (i += 1) {
                            self.advance();
                        }
                    }
                }

                return error.UnterminatedStrLiteral;
            },
            '\'' => {
                const mark = self.buf_pos;
                self.advance();

                if (self.peek() == '\'') return error.EmptyCharLiteral;

                var char_count: usize = 0;
                while (self.peek()) |next| {
                    if (next == '\'') {
                        self.advance();

                        return Token.init(
                            .LitChar,
                            self.source[mark..self.buf_pos],
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }

                    if (next == '\\') {
                        self.advance();
                        char_count += 1;

                        const escaped_char = self.peek() orelse return error.UnterminatedCharLiteral;
                        switch (escaped_char) {
                            '\\', '\'', 'n', 't', 'r' => {
                                self.advance();
                            },
                            'u' => {
                                self.advance();

                                var unicode_value: u21 = 0;
                                var digit_count: usize = 0;
                                while (digit_count < 6) : (digit_count += 1) {
                                    const hex = self.peek() orelse break;
                                    if (!ascii.isHex(hex)) break;

                                    const digit_value = hexDigitToValue(hex);
                                    unicode_value = (unicode_value << 4) | digit_value;

                                    self.advance();
                                }

                                if (digit_count == 0) return error.InvalidUnicodeEscapeSequence;

                                if (unicode_value > 0x10FFFF or
                                    (unicode_value >= 0xD800 and unicode_value <= 0xDFFF))
                                {
                                    return error.CodePointOutOfRange;
                                }
                            },
                            else => return error.UnrecognizedEscapeSequence,
                        }
                    } else {
                        char_count += 1;

                        const utf8_len = std.unicode.utf8ByteSequenceLength(next) catch {
                            // fail silently with invalid sequence
                            self.advance();
                            continue;
                        };

                        var i: usize = 0;
                        while (i < utf8_len) : (i += 1) {
                            self.advance();
                        }
                    }

                    if (char_count > 1) return error.MultipleCharsInLiteral;
                }

                return error.UnterminatedCharLiteral;
            },
            '+' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            .OpFloatAdd,
                            "+.",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }
                }

                return Token.init(
                    .OpIntAdd,
                    "+",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            '-' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '>') {
                        self.advance();

                        return Token.init(
                            .SymArrowRight,
                            "->",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }

                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            .OpFloatSub,
                            "-.",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }
                }

                return Token.init(
                    .OpIntSub,
                    "-",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            '*' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '*') {
                        self.advance();

                        return Token.init(
                            .OpExp,
                            "**",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }

                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            .OpFloatMul,
                            "*.",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }
                }

                return Token.init(
                    .OpIntMul,
                    "*",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            '/' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        return Token.init(
                            .OpNotEqual,
                            "/=",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }

                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            .OpFloatDiv,
                            "/.",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }
                }

                return Token.init(
                    .OpIntDiv,
                    "/",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            '<' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        return Token.init(
                            .OpLessThanEqual,
                            "<=",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }

                    if (next == '>') {
                        self.advance();

                        return Token.init(
                            .OpAppend,
                            "<>",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }

                    if (next == '|') {
                        self.advance();

                        return Token.init(
                            .OpPipeLeft,
                            "<|",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }

                    if (next == '<') {
                        self.advance();

                        return Token.init(
                            .OpComposeLeft,
                            "<<",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }
                }

                return Token.init(
                    .OpLessThan,
                    "<",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            '>' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        return Token.init(
                            .OpGreaterThanEqual,
                            ">=",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }

                    if (next == '>') {
                        self.advance();

                        return Token.init(
                            .OpComposeRight,
                            ">>",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }
                }

                return Token.init(
                    .OpGreaterThan,
                    ">",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            '&' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '&') {
                        self.advance();

                        return Token.init(
                            .OpLogicalAnd,
                            "&&",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }
                }

                return Token.init(
                    .Unrecognized,
                    "&",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            '|' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '|') {
                        self.advance();

                        return Token.init(
                            .OpLogicalOr,
                            "||",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }

                    if (next == '>') {
                        self.advance();

                        return Token.init(
                            .OpPipeRight,
                            "|>",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }
                }

                return Token.init(
                    .SymPipe,
                    "|",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            '\\' => {
                self.advance();

                return Token.init(
                    .OpLambda,
                    "\\",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            ':' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == ':') {
                        self.advance();

                        return Token.init(
                            .OpCons,
                            "::",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }
                }

                return Token.init(
                    .DelColon,
                    ":",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            ',' => {
                self.advance();

                return Token.init(
                    .DelComma,
                    ",",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            '$' => {
                self.advance();

                return Token.init(
                    .SymDollarSign,
                    "$",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            '.' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            .OpDoubleDot,
                            "..",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }
                }

                return Token.init(
                    .DelDot,
                    ".",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            '{' => {
                self.advance();

                return Token.init(
                    .DelLCurly,
                    "{",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            '}' => {
                self.advance();

                return Token.init(
                    .DelRCurly,
                    "}",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            '=' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '>') {
                        self.advance();

                        return Token.init(
                            .SymDoubleArrowRight,
                            "=>",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }

                    if (next == '=') {
                        self.advance();

                        return Token.init(
                            .OpEquality,
                            "==",
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }
                }

                return Token.init(
                    .OpAssign,
                    "=",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            '(' => {
                self.advance();

                return Token.init(
                    .DelLParen,
                    "(",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            ')' => {
                self.advance();

                return Token.init(
                    .DelRParen,
                    ")",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            '[' => {
                self.advance();

                return Token.init(
                    .DelLBrack,
                    "[",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
            ']' => {
                self.advance();

                return Token.init(
                    .DelRBrack,
                    "]",
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
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
                    .LitInt,
                    self.source[start..self.buf_pos],
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
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
                            .SymUnderscore,
                            self.source[start..self.buf_pos],
                            self.filename,
                            start,
                            self.buf_pos,
                            start_line,
                            start_col,
                        );
                    }

                    if (self.peek()) |next| {
                        if (ascii.isUpper(next)) return error.InvalidIdentifier;

                        if (ascii.isDigit(next)) return error.InvalidIntLiteral;

                        switch (next) {
                            'a'...'z', '_' => {
                                while (self.peek()) |next_char| {
                                    switch (next_char) {
                                        'a'...'z', 'A'...'Z', '_', '0'...'9' => self.advance(),
                                        '?' => {
                                            self.advance();

                                            if (self.peek()) |x| {
                                                if (!ascii.isWhitespace(x)) return error.InvalidIdentifier;
                                            }

                                            break;
                                        },
                                        else => break,
                                    }
                                }

                                return Token.init(
                                    .LowerIdent,
                                    self.source[start..self.buf_pos],
                                    self.filename,
                                    start,
                                    self.buf_pos,
                                    start_line,
                                    start_col,
                                );
                            },
                            else => {
                                return Token.init(
                                    .SymUnderscore,
                                    "_",
                                    self.filename,
                                    start,
                                    self.buf_pos,
                                    start_line,
                                    start_col,
                                );
                            },
                        }
                    }

                    return Token.init(
                        .SymUnderscore,
                        "_",
                        self.filename,
                        start,
                        self.buf_pos,
                        start_line,
                        start_col,
                    );
                }

                while (self.peek()) |next| {
                    switch (next) {
                        'a'...'z', 'A'...'Z', '_', '0'...'9' => self.advance(),
                        '?' => {
                            self.advance();

                            if (self.peek()) |x| {
                                if (!ascii.isWhitespace(x)) return error.InvalidIdentifier;
                            }

                            break;
                        },
                        else => break,
                    }
                }

                if (self.checkExactMatch(start, "alias", .KwAlias)) |token| return token;
                if (self.checkExactMatch(start, "as", .KwAs)) |token| return token;
                if (self.checkExactMatch(start, "else", .KwElse)) |token| return token;
                if (self.checkExactMatch(start, "end", .KwEnd)) |token| return token;
                if (self.checkExactMatch(start, "exposing", .KwExposing)) |token| return token;
                if (self.checkExactMatch(start, "foreign", .KwForeign)) |token| return token;
                if (self.checkExactMatch(start, "hiding", .KwHiding)) |token| return token;
                if (self.checkExactMatch(start, "if", .KwIf)) |token| return token;
                if (self.checkExactMatch(start, "in", .KwIn)) |token| return token;
                if (self.checkExactMatch(start, "include", .KwInclude)) |token| return token;
                if (self.checkExactMatch(start, "infixl", .KwInfixLeft)) |token| return token;
                if (self.checkExactMatch(start, "infixn", .KwInfixNon)) |token| return token;
                if (self.checkExactMatch(start, "infixr", .KwInfixRight)) |token| return token;
                if (self.checkExactMatch(start, "let", .KwLet)) |token| return token;
                if (self.checkExactMatch(start, "match", .KwMatch)) |token| return token;
                if (self.checkExactMatch(start, "module", .KwModule)) |token| return token;
                if (self.checkExactMatch(start, "on", .KwOn)) |token| return token;
                if (self.checkExactMatch(start, "open", .KwOpen)) |token| return token;
                if (self.checkExactMatch(start, "renaming", .KwRenaming)) |token| return token;
                if (self.checkExactMatch(start, "then", .KwThen)) |token| return token;
                if (self.checkExactMatch(start, "to", .KwTo)) |token| return token;
                if (self.checkExactMatch(start, "type", .KwType)) |token| return token;
                if (self.checkExactMatch(start, "using", .KwUsing)) |token| return token;
                if (self.checkExactMatch(start, "when", .KwWhen)) |token| return token;

                const first_char = self.source[start];

                switch (first_char) {
                    'A'...'Z' => return Token.init(
                        .UpperIdent,
                        self.source[start..self.buf_pos],
                        self.filename,
                        start,
                        self.buf_pos,
                        start_line,
                        start_col,
                    ),
                    'a'...'z', '_' => return Token.init(
                        .LowerIdent,
                        self.source[start..self.buf_pos],
                        self.filename,
                        start,
                        self.buf_pos,
                        start_line,
                        start_col,
                    ),
                    else => return Token.init(
                        .Unrecognized,
                        self.source[start..self.buf_pos],
                        self.filename,
                        start,
                        self.buf_pos,
                        start_line,
                        start_col,
                    ),
                }
            },
            else => {
                self.advance();

                return Token.init(
                    .Unrecognized,
                    self.source[start..self.buf_pos],
                    self.filename,
                    start,
                    self.buf_pos,
                    start_line,
                    start_col,
                );
            },
        }
    }
};

const testing = std.testing;

const TEST_FILE = "some_file";

const TestCase = struct {
    source: []const u8,
    kind: TokenKind,
    lexeme: []const u8,
};

test "[keyword]" {
    const cases = [_]TestCase{
        .{ .source = "alias", .kind = .KwAlias, .lexeme = "alias" },
        .{ .source = "as", .kind = .KwAs, .lexeme = "as" },
        .{ .source = "else", .kind = .KwElse, .lexeme = "else" },
        .{ .source = "end", .kind = .KwEnd, .lexeme = "end" },
        .{ .source = "exposing", .kind = .KwExposing, .lexeme = "exposing" },
        .{ .source = "foreign", .kind = .KwForeign, .lexeme = "foreign" },
        .{ .source = "hiding", .kind = .KwHiding, .lexeme = "hiding" },
        .{ .source = "if", .kind = .KwIf, .lexeme = "if" },
        .{ .source = "in", .kind = .KwIn, .lexeme = "in" },
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
        var lexer = Lexer.init(case.source, TEST_FILE);

        const token = try lexer.nextToken();
        try testing.expectEqual(case.kind, token.kind);
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try testing.expectEqual(.Eof, eof.kind);
    }
}

test "[delimiter]" {
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
        var lexer = Lexer.init(case.source, TEST_FILE);

        const token = try lexer.nextToken();
        try testing.expectEqual(case.kind, token.kind);
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try testing.expectEqual(.Eof, eof.kind);
    }
}

test "[symbol]" {
    const cases = [_]TestCase{
        .{ .source = "->", .kind = .SymArrowRight, .lexeme = "->" },
        .{ .source = "$", .kind = .SymDollarSign, .lexeme = "$" },
        .{ .source = "=>", .kind = .SymDoubleArrowRight, .lexeme = "=>" },
        .{ .source = "|", .kind = .SymPipe, .lexeme = "|" },
        .{ .source = "_", .kind = .SymUnderscore, .lexeme = "_" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        const token = try lexer.nextToken();
        try testing.expectEqual(case.kind, token.kind);
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try testing.expectEqual(.Eof, eof.kind);
    }
}

test "[operator]" {
    const cases = [_]TestCase{
        .{ .source = "<>", .kind = .OpAppend, .lexeme = "<>" },
        .{ .source = "=", .kind = .OpAssign, .lexeme = "=" },
        .{ .source = "<<", .kind = .OpComposeLeft, .lexeme = "<<" },
        .{ .source = ">>", .kind = .OpComposeRight, .lexeme = ">>" },
        .{ .source = "::", .kind = .OpCons, .lexeme = "::" },
        .{ .source = "..", .kind = .OpDoubleDot, .lexeme = ".." },
        .{ .source = "\\", .kind = .OpLambda, .lexeme = "\\" },
        .{ .source = "<|", .kind = .OpPipeLeft, .lexeme = "<|" },
        .{ .source = "|>", .kind = .OpPipeRight, .lexeme = "|>" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        const token = try lexer.nextToken();
        try testing.expectEqual(case.kind, token.kind);
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try testing.expectEqual(.Eof, eof.kind);
    }
}

test "[arithmetic operator]" {
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
        var lexer = Lexer.init(case.source, TEST_FILE);

        const token = try lexer.nextToken();
        try testing.expectEqual(case.kind, token.kind);
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try testing.expectEqual(.Eof, eof.kind);
    }
}

test "[comparison operator] (relational)" {
    const cases = [_]TestCase{
        .{ .source = "==", .kind = .OpEquality, .lexeme = "==" },
        .{ .source = ">", .kind = .OpGreaterThan, .lexeme = ">" },
        .{ .source = ">=", .kind = .OpGreaterThanEqual, .lexeme = ">=" },
        .{ .source = "<", .kind = .OpLessThan, .lexeme = "<" },
        .{ .source = "<=", .kind = .OpLessThanEqual, .lexeme = "<=" },
        .{ .source = "/=", .kind = .OpNotEqual, .lexeme = "/=" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        const token = try lexer.nextToken();
        try testing.expectEqual(case.kind, token.kind);
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try testing.expectEqual(.Eof, eof.kind);
    }
}

test "[logical operator] (boolean)" {
    const cases = [_]TestCase{
        .{ .source = "&&", .kind = .OpLogicalAnd, .lexeme = "&&" },
        .{ .source = "||", .kind = .OpLogicalOr, .lexeme = "||" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        const token = try lexer.nextToken();
        try testing.expectEqual(case.kind, token.kind);
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try testing.expectEqual(.Eof, eof.kind);
    }
}

test "[special]" {
    const cases = [_]TestCase{
        .{ .source = "?", .kind = .TypedHole, .lexeme = "?" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        const token = try lexer.nextToken();
        try testing.expectEqual(case.kind, token.kind);
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try testing.expectEqual(.Eof, eof.kind);
    }
}

test "[comment]" {
    const cases = [_]TestCase{
        .{ .source = "# this is a comment", .kind = .Comment, .lexeme = "# this is a comment" },
        .{ .source = "## this is a doc comment", .kind = .DocComment, .lexeme = "## this is a doc comment" },
        .{ .source = "# 你好，世界", .kind = .Comment, .lexeme = "# 你好，世界" },
        .{ .source = "## こんにちは", .kind = .DocComment, .lexeme = "## こんにちは" },
        .{ .source = "# 🚀 👽 💣", .kind = .Comment, .lexeme = "# 🚀 👽 💣" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        const token = try lexer.nextToken();
        try testing.expectEqual(case.kind, token.kind);
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try testing.expectEqual(.Eof, eof.kind);
    }
}

test "[multiline string]" {
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
        var lexer = Lexer.init(case.source, TEST_FILE);

        const token = try lexer.nextToken();
        try testing.expectEqual(case.kind, token.kind);
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try testing.expectEqual(.Eof, eof.kind);
    }
}

test "[multiline string] error.UnterminatedStrLiteral" {
    const invalid_cases = [_][]const u8{
        \\""" This is an
        \\unterminated multiline string with
        \\unicode: 你好, こんにちは
        \\
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try testing.expectError(error.UnterminatedStrLiteral, result);
    }
}

test "[string literal]" {
    const cases = [_]TestCase{
        .{ .source = "\"foo\"", .kind = .LitString, .lexeme = "\"foo\"" },
        .{ .source = "\"1\"", .kind = .LitString, .lexeme = "\"1\"" },
        .{ .source = "\"$\"", .kind = .LitString, .lexeme = "\"$\"" },
        .{ .source = "\"Backslash: \\\\\"", .kind = .LitString, .lexeme = "\"Backslash: \\\\\"" },
        .{ .source = "\"Double quote: \\\"Hello!\\\"\"", .kind = .LitString, .lexeme = "\"Double quote: \\\"Hello!\\\"\"" },
        .{ .source = "\"First line\\nSecond line\"", .kind = .LitString, .lexeme = "\"First line\\nSecond line\"" },
        .{ .source = "\"Column1\\tColumn2\\tColumn3\"", .kind = .LitString, .lexeme = "\"Column1\\tColumn2\\tColumn3\"" },
        .{ .source = "\"Carriage return\\rOverwritten text\"", .kind = .LitString, .lexeme = "\"Carriage return\\rOverwritten text\"" },
        .{ .source = "\"Backspace test: abc\\bdef\"", .kind = .LitString, .lexeme = "\"Backspace test: abc\\bdef\"" },
        .{ .source = "\"Unicode test: \\u1\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u1\"" },
        .{ .source = "\"Unicode test: \\u10\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u10\"" },
        .{ .source = "\"Unicode test: \\u100\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u100\"" },
        .{ .source = "\"Unicode test: \\u1000\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u1000\"" },
        .{ .source = "\"Unicode test: \\u10000\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u10000\"" },
        .{ .source = "\"Unicode test: \\u100000\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u100000\"" },
        .{ .source = "\"Unicode test: \\u10FFFF\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u10FFFF\"" },
        .{ .source = "\"Unicode test: \\u0000\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u0000\"" }, // edge case
        .{ .source = "\"Unicode test: \\u0020\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u0020\"" }, // edge case
        .{ .source = "\"Unicode test: \\u007F\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u007F\"" }, // edge case
        .{ .source = "\"Unicode with extra: \\u1234Hello\"", .kind = .LitString, .lexeme = "\"Unicode with extra: \\u1234Hello\"" },
        .{ .source = "\"✅\"", .kind = .LitString, .lexeme = "\"✅\"" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        const token = try lexer.nextToken();
        try testing.expectEqual(case.kind, token.kind);
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try testing.expectEqual(.Eof, eof.kind);
    }
}

test "[string literal] error.CodePointOutOfRange" {
    const invalid_cases = [_][]const u8{
        "\"\\u110000\"",
        "\"\\uD800\"", // high surrogate
        "\"\\uDFFF\"", // low surrogate
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try testing.expectError(error.CodePointOutOfRange, result);
    }
}

test "[string literal] error.UnrecognizedEscapeSequence" {
    const invalid_cases = [_][]const u8{
        "\"\\q\"",
        "\"\\k\"",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try testing.expectError(error.UnrecognizedEscapeSequence, result);
    }
}

test "[string literal] error.InvalidUnicodeEscapeSequence" {
    const invalid_cases = [_][]const u8{
        "\"unicode missing digits: \\u\"", // Unicode escape needs at least 1 hex digit
        "\"invalid unicode: \\uGHIJ\"", // Unicode escape must only contain hex digits
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try testing.expectError(error.InvalidUnicodeEscapeSequence, result);
    }
}

test "[string literal] error.UnterminatedStrLiteral" {
    const invalid_cases = [_][]const u8{
        "\"no closing quote",
        "\"escape at end\\",
        "\"unicode escape at end\\u123",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try testing.expectError(error.UnterminatedStrLiteral, result);
    }
}

test "[char literal]" {
    const cases = [_]TestCase{
        .{ .source = "'a'", .kind = .LitChar, .lexeme = "'a'" },
        .{ .source = "'1'", .kind = .LitChar, .lexeme = "'1'" },
        .{ .source = "'$'", .kind = .LitChar, .lexeme = "'$'" },
        .{ .source = "'\\n'", .kind = .LitChar, .lexeme = "'\\n'" },
        .{ .source = "'\\t'", .kind = .LitChar, .lexeme = "'\\t'" },
        .{ .source = "'\\r'", .kind = .LitChar, .lexeme = "'\\r'" },
        .{ .source = "'\\''", .kind = .LitChar, .lexeme = "'\\''" },
        .{ .source = "'\\\\'", .kind = .LitChar, .lexeme = "'\\\\'" },
        .{ .source = "'\\u1'", .kind = .LitChar, .lexeme = "'\\u1'" },
        .{ .source = "'\\u10'", .kind = .LitChar, .lexeme = "'\\u10'" },
        .{ .source = "'\\u100'", .kind = .LitChar, .lexeme = "'\\u100'" },
        .{ .source = "'\\u1000'", .kind = .LitChar, .lexeme = "'\\u1000'" },
        .{ .source = "'\\u10000'", .kind = .LitChar, .lexeme = "'\\u10000'" },
        .{ .source = "'\\u100000'", .kind = .LitChar, .lexeme = "'\\u100000'" },
        .{ .source = "'\\u10FFFF'", .kind = .LitChar, .lexeme = "'\\u10FFFF'" },
        .{ .source = "'\\u0000'", .kind = .LitChar, .lexeme = "'\\u0000'" }, // edge case
        .{ .source = "'\\u0020'", .kind = .LitChar, .lexeme = "'\\u0020'" }, // edge case
        .{ .source = "'\\u007F'", .kind = .LitChar, .lexeme = "'\\u007F'" }, // edge case
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        const token = try lexer.nextToken();
        try testing.expectEqual(case.kind, token.kind);
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try testing.expectEqual(.Eof, eof.kind);
    }
}

test "[char literal] error.EmptyCharLiteral" {
    const source = "''";

    var lexer = Lexer.init(source, TEST_FILE);

    const result = lexer.nextToken();
    try testing.expectError(error.EmptyCharLiteral, result);
}

test "[char literal] error.CodePointOutOfRange" {
    const invalid_cases = [_][]const u8{
        "'\\u110000'",
        "'\\uD800'", // high surrogate
        "'\\uDFFF'", // low surrogate
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try testing.expectError(error.CodePointOutOfRange, result);
    }
}

test "[char literal] error.UnrecognizedEscapeSequence" {
    const invalid_cases = [_][]const u8{
        "'\\q'",
        "'\\k'",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try testing.expectError(error.UnrecognizedEscapeSequence, result);
    }
}

test "[char literal] error.MultipleCharsInLiteral" {
    const invalid_cases = [_][]const u8{
        "'ab'",
        "'foo'",
        "'\\n\n'",
        "'a\\n'",
        "'\\na'",
        "'\\u123k'",
        "'\\u000000a'",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try testing.expectError(error.MultipleCharsInLiteral, result);
    }
}

test "[char literal] error.UnterminatedCharLiteral" {
    const invalid_cases = [_][]const u8{
        "'a",
        "'1",
        "'$",
        "'\\n",
        "'\\t",
        "'\\r",
        "'\\'",
        "'\\\\",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try testing.expectError(error.UnterminatedCharLiteral, result);
    }
}

test "[char literal] error.InvalidUnicodeEscapeSequence" {
    const invalid_cases = [_][]const u8{
        "'\\u'",
        "'\\ug'",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try testing.expectError(error.InvalidUnicodeEscapeSequence, result);
    }
}

test "[integer literal]" {
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
        var lexer = Lexer.init(case.source, TEST_FILE);

        const token = try lexer.nextToken();
        try testing.expectEqual(case.kind, token.kind);
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try testing.expectEqual(.Eof, eof.kind);
    }
}

test "[integer literal] error.InvalidIntLiteral" {
    const invalid_cases = [_][]const u8{
        "10__000", // consecutive underscores
        "_1000", // leading underscore
        "1000_", // trailing underscore
        "0x_1F", // underscore after prefix
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try testing.expectError(error.InvalidIntLiteral, result);
    }
}

test "[identifier]" {
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
        .{ .source = "foo?", .kind = .LowerIdent, .lexeme = "foo?" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        const token = try lexer.nextToken();
        try testing.expectEqual(case.kind, token.kind);
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try testing.expectEqual(.Eof, eof.kind);
    }
}

test "[identifier] error.InvalidIdentifier" {
    const invalid_cases = [_][]const u8{
        "_Foo",
        "_Bar",
        "?foo",
        "fo?o",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        try testing.expectError(error.InvalidIdentifier, lexer.nextToken());
    }
}

test "type variant" {
    const source = "type FooBar = | Foo | Bar";

    const expected_tokens = [_]Token{
        Token.init(.KwType, "type", TEST_FILE, 0, 4, 1, 1),
        Token.init(.UpperIdent, "FooBar", TEST_FILE, 5, 11, 1, 6),
        Token.init(.OpAssign, "=", TEST_FILE, 12, 13, 1, 13),
        Token.init(.SymPipe, "|", TEST_FILE, 14, 15, 1, 15),
        Token.init(.UpperIdent, "Foo", TEST_FILE, 16, 19, 1, 17),
        Token.init(.SymPipe, "|", TEST_FILE, 20, 21, 1, 21),
        Token.init(.UpperIdent, "Bar", TEST_FILE, 22, 25, 1, 23),
        Token.init(.Eof, "", TEST_FILE, 25, 25, 1, 26),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        try testing.expectEqual(expected.kind, token.kind);
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);
        try testing.expectEqual(expected.loc.buf.start, token.loc.buf.start);
        try testing.expectEqual(expected.loc.buf.end, token.loc.buf.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "type alias" {
    const source = "type alias Seconds = Int";

    const expected_tokens = [_]Token{
        Token.init(.KwType, "type", TEST_FILE, 0, 4, 1, 1),
        Token.init(.KwAlias, "alias", TEST_FILE, 5, 10, 1, 6),
        Token.init(.UpperIdent, "Seconds", TEST_FILE, 11, 18, 1, 12),
        Token.init(.OpAssign, "=", TEST_FILE, 19, 20, 1, 20),
        Token.init(.UpperIdent, "Int", TEST_FILE, 21, 24, 1, 22),
        Token.init(.Eof, "", TEST_FILE, 24, 24, 1, 25),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        try testing.expectEqual(expected.kind, token.kind);
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);
        try testing.expectEqual(expected.loc.buf.start, token.loc.buf.start);
        try testing.expectEqual(expected.loc.buf.end, token.loc.buf.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "record type" {
    const source = "type FooBar = { foo : Int, bar : String }";

    const expected_tokens = [_]Token{
        Token.init(.KwType, "type", TEST_FILE, 0, 4, 1, 1),
        Token.init(.UpperIdent, "FooBar", TEST_FILE, 5, 11, 1, 6),
        Token.init(.OpAssign, "=", TEST_FILE, 12, 13, 1, 13),
        Token.init(.DelLCurly, "{", TEST_FILE, 14, 15, 1, 15),
        Token.init(.LowerIdent, "foo", TEST_FILE, 16, 19, 1, 17),
        Token.init(.DelColon, ":", TEST_FILE, 20, 21, 1, 21),
        Token.init(.UpperIdent, "Int", TEST_FILE, 22, 25, 1, 23),
        Token.init(.DelComma, ",", TEST_FILE, 25, 26, 1, 26),
        Token.init(.LowerIdent, "bar", TEST_FILE, 27, 30, 1, 28),
        Token.init(.DelColon, ":", TEST_FILE, 31, 32, 1, 32),
        Token.init(.UpperIdent, "String", TEST_FILE, 33, 39, 1, 34),
        Token.init(.DelRCurly, "}", TEST_FILE, 40, 41, 1, 41),
        Token.init(.Eof, "", TEST_FILE, 41, 41, 1, 42),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        try testing.expectEqual(expected.kind, token.kind);
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);
        try testing.expectEqual(expected.loc.buf.start, token.loc.buf.start);
        try testing.expectEqual(expected.loc.buf.end, token.loc.buf.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "module definition" {
    const source = "module Foo exposing (Foo(..), bar) end";

    const expected_tokens = [_]Token{
        Token.init(.KwModule, "module", TEST_FILE, 0, 6, 1, 1),
        Token.init(.UpperIdent, "Foo", TEST_FILE, 7, 10, 1, 8),
        Token.init(.KwExposing, "exposing", TEST_FILE, 11, 19, 1, 12),
        Token.init(.DelLParen, "(", TEST_FILE, 20, 21, 1, 21),
        Token.init(.UpperIdent, "Foo", TEST_FILE, 21, 24, 1, 22),
        Token.init(.DelLParen, "(", TEST_FILE, 24, 25, 1, 25),
        Token.init(.OpDoubleDot, "..", TEST_FILE, 25, 27, 1, 26),
        Token.init(.DelRParen, ")", TEST_FILE, 27, 28, 1, 28),
        Token.init(.DelComma, ",", TEST_FILE, 28, 29, 1, 29),
        Token.init(.LowerIdent, "bar", TEST_FILE, 30, 33, 1, 31),
        Token.init(.DelRParen, ")", TEST_FILE, 33, 34, 1, 34),
        Token.init(.KwEnd, "end", TEST_FILE, 35, 38, 1, 36),
        Token.init(.Eof, "", TEST_FILE, 38, 38, 1, 39),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        try testing.expectEqual(expected.kind, token.kind);
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);
        try testing.expectEqual(expected.loc.buf.start, token.loc.buf.start);
        try testing.expectEqual(expected.loc.buf.end, token.loc.buf.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "top level function definition" {
    const source = "let add : Int -> Int -> Int = \\x y => x + y";

    const expected_tokens = [_]Token{
        Token.init(.KwLet, "let", TEST_FILE, 0, 3, 1, 1),
        Token.init(.LowerIdent, "add", TEST_FILE, 4, 7, 1, 5),
        Token.init(.DelColon, ":", TEST_FILE, 8, 9, 1, 9),
        Token.init(.UpperIdent, "Int", TEST_FILE, 10, 13, 1, 11),
        Token.init(.SymArrowRight, "->", TEST_FILE, 14, 16, 1, 15),
        Token.init(.UpperIdent, "Int", TEST_FILE, 17, 20, 1, 18),
        Token.init(.SymArrowRight, "->", TEST_FILE, 21, 23, 1, 22),
        Token.init(.UpperIdent, "Int", TEST_FILE, 24, 27, 1, 25),
        Token.init(.OpAssign, "=", TEST_FILE, 28, 29, 1, 29),
        Token.init(.OpLambda, "\\", TEST_FILE, 30, 31, 1, 31),
        Token.init(.LowerIdent, "x", TEST_FILE, 31, 32, 1, 32),
        Token.init(.LowerIdent, "y", TEST_FILE, 33, 34, 1, 34),
        Token.init(.SymDoubleArrowRight, "=>", TEST_FILE, 35, 37, 1, 36),
        Token.init(.LowerIdent, "x", TEST_FILE, 38, 39, 1, 39),
        Token.init(.OpIntAdd, "+", TEST_FILE, 40, 41, 1, 41),
        Token.init(.LowerIdent, "y", TEST_FILE, 42, 43, 1, 43),
        Token.init(.Eof, "", TEST_FILE, 43, 43, 1, 44),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        try testing.expectEqual(expected.kind, token.kind);
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);
        try testing.expectEqual(expected.loc.buf.start, token.loc.buf.start);
        try testing.expectEqual(expected.loc.buf.end, token.loc.buf.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "pattern matching" {
    const source = "match x on | Foo => 1 | Bar => 2 _ => 3";

    const expected_tokens = [_]Token{
        Token.init(.KwMatch, "match", TEST_FILE, 0, 5, 1, 1),
        Token.init(.LowerIdent, "x", TEST_FILE, 6, 7, 1, 7),
        Token.init(.KwOn, "on", TEST_FILE, 8, 10, 1, 9),
        Token.init(.SymPipe, "|", TEST_FILE, 11, 12, 1, 12),
        Token.init(.UpperIdent, "Foo", TEST_FILE, 13, 16, 1, 14),
        Token.init(.SymDoubleArrowRight, "=>", TEST_FILE, 17, 19, 1, 18),
        Token.init(.LitInt, "1", TEST_FILE, 20, 21, 1, 21),
        Token.init(.SymPipe, "|", TEST_FILE, 22, 23, 1, 23),
        Token.init(.UpperIdent, "Bar", TEST_FILE, 24, 27, 1, 25),
        Token.init(.SymDoubleArrowRight, "=>", TEST_FILE, 28, 30, 1, 29),
        Token.init(.LitInt, "2", TEST_FILE, 31, 32, 1, 32),
        Token.init(.SymUnderscore, "_", TEST_FILE, 33, 34, 1, 34),
        Token.init(.SymDoubleArrowRight, "=>", TEST_FILE, 35, 37, 1, 36),
        Token.init(.LitInt, "3", TEST_FILE, 38, 39, 1, 39),
        Token.init(.Eof, "", TEST_FILE, 39, 39, 1, 40),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        try testing.expectEqual(expected.kind, token.kind);
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);
        try testing.expectEqual(expected.loc.buf.start, token.loc.buf.start);
        try testing.expectEqual(expected.loc.buf.end, token.loc.buf.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "let/in block" {
    const source = "let x : Int = 42 in";

    const expected_tokens = [_]Token{
        Token.init(.KwLet, "let", TEST_FILE, 0, 3, 1, 1),
        Token.init(.LowerIdent, "x", TEST_FILE, 4, 5, 1, 5),
        Token.init(.DelColon, ":", TEST_FILE, 6, 7, 1, 7),
        Token.init(.UpperIdent, "Int", TEST_FILE, 8, 11, 1, 9),
        Token.init(.OpAssign, "=", TEST_FILE, 12, 13, 1, 13),
        Token.init(.LitInt, "42", TEST_FILE, 14, 16, 1, 15),
        Token.init(.KwIn, "in", TEST_FILE, 17, 19, 1, 18),
        Token.init(.Eof, "", TEST_FILE, 19, 19, 1, 20),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        try testing.expectEqual(expected.kind, token.kind);
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);
        try testing.expectEqual(expected.loc.buf.start, token.loc.buf.start);
        try testing.expectEqual(expected.loc.buf.end, token.loc.buf.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "if/then/else expression" {
    const source = "if x == 1 then True else False";

    const expected_tokens = [_]Token{
        Token.init(.KwIf, "if", TEST_FILE, 0, 2, 1, 1),
        Token.init(.LowerIdent, "x", TEST_FILE, 3, 4, 1, 4),
        Token.init(.OpEquality, "==", TEST_FILE, 5, 7, 1, 6),
        Token.init(.LitInt, "1", TEST_FILE, 8, 9, 1, 9),
        Token.init(.KwThen, "then", TEST_FILE, 10, 14, 1, 11),
        Token.init(.UpperIdent, "True", TEST_FILE, 15, 19, 1, 16),
        Token.init(.KwElse, "else", TEST_FILE, 20, 24, 1, 21),
        Token.init(.UpperIdent, "False", TEST_FILE, 25, 30, 1, 26),
        Token.init(.Eof, "", TEST_FILE, 30, 30, 1, 31),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        try testing.expectEqual(expected.kind, token.kind);
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);
        try testing.expectEqual(expected.loc.buf.start, token.loc.buf.start);
        try testing.expectEqual(expected.loc.buf.end, token.loc.buf.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}
