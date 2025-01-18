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
    SymDoubleArrowRight,
    SymPipe,
    SymUnderscore,

    // Operators
    OpAssign,
    OpComposeLeft,
    OpComposeRight,
    OpCons,
    OpDoubleDot,
    OpLambda,
    OpListConcat,
    OpPipeLeft,
    OpPipeRight,
    OpStrConcat,

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
    /// - `loc`: The complete location information for the token.
    pub fn init(kind: TokenKind, lexeme: []const u8, loc: TokenLoc) Token {
        return Token{
            .kind = kind,
            .lexeme = lexeme,
            .loc = loc,
        };
    }
};

pub const LexerError = error{
    CodePointOutOfRange,
    EmptyCharLiteral,
    InvalidFloatLiteral,
    InvalidIdentifier,
    InvalidIntLiteral,
    InvalidUnicodeEscapeSequence,
    MultipleCharsInLiteral,
    UnrecognizedCharEscapeSequence,
    UnrecognizedStrEscapeSequence,
    UnterminatedCharLiteral,
    UnterminatedStrLiteral,
};

/// A structure representing a lexer which processes source code and generates tokens.
/// It keeps track of the position in the source code, the current line and column, and performs
/// operations to extract tokens from the source.
pub const Lexer = struct {
    source: []const u8,
    loc: TokenLoc,

    /// Initializes a new lexer with the given source code.
    ///
    /// - `source`: The source code to be lexed.
    /// - `filename`: The name of the source file where the token is located.
    pub fn init(source: []const u8, filename: []const u8) Lexer {
        return Lexer{
            .source = source,
            .loc = .{
                .filename = filename,
                .buf = .{ .start = 0, .end = 0 },
                .src = .{ .line = 1, .col = 1 },
            },
        };
    }

    /// Peeks at the next character in the source code without advancing the position.
    ///
    /// Returns `null` if the end of the source is reached.
    fn peek(self: *Lexer) ?u8 {
        if (self.loc.buf.end >= self.source.len) return null;

        return self.source[self.loc.buf.end];
    }

    /// Advances the lexer by one position in the source code.
    /// Updates the current line and column numbers accordingly.
    fn advance(self: *Lexer) void {
        if (self.loc.buf.end >= self.source.len) return;

        if (self.source[self.loc.buf.end] == '\n') {
            self.loc.src.line += 1;
            self.loc.src.col = 1;
        } else {
            self.loc.src.col += 1;
        }

        self.loc.buf.end += 1;
    }

    fn skipWhitespace(self: *Lexer) void {
        while (self.peek()) |c| {
            if (!ascii.isWhitespace(c)) break;

            self.advance();
        }

        self.loc.buf.start = self.loc.buf.end;
    }

    /// Checks if there is an exact match for a keyword starting at a given position.
    ///
    /// - `start`: The starting position of the match in the source code.
    /// - `keyword`: The keyword to check for.
    /// - `kind`: The token kind that corresponds to the keyword.
    ///
    /// Returns a token if there is a match, or `null` if there is no match.
    fn checkExactMatch(
        self: *Lexer,
        start: usize,
        keyword: []const u8,
        kind: TokenKind,
    ) ?Token {
        const len = keyword.len;
        const lexeme = self.source[start..self.loc.buf.end];

        const is_exact_match = self.loc.buf.end - start == len and
            std.mem.eql(u8, lexeme, keyword);

        if (!is_exact_match) return null;

        const end_loc = TokenLoc{
            .filename = self.loc.filename,
            .buf = .{
                .start = start,
                .end = self.loc.buf.end,
            },
            .src = .{
                .line = self.loc.src.line,
                .col = self.loc.src.col - len,
            },
        };

        return Token.init(kind, lexeme, end_loc);
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

    fn handleNumber(self: *Lexer, base: enum { Decimal, Hex, Octal, Binary }) LexerError!Token {
        const offset = if (base == .Decimal) @as(usize, 0) else 2;
        const mark = if (base == .Decimal)
            self.loc.buf.start
        else
            self.loc.buf.end - offset;
        const col_offset = self.loc.src.col - offset;

        if (base != .Decimal) {
            if (self.peek()) |next| {
                if (next == '_') return error.InvalidIntLiteral;
            }
        }

        var found_valid_digit = true;
        var last_was_underscore = false;
        var is_float = false;

        // Handle initial digit sequence
        while (self.peek()) |c| {
            if (base == .Decimal and c == '.') {
                is_float = true;

                self.advance();
                break;
            }

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

        if (last_was_underscore) return error.InvalidIntLiteral;

        if (is_float) {
            // Must have at least one digit after decimal
            var found_fraction_digit = false;
            last_was_underscore = false;

            while (self.peek()) |c| {
                if (ascii.isDigit(c)) {
                    found_fraction_digit = true;
                    last_was_underscore = false;

                    self.advance();
                } else if (c == '_') {
                    if (!found_fraction_digit or last_was_underscore) {
                        return error.InvalidFloatLiteral;
                    }

                    last_was_underscore = true;

                    self.advance();
                } else {
                    break;
                }
            }

            if (!found_fraction_digit or last_was_underscore) {
                return error.InvalidFloatLiteral;
            }

            if (self.peek()) |c| {
                if (c == 'e') {
                    self.advance();

                    if (self.peek()) |next| {
                        if (next == '+' or next == '-') {
                            self.advance();
                        }
                    }

                    // Must have at least one digit in exponent
                    var found_exponent_digit = false;
                    last_was_underscore = false;

                    while (self.peek()) |digit| {
                        if (ascii.isDigit(digit)) {
                            found_exponent_digit = true;
                            last_was_underscore = false;

                            self.advance();
                        } else if (digit == '_') {
                            if (!found_exponent_digit or last_was_underscore) {
                                return error.InvalidFloatLiteral;
                            }

                            last_was_underscore = true;

                            self.advance();
                        } else {
                            break;
                        }
                    }

                    if (!found_exponent_digit or last_was_underscore) {
                        return error.InvalidFloatLiteral;
                    }
                }
            }

            const lexeme = self.source[mark..self.loc.buf.end];

            return Token.init(.LitFloat, lexeme, .{
                .filename = self.loc.filename,
                .buf = .{
                    .start = mark,
                    .end = self.loc.buf.end,
                },
                .src = .{
                    .line = self.loc.src.line,
                    .col = col_offset,
                },
            });
        }

        const lexeme = self.source[mark..self.loc.buf.end];

        return Token.init(.LitInt, lexeme, .{
            .filename = self.loc.filename,
            .buf = .{
                .start = mark,
                .end = self.loc.buf.end,
            },
            .src = .{
                .line = self.loc.src.line,
                .col = col_offset,
            },
        });
    }

    fn isValidUnicodeCodepoint(value: u21) bool {
        // Value must be <= 0x10FFFF and not in the surrogate range (0xD800..0xDFFF)
        if (value > 0x10FFFF) return false;
        if (value >= 0xD800 and value <= 0xDFFF) return false;
        return true;
    }

    fn handleUnicodeEscape(self: *Lexer) LexerError!void {
        self.advance();

        if (self.peek()) |next| {
            if (next != '{') return error.InvalidUnicodeEscapeSequence;

            self.advance();
        } else {
            return error.InvalidUnicodeEscapeSequence;
        }

        // Save the starting position of the hex digits
        const escape_start = self.loc.buf.end;
        const escape_col = self.loc.src.col;

        var unicode_value: u21 = 0;
        var digit_count: usize = 0;
        while (digit_count < 6) : (digit_count += 1) {
            const hex = self.peek() orelse break;
            if (!ascii.isHex(hex)) break;

            const digit_value = hexDigitToValue(hex);
            unicode_value = (unicode_value << 4) | digit_value;

            self.advance();
        }

        if (self.peek()) |next| {
            if (next != '}') {
                self.loc.buf.start = escape_start;
                self.loc.src.col = escape_col;

                return error.InvalidUnicodeEscapeSequence;
            }

            self.advance();
        } else {
            self.loc.buf.start = escape_start;
            self.loc.src.col = escape_col;

            return error.InvalidUnicodeEscapeSequence;
        }

        if (digit_count == 0) {
            self.loc.buf.start = escape_start;
            self.loc.src.col = escape_col;

            return error.InvalidUnicodeEscapeSequence;
        }

        if (!isValidUnicodeCodepoint(unicode_value)) {
            self.loc.buf.start = escape_start - 2;
            self.loc.src.col = escape_col - 2;

            return error.CodePointOutOfRange;
        }
    }

    /// Retrieves the next token from the source code, advancing the lexer.
    ///
    /// - Returns: A token object corresponding to the next recognized token.
    pub fn nextToken(self: *Lexer) LexerError!Token {
        self.skipWhitespace();

        const buf_start = self.loc.buf.start;
        const start_line = self.loc.src.line;
        const start_col = self.loc.src.col;

        const c = self.peek() orelse {
            const end_loc = TokenLoc{
                .filename = self.loc.filename,
                .buf = .{
                    .start = buf_start,
                    .end = self.loc.buf.start,
                },
                .src = .{
                    .line = start_line,
                    .col = start_col,
                },
            };

            return Token.init(.Eof, "", end_loc);
        };

        switch (c) {
            '?' => {
                self.advance();

                if (self.peek() == null) {
                    const end_loc = TokenLoc{
                        .filename = self.loc.filename,
                        .buf = .{
                            .start = buf_start,
                            .end = self.loc.buf.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    };

                    return Token.init(.TypedHole, "?", end_loc);
                }

                return error.InvalidIdentifier;
            },
            '#' => {
                const mark = buf_start;
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
                const lexeme = self.source[mark..self.loc.buf.end];
                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(kind, lexeme, end_loc);
            },
            '"' => {
                const mark = buf_start;
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

                                if (!found_end) return error.UnterminatedStrLiteral;

                                const lexeme = self.source[mark..self.loc.buf.end];
                                const end_loc = TokenLoc{
                                    .filename = self.loc.filename,
                                    .buf = .{
                                        .start = mark,
                                        .end = self.loc.buf.end,
                                    },
                                    .src = .{
                                        .line = start_line,
                                        .col = start_col,
                                    },
                                };

                                return Token.init(.LitMultilineString, lexeme, end_loc);
                            }
                        }

                        const lexeme = self.source[mark..self.loc.buf.end];
                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = mark,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.LitString, lexeme, end_loc);
                    }
                }

                var found_closing_quote = false;

                while (self.peek()) |next| {
                    if (next == '"') {
                        self.advance();

                        found_closing_quote = true;
                        break;
                    }

                    if (next == '\\') {
                        self.advance();

                        const escaped_char = self.peek() orelse return error.UnterminatedStrLiteral;
                        switch (escaped_char) {
                            '\\', '"', 'n', 't', 'r' => self.advance(),
                            'u' => try self.handleUnicodeEscape(),
                            else => return error.UnrecognizedStrEscapeSequence,
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

                if (!found_closing_quote) return error.UnterminatedStrLiteral;

                const lexeme = self.source[mark..self.loc.buf.end];
                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = mark,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.LitString, lexeme, end_loc);
            },
            '\'' => {
                const mark = buf_start;
                self.advance();

                if (self.peek() == '\'') return error.EmptyCharLiteral;

                var char_count: usize = 0;
                var found_closing_quote = false;

                while (self.peek()) |next| {
                    if (next == '\'') {
                        self.advance();

                        found_closing_quote = true;
                        break;
                    }

                    if (next == '\\') {
                        self.advance();

                        char_count += 1;

                        const escaped_char = self.peek() orelse return error.UnterminatedCharLiteral;
                        switch (escaped_char) {
                            '\\', '\'', 'n', 't', 'r' => self.advance(),
                            'u' => try self.handleUnicodeEscape(),
                            else => return error.UnrecognizedCharEscapeSequence,
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
                }

                if (!found_closing_quote) return error.UnterminatedCharLiteral;

                if (char_count > 1) return error.MultipleCharsInLiteral;

                const lexeme = self.source[mark..self.loc.buf.end];
                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = mark,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.LitChar, lexeme, end_loc);
            },
            '+' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '.') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpFloatAdd, "+.", end_loc);
                    }
                }

                if (self.peek()) |next| {
                    if (next == '+') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpListConcat, "++", end_loc);
                    }
                }

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.OpIntAdd, "+", end_loc);
            },
            '-' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '>') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.SymArrowRight, "->", end_loc);
                    }

                    if (next == '.') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpFloatSub, "-.", end_loc);
                    }
                }

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.OpIntSub, "-", end_loc);
            },
            '*' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '*') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpExp, "**", end_loc);
                    }

                    if (next == '.') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpFloatMul, "*.", end_loc);
                    }
                }

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.OpIntMul, "*", end_loc);
            },
            '/' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpNotEqual, "/=", end_loc);
                    }

                    if (next == '.') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpFloatDiv, "/.", end_loc);
                    }
                }

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.OpIntDiv, "/", end_loc);
            },
            '<' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpLessThanEqual, "<=", end_loc);
                    }

                    if (next == '>') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpStrConcat, "<>", end_loc);
                    }

                    if (next == '|') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpPipeLeft, "<|", end_loc);
                    }

                    if (next == '<') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpComposeLeft, "<<", end_loc);
                    }
                }

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.OpLessThan, "<", end_loc);
            },
            '>' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpGreaterThanEqual, ">=", end_loc);
                    }

                    if (next == '>') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpComposeRight, ">>", end_loc);
                    }
                }

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.OpGreaterThan, ">", end_loc);
            },
            '&' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '&') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpLogicalAnd, "&&", end_loc);
                    }
                }

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.Unrecognized, "&", end_loc);
            },
            '|' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '|') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpLogicalOr, "||", end_loc);
                    }

                    if (next == '>') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpPipeRight, "|>", end_loc);
                    }
                }

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.SymPipe, "|", end_loc);
            },
            '\\' => {
                self.advance();

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.OpLambda, "\\", end_loc);
            },
            ':' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == ':') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpCons, "::", end_loc);
                    }
                }

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.DelColon, ":", end_loc);
            },
            ',' => {
                self.advance();

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.DelComma, ",", end_loc);
            },
            '.' => {
                self.advance();

                if (self.peek()) |next| {
                    if (ascii.isDigit(next)) return error.InvalidFloatLiteral;

                    if (next == '.') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpDoubleDot, "..", end_loc);
                    }
                }

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.DelDot, ".", end_loc);
            },
            '{' => {
                self.advance();

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.DelLCurly, "{", end_loc);
            },
            '}' => {
                self.advance();

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.DelRCurly, "}", end_loc);
            },
            '=' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '>') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.SymDoubleArrowRight, "=>", end_loc);
                    }

                    if (next == '=') {
                        self.advance();

                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.OpEquality, "==", end_loc);
                    }
                }

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.OpAssign, "=", end_loc);
            },
            '(' => {
                self.advance();

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.DelLParen, "(", end_loc);
            },
            ')' => {
                self.advance();

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.DelRParen, ")", end_loc);
            },
            '[' => {
                self.advance();

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.DelLBrack, "[", end_loc);
            },
            ']' => {
                self.advance();

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.DelRBrack, "]", end_loc);
            },
            '0' => {
                self.advance();

                if (self.peek()) |next| {
                    switch (next) {
                        'b' => {
                            self.advance();

                            return self.handleNumber(.Binary);
                        },
                        'o' => {
                            self.advance();

                            return self.handleNumber(.Octal);
                        },
                        'x' => {
                            self.advance();

                            return self.handleNumber(.Hex);
                        },
                        else => return self.handleNumber(.Decimal),
                    }
                }

                const lexeme = self.source[buf_start..self.loc.buf.end];
                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.LitInt, lexeme, end_loc);
            },
            '1'...'9' => {
                return self.handleNumber(.Decimal);
            },
            'a'...'z', 'A'...'Z', '_' => {
                if (c == '_') {
                    self.advance();

                    if (self.peek() == null) {
                        const lexeme = self.source[buf_start..self.loc.buf.end];
                        const end_loc = TokenLoc{
                            .filename = self.loc.filename,
                            .buf = .{
                                .start = buf_start,
                                .end = self.loc.buf.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        };

                        return Token.init(.SymUnderscore, lexeme, end_loc);
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

                                const lexeme = self.source[buf_start..self.loc.buf.end];
                                const end_loc = TokenLoc{
                                    .filename = self.loc.filename,
                                    .buf = .{
                                        .start = buf_start,
                                        .end = self.loc.buf.end,
                                    },
                                    .src = .{
                                        .line = start_line,
                                        .col = start_col,
                                    },
                                };

                                return Token.init(.LowerIdent, lexeme, end_loc);
                            },
                            else => {
                                const end_loc = TokenLoc{
                                    .filename = self.loc.filename,
                                    .buf = .{
                                        .start = buf_start,
                                        .end = self.loc.buf.end,
                                    },
                                    .src = .{
                                        .line = start_line,
                                        .col = start_col,
                                    },
                                };

                                return Token.init(.SymUnderscore, "_", end_loc);
                            },
                        }
                    }

                    const end_loc = TokenLoc{
                        .filename = self.loc.filename,
                        .buf = .{
                            .start = buf_start,
                            .end = self.loc.buf.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    };

                    return Token.init(.SymUnderscore, "_", end_loc);
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

                if (self.checkExactMatch(buf_start, "alias", .KwAlias)) |token| return token;
                if (self.checkExactMatch(buf_start, "as", .KwAs)) |token| return token;
                if (self.checkExactMatch(buf_start, "else", .KwElse)) |token| return token;
                if (self.checkExactMatch(buf_start, "end", .KwEnd)) |token| return token;
                if (self.checkExactMatch(buf_start, "exposing", .KwExposing)) |token| return token;
                if (self.checkExactMatch(buf_start, "foreign", .KwForeign)) |token| return token;
                if (self.checkExactMatch(buf_start, "hiding", .KwHiding)) |token| return token;
                if (self.checkExactMatch(buf_start, "if", .KwIf)) |token| return token;
                if (self.checkExactMatch(buf_start, "in", .KwIn)) |token| return token;
                if (self.checkExactMatch(buf_start, "include", .KwInclude)) |token| return token;
                if (self.checkExactMatch(buf_start, "infixl", .KwInfixLeft)) |token| return token;
                if (self.checkExactMatch(buf_start, "infixn", .KwInfixNon)) |token| return token;
                if (self.checkExactMatch(buf_start, "infixr", .KwInfixRight)) |token| return token;
                if (self.checkExactMatch(buf_start, "let", .KwLet)) |token| return token;
                if (self.checkExactMatch(buf_start, "match", .KwMatch)) |token| return token;
                if (self.checkExactMatch(buf_start, "module", .KwModule)) |token| return token;
                if (self.checkExactMatch(buf_start, "on", .KwOn)) |token| return token;
                if (self.checkExactMatch(buf_start, "open", .KwOpen)) |token| return token;
                if (self.checkExactMatch(buf_start, "renaming", .KwRenaming)) |token| return token;
                if (self.checkExactMatch(buf_start, "then", .KwThen)) |token| return token;
                if (self.checkExactMatch(buf_start, "to", .KwTo)) |token| return token;
                if (self.checkExactMatch(buf_start, "type", .KwType)) |token| return token;
                if (self.checkExactMatch(buf_start, "using", .KwUsing)) |token| return token;
                if (self.checkExactMatch(buf_start, "when", .KwWhen)) |token| return token;

                const lexeme = self.source[buf_start..self.loc.buf.end];
                const first_char = lexeme[0];

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                switch (first_char) {
                    'A'...'Z' => return Token.init(.UpperIdent, lexeme, end_loc),
                    'a'...'z', '_' => return Token.init(.LowerIdent, lexeme, end_loc),
                    else => return Token.init(.Unrecognized, lexeme, end_loc),
                }
            },
            else => {
                self.advance();

                const lexeme = self.source[buf_start..self.loc.buf.end];
                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .buf = .{
                        .start = buf_start,
                        .end = self.loc.buf.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                return Token.init(.Unrecognized, lexeme, end_loc);
            },
        }
    }
};

const testing = std.testing;

const TEST_FILE = "test.mox";

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
        .{ .source = "++", .kind = .OpListConcat, .lexeme = "++" },
        .{ .source = "..", .kind = .OpDoubleDot, .lexeme = ".." },
        .{ .source = "::", .kind = .OpCons, .lexeme = "::" },
        .{ .source = "<<", .kind = .OpComposeLeft, .lexeme = "<<" },
        .{ .source = "<>", .kind = .OpStrConcat, .lexeme = "<>" },
        .{ .source = "<|", .kind = .OpPipeLeft, .lexeme = "<|" },
        .{ .source = "=", .kind = .OpAssign, .lexeme = "=" },
        .{ .source = ">>", .kind = .OpComposeRight, .lexeme = ">>" },
        .{ .source = "\\", .kind = .OpLambda, .lexeme = "\\" },
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
        .{ .source = "\"\"", .kind = .LitString, .lexeme = "\"\"" },
        .{ .source = "\"foo\"", .kind = .LitString, .lexeme = "\"foo\"" },
        .{ .source = "\"1\"", .kind = .LitString, .lexeme = "\"1\"" },
        .{ .source = "\"$\"", .kind = .LitString, .lexeme = "\"$\"" },
        .{ .source = "\"Backslash: \\\\\"", .kind = .LitString, .lexeme = "\"Backslash: \\\\\"" },
        .{ .source = "\"Double quote: \\\"Hello!\\\"\"", .kind = .LitString, .lexeme = "\"Double quote: \\\"Hello!\\\"\"" },
        .{ .source = "\"First line\\nSecond line\"", .kind = .LitString, .lexeme = "\"First line\\nSecond line\"" },
        .{ .source = "\"Column1\\tColumn2\\tColumn3\"", .kind = .LitString, .lexeme = "\"Column1\\tColumn2\\tColumn3\"" },
        .{ .source = "\"Carriage return\\rOverwritten text\"", .kind = .LitString, .lexeme = "\"Carriage return\\rOverwritten text\"" },
        .{ .source = "\"Unicode test: \\u{1}\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u{1}\"" },
        .{ .source = "\"Unicode test: \\u{10}\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u{10}\"" },
        .{ .source = "\"Unicode test: \\u{100}\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u{100}\"" },
        .{ .source = "\"Unicode test: \\u{1000}\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u{1000}\"" },
        .{ .source = "\"Unicode test: \\u{10000}\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u{10000}\"" },
        .{ .source = "\"Unicode test: \\u{100000}\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u{100000}\"" },
        .{ .source = "\"Unicode test: \\u{10FFFF}\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u{10FFFF}\"" },
        .{ .source = "\"Unicode test: \\u{0000}\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u{0000}\"" }, // edge case
        .{ .source = "\"Unicode test: \\u{0020}\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u{0020}\"" }, // edge case
        .{ .source = "\"Unicode test: \\u{007F}\"", .kind = .LitString, .lexeme = "\"Unicode test: \\u{007F}\"" }, // edge case
        .{ .source = "\"Unicode with extra: \\u{1234}Hello\"", .kind = .LitString, .lexeme = "\"Unicode with extra: \\u{1234}Hello\"" },
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
        "\"\\u{110000}\"",
        "\"\\u{D800}\"", // high surrogate
        "\"\\u{DFFF}\"", // low surrogate
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try testing.expectError(error.CodePointOutOfRange, result);
    }
}

test "[string literal] error.UnrecognizedStrEscapeSequence" {
    const invalid_cases = [_][]const u8{
        "\"\\q\"",
        "\"\\k\"",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try testing.expectError(error.UnrecognizedStrEscapeSequence, result);
    }
}

test "[string literal] error.InvalidUnicodeEscapeSequence" {
    const invalid_cases = [_][]const u8{
        "\"unicode missing digits: \\u{}\"", // Unicode escape needs at least 1 hex digit
        "\"invalid unicode: \\u{GHIJ}\"", // Unicode escape must only contain hex digits
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
        "\"unicode escape at end\\u{123}",
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
        .{ .source = "'\\u{1}'", .kind = .LitChar, .lexeme = "'\\u{1}'" },
        .{ .source = "'\\u{10}'", .kind = .LitChar, .lexeme = "'\\u{10}'" },
        .{ .source = "'\\u{100}'", .kind = .LitChar, .lexeme = "'\\u{100}'" },
        .{ .source = "'\\u{1000}'", .kind = .LitChar, .lexeme = "'\\u{1000}'" },
        .{ .source = "'\\u{10000}'", .kind = .LitChar, .lexeme = "'\\u{10000}'" },
        .{ .source = "'\\u{100000}'", .kind = .LitChar, .lexeme = "'\\u{100000}'" },
        .{ .source = "'\\u{10FFFF}'", .kind = .LitChar, .lexeme = "'\\u{10FFFF}'" },
        .{ .source = "'\\u{0000}'", .kind = .LitChar, .lexeme = "'\\u{0000}'" }, // edge case
        .{ .source = "'\\u{0020}'", .kind = .LitChar, .lexeme = "'\\u{0020}'" }, // edge case
        .{ .source = "'\\u{007F}'", .kind = .LitChar, .lexeme = "'\\u{007F}'" }, // edge case
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
        "'\\u{110000}'",
        "'\\u{D800}'", // high surrogate
        "'\\u{DFFF}'", // low surrogate
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try testing.expectError(error.CodePointOutOfRange, result);
    }
}

test "[char literal] error.UnrecognizedCharEscapeSequence" {
    const invalid_cases = [_][]const u8{
        "'\\q'",
        "'\\k'",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try testing.expectError(error.UnrecognizedCharEscapeSequence, result);
    }
}

test "[char literal] error.MultipleCharsInLiteral" {
    const invalid_cases = [_][]const u8{
        "'ab'",
        "'foo'",
        "'\\n\n'",
        "'a\\n'",
        "'\\na'",
        "'\\u{123}k'",
        "'\\u{000000}a'",
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
        "'\\u{}'",
        "'\\u{g}'",
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
        "10__000.0", // fail before we even know it's a float
        "_1000.0", // fail before we even know it's a float
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try testing.expectError(error.InvalidIntLiteral, result);
    }
}

test "[float literal]" {
    const cases = [_]TestCase{
        .{ .source = "42.0", .kind = .LitFloat, .lexeme = "42.0" },
        .{ .source = "42_000_000.0", .kind = .LitFloat, .lexeme = "42_000_000.0" },
        .{ .source = "0.5", .kind = .LitFloat, .lexeme = "0.5" },
        .{ .source = "1.23e3", .kind = .LitFloat, .lexeme = "1.23e3" },
        .{ .source = "4.56e-2", .kind = .LitFloat, .lexeme = "4.56e-2" },
        .{ .source = "3.141_592", .kind = .LitFloat, .lexeme = "3.141_592" },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        const token = try lexer.nextToken();
        try std.testing.expectEqual(case.kind, token.kind);
        try std.testing.expectEqualStrings(case.lexeme, token.lexeme);

        const eof = try lexer.nextToken();
        try std.testing.expectEqual(TokenKind.Eof, eof.kind);
    }
}

test "[float literal] error.InvalidFloat" {
    const invalid_cases = [_][]const u8{
        "1000._", // trailing underscore
        ".5e3", // must begin with a digit
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        const result = lexer.nextToken();
        try std.testing.expectError(error.InvalidFloatLiteral, result);
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
        Token.init(.KwType, "type", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 0, .end = 4 },
            .src = .{ .line = 1, .col = 1 },
        }),
        Token.init(.UpperIdent, "FooBar", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 5, .end = 11 },
            .src = .{ .line = 1, .col = 6 },
        }),
        Token.init(.OpAssign, "=", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 12, .end = 13 },
            .src = .{ .line = 1, .col = 13 },
        }),
        Token.init(.SymPipe, "|", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 14, .end = 15 },
            .src = .{ .line = 1, .col = 15 },
        }),
        Token.init(.UpperIdent, "Foo", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 16, .end = 19 },
            .src = .{ .line = 1, .col = 17 },
        }),
        Token.init(.SymPipe, "|", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 20, .end = 21 },
            .src = .{ .line = 1, .col = 21 },
        }),
        Token.init(.UpperIdent, "Bar", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 22, .end = 25 },
            .src = .{ .line = 1, .col = 23 },
        }),
        Token.init(.Eof, "", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 25, .end = 25 },
            .src = .{ .line = 1, .col = 26 },
        }),
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
        Token.init(.KwType, "type", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 0, .end = 4 },
            .src = .{ .line = 1, .col = 1 },
        }),
        Token.init(.KwAlias, "alias", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 5, .end = 10 },
            .src = .{ .line = 1, .col = 6 },
        }),
        Token.init(.UpperIdent, "Seconds", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 11, .end = 18 },
            .src = .{ .line = 1, .col = 12 },
        }),
        Token.init(.OpAssign, "=", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 19, .end = 20 },
            .src = .{ .line = 1, .col = 20 },
        }),
        Token.init(.UpperIdent, "Int", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 21, .end = 24 },
            .src = .{ .line = 1, .col = 22 },
        }),
        Token.init(.Eof, "", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 24, .end = 24 },
            .src = .{ .line = 1, .col = 25 },
        }),
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
        Token.init(.KwType, "type", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 0, .end = 4 },
            .src = .{ .line = 1, .col = 1 },
        }),
        Token.init(.UpperIdent, "FooBar", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 5, .end = 11 },
            .src = .{ .line = 1, .col = 6 },
        }),
        Token.init(.OpAssign, "=", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 12, .end = 13 },
            .src = .{ .line = 1, .col = 13 },
        }),
        Token.init(.DelLCurly, "{", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 14, .end = 15 },
            .src = .{ .line = 1, .col = 15 },
        }),
        Token.init(.LowerIdent, "foo", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 16, .end = 19 },
            .src = .{ .line = 1, .col = 17 },
        }),
        Token.init(.DelColon, ":", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 20, .end = 21 },
            .src = .{ .line = 1, .col = 21 },
        }),
        Token.init(.UpperIdent, "Int", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 22, .end = 25 },
            .src = .{ .line = 1, .col = 23 },
        }),
        Token.init(.DelComma, ",", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 25, .end = 26 },
            .src = .{ .line = 1, .col = 26 },
        }),
        Token.init(.LowerIdent, "bar", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 27, .end = 30 },
            .src = .{ .line = 1, .col = 28 },
        }),
        Token.init(.DelColon, ":", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 31, .end = 32 },
            .src = .{ .line = 1, .col = 32 },
        }),
        Token.init(.UpperIdent, "String", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 33, .end = 39 },
            .src = .{ .line = 1, .col = 34 },
        }),
        Token.init(.DelRCurly, "}", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 40, .end = 41 },
            .src = .{ .line = 1, .col = 41 },
        }),
        Token.init(.Eof, "", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 41, .end = 41 },
            .src = .{ .line = 1, .col = 42 },
        }),
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
        Token.init(.KwModule, "module", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 0, .end = 6 },
            .src = .{ .line = 1, .col = 1 },
        }),
        Token.init(.UpperIdent, "Foo", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 7, .end = 10 },
            .src = .{ .line = 1, .col = 8 },
        }),
        Token.init(.KwExposing, "exposing", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 11, .end = 19 },
            .src = .{ .line = 1, .col = 12 },
        }),
        Token.init(.DelLParen, "(", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 20, .end = 21 },
            .src = .{ .line = 1, .col = 21 },
        }),
        Token.init(.UpperIdent, "Foo", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 21, .end = 24 },
            .src = .{ .line = 1, .col = 22 },
        }),
        Token.init(.DelLParen, "(", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 24, .end = 25 },
            .src = .{ .line = 1, .col = 25 },
        }),
        Token.init(.OpDoubleDot, "..", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 25, .end = 27 },
            .src = .{ .line = 1, .col = 26 },
        }),
        Token.init(.DelRParen, ")", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 27, .end = 28 },
            .src = .{ .line = 1, .col = 28 },
        }),
        Token.init(.DelComma, ",", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 28, .end = 29 },
            .src = .{ .line = 1, .col = 29 },
        }),
        Token.init(.LowerIdent, "bar", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 30, .end = 33 },
            .src = .{ .line = 1, .col = 31 },
        }),
        Token.init(.DelRParen, ")", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 33, .end = 34 },
            .src = .{ .line = 1, .col = 34 },
        }),
        Token.init(.KwEnd, "end", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 35, .end = 38 },
            .src = .{ .line = 1, .col = 36 },
        }),
        Token.init(.Eof, "", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 38, .end = 38 },
            .src = .{ .line = 1, .col = 39 },
        }),
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
        Token.init(.KwLet, "let", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 0, .end = 3 },
            .src = .{ .line = 1, .col = 1 },
        }),
        Token.init(.LowerIdent, "add", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 4, .end = 7 },
            .src = .{ .line = 1, .col = 5 },
        }),
        Token.init(.DelColon, ":", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 8, .end = 9 },
            .src = .{ .line = 1, .col = 9 },
        }),
        Token.init(.UpperIdent, "Int", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 10, .end = 13 },
            .src = .{ .line = 1, .col = 11 },
        }),
        Token.init(.SymArrowRight, "->", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 14, .end = 16 },
            .src = .{ .line = 1, .col = 15 },
        }),
        Token.init(.UpperIdent, "Int", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 17, .end = 20 },
            .src = .{ .line = 1, .col = 18 },
        }),
        Token.init(.SymArrowRight, "->", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 21, .end = 23 },
            .src = .{ .line = 1, .col = 22 },
        }),
        Token.init(.UpperIdent, "Int", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 24, .end = 27 },
            .src = .{ .line = 1, .col = 25 },
        }),
        Token.init(.OpAssign, "=", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 28, .end = 29 },
            .src = .{ .line = 1, .col = 29 },
        }),
        Token.init(.OpLambda, "\\", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 30, .end = 31 },
            .src = .{ .line = 1, .col = 31 },
        }),
        Token.init(.LowerIdent, "x", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 31, .end = 32 },
            .src = .{ .line = 1, .col = 32 },
        }),
        Token.init(.LowerIdent, "y", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 33, .end = 34 },
            .src = .{ .line = 1, .col = 34 },
        }),
        Token.init(.SymDoubleArrowRight, "=>", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 35, .end = 37 },
            .src = .{ .line = 1, .col = 36 },
        }),
        Token.init(.LowerIdent, "x", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 38, .end = 39 },
            .src = .{ .line = 1, .col = 39 },
        }),
        Token.init(.OpIntAdd, "+", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 40, .end = 41 },
            .src = .{ .line = 1, .col = 41 },
        }),
        Token.init(.LowerIdent, "y", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 42, .end = 43 },
            .src = .{ .line = 1, .col = 43 },
        }),
        Token.init(.Eof, "", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 43, .end = 43 },
            .src = .{ .line = 1, .col = 44 },
        }),
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
        Token.init(.KwMatch, "match", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 0, .end = 5 },
            .src = .{ .line = 1, .col = 1 },
        }),
        Token.init(.LowerIdent, "x", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 6, .end = 7 },
            .src = .{ .line = 1, .col = 7 },
        }),
        Token.init(.KwOn, "on", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 8, .end = 10 },
            .src = .{ .line = 1, .col = 9 },
        }),
        Token.init(.SymPipe, "|", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 11, .end = 12 },
            .src = .{ .line = 1, .col = 12 },
        }),
        Token.init(.UpperIdent, "Foo", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 13, .end = 16 },
            .src = .{ .line = 1, .col = 14 },
        }),
        Token.init(.SymDoubleArrowRight, "=>", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 17, .end = 19 },
            .src = .{ .line = 1, .col = 18 },
        }),
        Token.init(.LitInt, "1", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 20, .end = 21 },
            .src = .{ .line = 1, .col = 21 },
        }),
        Token.init(.SymPipe, "|", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 22, .end = 23 },
            .src = .{ .line = 1, .col = 23 },
        }),
        Token.init(.UpperIdent, "Bar", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 24, .end = 27 },
            .src = .{ .line = 1, .col = 25 },
        }),
        Token.init(.SymDoubleArrowRight, "=>", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 28, .end = 30 },
            .src = .{ .line = 1, .col = 29 },
        }),
        Token.init(.LitInt, "2", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 31, .end = 32 },
            .src = .{ .line = 1, .col = 32 },
        }),
        Token.init(.SymUnderscore, "_", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 33, .end = 34 },
            .src = .{ .line = 1, .col = 34 },
        }),
        Token.init(.SymDoubleArrowRight, "=>", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 35, .end = 37 },
            .src = .{ .line = 1, .col = 36 },
        }),
        Token.init(.LitInt, "3", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 38, .end = 39 },
            .src = .{ .line = 1, .col = 39 },
        }),
        Token.init(.Eof, "", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 39, .end = 39 },
            .src = .{ .line = 1, .col = 40 },
        }),
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
        Token.init(.KwLet, "let", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 0, .end = 3 },
            .src = .{ .line = 1, .col = 1 },
        }),
        Token.init(.LowerIdent, "x", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 4, .end = 5 },
            .src = .{ .line = 1, .col = 5 },
        }),
        Token.init(.DelColon, ":", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 6, .end = 7 },
            .src = .{ .line = 1, .col = 7 },
        }),
        Token.init(.UpperIdent, "Int", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 8, .end = 11 },
            .src = .{ .line = 1, .col = 9 },
        }),
        Token.init(.OpAssign, "=", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 12, .end = 13 },
            .src = .{ .line = 1, .col = 13 },
        }),
        Token.init(.LitInt, "42", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 14, .end = 16 },
            .src = .{ .line = 1, .col = 15 },
        }),
        Token.init(.KwIn, "in", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 17, .end = 19 },
            .src = .{ .line = 1, .col = 18 },
        }),
        Token.init(.Eof, "", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 19, .end = 19 },
            .src = .{ .line = 1, .col = 20 },
        }),
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
        Token.init(.KwIf, "if", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 0, .end = 2 },
            .src = .{ .line = 1, .col = 1 },
        }),
        Token.init(.LowerIdent, "x", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 3, .end = 4 },
            .src = .{ .line = 1, .col = 4 },
        }),
        Token.init(.OpEquality, "==", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 5, .end = 7 },
            .src = .{ .line = 1, .col = 6 },
        }),
        Token.init(.LitInt, "1", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 8, .end = 9 },
            .src = .{ .line = 1, .col = 9 },
        }),
        Token.init(.KwThen, "then", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 10, .end = 14 },
            .src = .{ .line = 1, .col = 11 },
        }),
        Token.init(.UpperIdent, "True", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 15, .end = 19 },
            .src = .{ .line = 1, .col = 16 },
        }),
        Token.init(.KwElse, "else", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 20, .end = 24 },
            .src = .{ .line = 1, .col = 21 },
        }),
        Token.init(.UpperIdent, "False", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 25, .end = 30 },
            .src = .{ .line = 1, .col = 26 },
        }),
        Token.init(.Eof, "", .{
            .filename = TEST_FILE,
            .buf = .{ .start = 30, .end = 30 },
            .src = .{ .line = 1, .col = 31 },
        }),
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
