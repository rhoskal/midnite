const std = @import("std");

const ascii = std.ascii;
const assert = std.debug.assert;

pub const KeywordKind = enum {
    Alias,
    As,
    Else,
    End,
    Exposing,
    Foreign,
    Hiding,
    If,
    In,
    Include,
    InfixLeft,
    InfixNon,
    InfixRight,
    Let,
    Match,
    Module,
    On,
    Open,
    Renaming,
    Then,
    To,
    Type,
    Using,
    When,
};

pub const OperatorKind = enum {
    // Arithmetic
    Exp,
    FloatAdd,
    FloatDiv,
    FloatMul,
    FloatSub,
    IntAdd,
    IntDiv,
    IntMul,
    IntSub,

    // Comparison (Relational)
    Equality,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    NotEqual,

    // Logical (Boolean)
    LogicalAnd,
    LogicalOr,

    // Other
    ComposeLeft,
    ComposeRight,
    Cons,
    Equal,
    Expand,
    Lambda,
    ListConcat,
    PipeLeft,
    PipeRight,
    StrConcat,
};

pub const DelimiterKind = enum {
    Colon,
    Comma,
    Dot,
    LeftBrace,
    LeftBracket,
    LeftParen,
    RightBrace,
    RightBracket,
    RightParen,
};

pub const LiteralKind = enum {
    Char,
    Float,
    Int,
    MultilineString,
    String,
};

pub const IdentifierKind = enum {
    Lower,
    Upper,
};

pub const SymbolKind = enum {
    ArrowRight,
    DoubleArrowRight,
    Pipe,
    Underscore,
};

pub const CommentKind = enum {
    Doc,
    Regular,
};

pub const SpecialKind = enum {
    Eof,
    Hole,
    Unrecognized,
};

/// A union enum representing the different kinds of tokens in the language.
/// Each category has its own enum defining the specific token types.
pub const TokenKind = union(enum) {
    comment: CommentKind,
    delimiter: DelimiterKind,
    identifier: IdentifierKind,
    keyword: KeywordKind,
    literal: LiteralKind,
    operator: OperatorKind,
    special: SpecialKind,
    symbol: SymbolKind,

    pub fn format(
        kind: TokenKind,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (kind) {
            .comment => |v| try writer.print(".comment = {s}", .{@tagName(v)}),
            .delimiter => |v| try writer.print(".delimiter = {s}", .{@tagName(v)}),
            .identifier => |v| try writer.print(".identifier = {s}", .{@tagName(v)}),
            .keyword => |v| try writer.print(".keyword = {s}", .{@tagName(v)}),
            .literal => |v| try writer.print(".literal = {s}", .{@tagName(v)}),
            .operator => |v| try writer.print(".operator = {s}", .{@tagName(v)}),
            .special => |v| try writer.print(".special = {s}", .{@tagName(v)}),
            .symbol => |v| try writer.print(".symbol = {s}", .{@tagName(v)}),
        }
    }
};

/// A structure representing raw buffer locations.
pub const Span = struct {
    /// The start index of the lexeme in the source buffer (inclusive).
    start: usize,

    /// The end index of the lexeme in the source buffer (exclusive).
    end: usize,
};

/// A human-readable line/column position.
pub const SourceLoc = struct {
    /// The line number in the source file (1-based).
    line: usize,

    /// The column number in the source file (1-based).
    col: usize,
};

/// Combines both raw and human-readable locations.
/// This dual representation enables both efficient string slicing (Span)
/// and meaningful error reporting (SourceLoc) without conversion overhead.
pub const TokenLoc = struct {
    /// The name of the source file where the token originated.
    filename: []const u8,

    /// The raw buffer location of the token.
    span: Span,

    /// The human-readable source location of the token.
    src: SourceLoc,
};

/// A structure representing a token with a specific kind, lexeme, and position in the source code.
pub const Token = struct {
    /// The kind of token (e.g., literal, keyword)
    kind: TokenKind,

    /// The string representation of the token
    lexeme: []const u8,

    /// The complete location information for the token
    loc: TokenLoc,

    pub fn init(kind: TokenKind, lexeme: []const u8, loc: TokenLoc) Token {
        // Assert location spans are valid
        assert(loc.span.start <= loc.span.end);
        // Assert line numbers are 1-based
        assert(loc.src.line > 0);
        // Assert column numbers are 1-based
        assert(loc.src.col > 0);
        // Assert lexeme length matches span
        assert(lexeme.len == loc.span.end - loc.span.start);

        return .{
            .kind = kind,
            .lexeme = lexeme,
            .loc = loc,
        };
    }

    pub fn format(
        token: Token,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("Token: {s} ({s})", .{ token.lexeme, token.kind });
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

/// A lexer that processes source code into a sequence of tokens.
///
/// The lexer is implemented as a deterministic finite automaton (DFA) with
/// single-character lookahead. This design choice means:
/// 1. Each state transition depends only on the current state and next character
/// 2. The lexer can make decisions with just one character of lookahead (peek)
/// 3. Token boundaries are determined without backtracking
/// 4. Error reporting is precise and immediate
///
/// This approach provides a good balance of simplicity, performance, and error
/// handling capability while keeping memory usage constant.
pub const Lexer = struct {
    /// The source code to be lexed.
    source: []const u8,

    /// The name of the source file where the token is located.
    loc: TokenLoc,

    pub fn init(source: []const u8, filename: []const u8) Lexer {
        // Filename must not be empty
        assert(filename.len > 0);

        return .{
            .source = source,
            .loc = .{
                .filename = filename,
                .span = .{ .start = 0, .end = 0 },
                .src = .{ .line = 1, .col = 1 },
            },
        };
    }

    /// Retrieves and returns the next token from the source code, advancing the lexer position.
    pub fn nextToken(self: *Lexer) LexerError!Token {
        // Assert our position tracking is valid
        assert(self.loc.span.end >= self.loc.span.start);
        assert(self.loc.span.start <= self.source.len);

        self.skipWhitespace();

        // Verify skipWhitespace properly synchronized positions
        assert(self.loc.span.start == self.loc.span.end);

        const span_start = self.loc.span.start;
        const start_line = self.loc.src.line;
        const start_col = self.loc.src.col;

        const c = self.peek() orelse {
            return Token.init(
                .{ .special = .Eof },
                "",
                TokenLoc{
                    .filename = self.loc.filename,
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.start,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                },
            );
        };

        switch (c) {
            '?' => {
                self.advance();

                if (self.peek() == null) {
                    return Token.init(
                        .{ .special = .Hole },
                        "?",
                        TokenLoc{
                            .filename = self.loc.filename,
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        },
                    );
                } else {
                    return error.InvalidIdentifier;
                }
            },
            '#' => {
                const position_start = span_start;
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

                const kind = if (is_doc) TokenKind{ .comment = .Doc } else TokenKind{ .comment = .Regular };
                const lexeme = self.source[position_start..self.loc.span.end];

                return Token.init(
                    kind,
                    lexeme,
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            '"' => {
                self.advance();

                return self.handleStringLiteral(span_start);
            },
            '\'' => {
                const position_start = span_start;
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

                        if (self.peek()) |escaped_char| {
                            switch (escaped_char) {
                                '\\', '\'', 'n', 't', 'r' => self.advance(),
                                'u' => try self.handleUnicodeEscape(),
                                else => return error.UnrecognizedCharEscapeSequence,
                            }
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

                if (found_closing_quote) {
                    if (char_count == 1) {
                        const lexeme = self.source[position_start..self.loc.span.end];

                        return Token.init(
                            .{ .literal = .Char },
                            lexeme,
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = position_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    } else {
                        return error.MultipleCharsInLiteral;
                    }
                } else {
                    return error.UnterminatedCharLiteral;
                }
            },
            '+' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .FloatAdd },
                            "+.",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }
                }

                if (self.peek()) |next| {
                    if (next == '+') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .ListConcat },
                            "++",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }
                }

                return Token.init(
                    .{ .operator = .IntAdd },
                    "+",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            '-' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '>') {
                        self.advance();

                        return Token.init(
                            .{ .symbol = .ArrowRight },
                            "->",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }

                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .FloatSub },
                            "-.",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }
                }

                return Token.init(
                    .{ .operator = .IntSub },
                    "-",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            '*' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '*') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .Exp },
                            "**",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }

                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .FloatMul },
                            "*.",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }
                }

                return Token.init(
                    .{ .operator = .IntMul },
                    "*",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            '/' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .NotEqual },
                            "/=",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }

                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .FloatDiv },
                            "/.",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }
                }

                return Token.init(
                    .{ .operator = .IntDiv },
                    "/",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            '<' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .LessThanEqual },
                            "<=",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }

                    if (next == '>') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .StrConcat },
                            "<>",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }

                    if (next == '|') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .PipeLeft },
                            "<|",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }

                    if (next == '<') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .ComposeLeft },
                            "<<",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }
                }

                return Token.init(
                    .{ .operator = .LessThan },
                    "<",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            '>' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '=') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .GreaterThanEqual },
                            ">=",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }

                    if (next == '>') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .ComposeRight },
                            ">>",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }
                }

                return Token.init(
                    .{ .operator = .GreaterThan },
                    ">",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            '&' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '&') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .LogicalAnd },
                            "&&",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }
                }

                return Token.init(
                    .{ .special = .Unrecognized },
                    "&",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            '|' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '|') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .LogicalOr },
                            "||",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }

                    if (next == '>') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .PipeRight },
                            "|>",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }
                }

                return Token.init(
                    .{ .symbol = .Pipe },
                    "|",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            '\\' => {
                self.advance();

                return Token.init(
                    .{ .operator = .Lambda },
                    "\\",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            ':' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == ':') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .Cons },
                            "::",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }
                }

                return Token.init(
                    .{ .delimiter = .Colon },
                    ":",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            ',' => {
                self.advance();

                return Token.init(
                    .{ .delimiter = .Comma },
                    ",",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            '.' => {
                self.advance();

                if (self.peek()) |next| {
                    if (ascii.isDigit(next)) return error.InvalidFloatLiteral;

                    if (next == '.') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .Expand },
                            "..",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }
                }

                return Token.init(
                    .{ .delimiter = .Dot },
                    ".",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            '{' => {
                self.advance();

                return Token.init(
                    .{ .delimiter = .LeftBrace },
                    "{",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            '}' => {
                self.advance();

                return Token.init(
                    .{ .delimiter = .RightBrace },
                    "}",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            '=' => {
                self.advance();

                if (self.peek()) |next| {
                    if (next == '>') {
                        self.advance();

                        return Token.init(
                            .{ .symbol = .DoubleArrowRight },
                            "=>",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }

                    if (next == '=') {
                        self.advance();

                        return Token.init(
                            .{ .operator = .Equality },
                            "==",
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
                        );
                    }
                }

                return Token.init(
                    .{ .operator = .Equal },
                    "=",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            '(' => {
                self.advance();

                return Token.init(
                    .{ .delimiter = .LeftParen },
                    "(",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            ')' => {
                self.advance();

                return Token.init(
                    .{ .delimiter = .RightParen },
                    ")",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            '[' => {
                self.advance();

                return Token.init(
                    .{ .delimiter = .LeftBracket },
                    "[",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            ']' => {
                self.advance();

                return Token.init(
                    .{ .delimiter = .RightBracket },
                    "]",
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
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

                const lexeme = self.source[span_start..self.loc.span.end];

                return Token.init(
                    .{ .literal = .Int },
                    lexeme,
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
            '1'...'9' => {
                return self.handleNumber(.Decimal);
            },
            'a'...'z', 'A'...'Z', '_' => {
                if (c == '_') {
                    self.advance();

                    if (self.peek() == null) {
                        const lexeme = self.source[span_start..self.loc.span.end];

                        return Token.init(
                            .{ .symbol = .Underscore },
                            lexeme,
                            TokenLoc{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            },
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

                                const lexeme = self.source[span_start..self.loc.span.end];

                                return Token.init(
                                    .{ .identifier = .Lower },
                                    lexeme,
                                    TokenLoc{
                                        .filename = self.loc.filename,
                                        .span = .{
                                            .start = span_start,
                                            .end = self.loc.span.end,
                                        },
                                        .src = .{
                                            .line = start_line,
                                            .col = start_col,
                                        },
                                    },
                                );
                            },
                            else => {
                                return Token.init(
                                    .{ .symbol = .Underscore },
                                    "_",
                                    TokenLoc{
                                        .filename = self.loc.filename,
                                        .span = .{
                                            .start = span_start,
                                            .end = self.loc.span.end,
                                        },
                                        .src = .{
                                            .line = start_line,
                                            .col = start_col,
                                        },
                                    },
                                );
                            },
                        }
                    }

                    return Token.init(
                        .{ .symbol = .Underscore },
                        "_",
                        TokenLoc{
                            .filename = self.loc.filename,
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        },
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

                if (self.checkExactMatch(span_start, "alias", .{ .keyword = .Alias })) |token| return token;
                if (self.checkExactMatch(span_start, "as", .{ .keyword = .As })) |token| return token;
                if (self.checkExactMatch(span_start, "else", .{ .keyword = .Else })) |token| return token;
                if (self.checkExactMatch(span_start, "end", .{ .keyword = .End })) |token| return token;
                if (self.checkExactMatch(span_start, "exposing", .{ .keyword = .Exposing })) |token| return token;
                if (self.checkExactMatch(span_start, "foreign", .{ .keyword = .Foreign })) |token| return token;
                if (self.checkExactMatch(span_start, "hiding", .{ .keyword = .Hiding })) |token| return token;
                if (self.checkExactMatch(span_start, "if", .{ .keyword = .If })) |token| return token;
                if (self.checkExactMatch(span_start, "in", .{ .keyword = .In })) |token| return token;
                if (self.checkExactMatch(span_start, "include", .{ .keyword = .Include })) |token| return token;
                if (self.checkExactMatch(span_start, "infixl", .{ .keyword = .InfixLeft })) |token| return token;
                if (self.checkExactMatch(span_start, "infixn", .{ .keyword = .InfixNon })) |token| return token;
                if (self.checkExactMatch(span_start, "infixr", .{ .keyword = .InfixRight })) |token| return token;
                if (self.checkExactMatch(span_start, "let", .{ .keyword = .Let })) |token| return token;
                if (self.checkExactMatch(span_start, "match", .{ .keyword = .Match })) |token| return token;
                if (self.checkExactMatch(span_start, "module", .{ .keyword = .Module })) |token| return token;
                if (self.checkExactMatch(span_start, "on", .{ .keyword = .On })) |token| return token;
                if (self.checkExactMatch(span_start, "open", .{ .keyword = .Open })) |token| return token;
                if (self.checkExactMatch(span_start, "renaming", .{ .keyword = .Renaming })) |token| return token;
                if (self.checkExactMatch(span_start, "then", .{ .keyword = .Then })) |token| return token;
                if (self.checkExactMatch(span_start, "to", .{ .keyword = .To })) |token| return token;
                if (self.checkExactMatch(span_start, "type", .{ .keyword = .Type })) |token| return token;
                if (self.checkExactMatch(span_start, "using", .{ .keyword = .Using })) |token| return token;
                if (self.checkExactMatch(span_start, "when", .{ .keyword = .When })) |token| return token;

                const lexeme = self.source[span_start..self.loc.span.end];
                const first_char = lexeme[0];

                const end_loc = TokenLoc{
                    .filename = self.loc.filename,
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                };

                switch (first_char) {
                    'A'...'Z' => return Token.init(.{ .identifier = .Upper }, lexeme, end_loc),
                    'a'...'z', '_' => return Token.init(.{ .identifier = .Lower }, lexeme, end_loc),
                    else => return Token.init(.{ .special = .Unrecognized }, lexeme, end_loc),
                }
            },
            else => {
                self.advance();

                const lexeme = self.source[span_start..self.loc.span.end];

                return Token.init(
                    .{ .special = .Unrecognized },
                    lexeme,
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
            },
        }
    }

    /// Peeks at the next character in the source code without advancing the position.
    fn peek(self: *Lexer) ?u8 {
        // Assert our internal state is valid
        assert(self.loc.span.end >= self.loc.span.start);
        assert(self.loc.span.start <= self.source.len);

        if (self.loc.span.end < self.source.len) {
            return self.source[self.loc.span.end];
        } else {
            return null;
        }
    }

    /// Advances the lexer by one position in the source code.
    /// Updates the current line and column numbers accordingly.
    fn advance(self: *Lexer) void {
        // Assert we haven't reached the end of input
        assert(self.loc.span.end < self.source.len);
        // Assert our position tracking is valid
        assert(self.loc.src.line > 0);
        assert(self.loc.src.col > 0);
        // Spans remain ordered
        assert(self.loc.span.start <= self.loc.span.end);

        if (self.source[self.loc.span.end] == '\n') {
            self.loc.src.line += 1;
            self.loc.src.col = 1;
        } else {
            self.loc.src.col += 1;
        }

        self.loc.span.end += 1;

        assert(self.loc.span.end <= self.source.len);
        assert(self.loc.span.end > self.loc.span.start);
    }

    /// Advances past whitespace and synchronizes span positions.
    fn skipWhitespace(self: *Lexer) void {
        const initial_pos = self.loc.span.end;

        while (self.peek()) |c| {
            if (ascii.isWhitespace(c)) {
                self.advance();
            } else {
                break;
            }
        }

        self.loc.span.start = self.loc.span.end;

        // Assert we've either moved forward or stayed in place
        assert(self.loc.span.end >= initial_pos);
        // Assert our start/end positions are synchronized after skipping
        assert(self.loc.span.start == self.loc.span.end);
    }

    /// Checks if there is an exact match for a keyword starting at a given position.
    ///
    /// - `start`: The starting position of the match in the source code.
    /// - `keyword`: The keyword to check for.
    /// - `kind`: The token kind that corresponds to the keyword.
    fn checkExactMatch(
        self: *Lexer,
        start: usize,
        keyword: []const u8,
        kind: TokenKind,
    ) ?Token {
        // Assert the start position is valid
        assert(start <= self.loc.span.end);
        assert(start < self.source.len);

        const len = keyword.len;
        const lexeme = self.source[start..self.loc.span.end];

        const is_exact_match = self.loc.span.end - start == len and
            std.mem.eql(u8, lexeme, keyword);

        if (is_exact_match) {
            // Assert our column calculation is valid
            assert(self.loc.src.col >= len);

            return Token.init(kind, lexeme, TokenLoc{
                .filename = self.loc.filename,
                .span = .{
                    .start = start,
                    .end = self.loc.span.end,
                },
                .src = .{
                    .line = self.loc.src.line,
                    .col = self.loc.src.col - len,
                },
            });
        } else {
            return null;
        }
    }

    fn handleMultilineString(self: *Lexer, span_start: usize, start_line: usize, start_col: usize) LexerError!Token {
        // We must have processed at least the opening triple quotes
        assert(self.loc.span.end >= 3);
        // Verify we entered this function after seeing exactly three quote characters
        assert(std.mem.eql(u8, self.source[self.loc.span.end - 3 .. self.loc.span.end], "\"\"\""));

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

        if (found_end) {
            const lexeme = self.source[span_start..self.loc.span.end];

            return Token.init(
                .{ .literal = .MultilineString },
                lexeme,
                TokenLoc{
                    .filename = self.loc.filename,
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                },
            );
        } else {
            return error.UnterminatedStrLiteral;
        }
    }

    fn handleNumber(self: *Lexer, base: enum { Decimal, Hex, Octal, Binary }) LexerError!Token {
        const offset = if (base == .Decimal) @as(usize, 0) else 2;
        const position_start = if (base == .Decimal)
            self.loc.span.start
        else
            self.loc.span.end - offset;

        // Assert our starting position is valid
        assert(position_start <= self.loc.span.end);
        assert(position_start < self.source.len);

        const col_offset = self.loc.src.col - offset;
        // Assert column calculation is valid
        assert(col_offset > 0);

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

            const lexeme = self.source[position_start..self.loc.span.end];

            return Token.init(
                .{ .literal = .Float },
                lexeme,
                TokenLoc{
                    .filename = self.loc.filename,
                    .span = .{
                        .start = position_start,
                        .end = self.loc.span.end,
                    },
                    .src = .{
                        .line = self.loc.src.line,
                        .col = col_offset,
                    },
                },
            );
        }

        const lexeme = self.source[position_start..self.loc.span.end];

        return Token.init(
            .{ .literal = .Int },
            lexeme,
            TokenLoc{
                .filename = self.loc.filename,
                .span = .{
                    .start = position_start,
                    .end = self.loc.span.end,
                },
                .src = .{
                    .line = self.loc.src.line,
                    .col = col_offset,
                },
            },
        );
    }

    fn handleStringLiteral(self: *Lexer, span_start: usize) LexerError!Token {
        const start_line = self.loc.src.line;
        const start_col = self.loc.src.col;

        assert(self.source[self.loc.span.end - 1] == '"');

        // Check for multiline string
        if (self.peek()) |next1| {
            if (next1 == '"') {
                self.advance();

                if (self.peek()) |next2| {
                    if (next2 == '"') {
                        self.advance();

                        return try self.handleMultilineString(span_start, start_line, start_col);
                    }
                }

                // Just two quotes - empty string
                const lexeme = self.source[span_start..self.loc.span.end];

                return Token.init(
                    .{ .literal = .String },
                    lexeme,
                    TokenLoc{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    },
                );
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

                if (self.peek()) |escaped_char| {
                    switch (escaped_char) {
                        '\\', '"', 'n', 't', 'r' => self.advance(),
                        'u' => try self.handleUnicodeEscape(),
                        else => return error.UnrecognizedStrEscapeSequence,
                    }
                } else {
                    return error.UnterminatedStrLiteral;
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

        if (found_closing_quote) {
            const lexeme = self.source[span_start..self.loc.span.end];

            return Token.init(
                .{ .literal = .String },
                lexeme,
                TokenLoc{
                    .filename = self.loc.filename,
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                },
            );
        } else {
            return error.UnterminatedStrLiteral;
        }
    }

    fn handleUnicodeEscape(self: *Lexer) LexerError!void {
        // Assert we have input to process
        assert(self.loc.span.end < self.source.len);
        // Spans must remain ordered
        assert(self.loc.span.end >= self.loc.span.start);
        assert(self.loc.src.line > 0);
        assert(self.loc.src.col > 0);

        self.advance();

        if (self.peek()) |next| {
            if (next != '{') return error.InvalidUnicodeEscapeSequence;

            self.advance();
        } else {
            return error.InvalidUnicodeEscapeSequence;
        }

        // Save the starting position of the hex digits
        const escape_start = self.loc.span.end;
        const escape_col = self.loc.src.col;

        var unicode_value: u21 = 0;
        var digit_count: usize = 0;
        while (digit_count < 6) : (digit_count += 1) {
            if (self.peek()) |hex| {
                if (ascii.isHex(hex)) {
                    const digit_value = hexDigitToValue(hex);
                    unicode_value = (unicode_value << 4) | digit_value;

                    self.advance();
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        if (digit_count > 0) {
            if (isValidUnicodeCodepoint(unicode_value)) {
                if (self.peek()) |next| {
                    if (next == '}') {
                        self.advance();

                        return;
                    }
                }

                self.loc.span.start = escape_start;
                self.loc.src.col = escape_col;

                return error.InvalidUnicodeEscapeSequence;
            } else {
                self.loc.span.start = escape_start - 2;
                self.loc.src.col = escape_col - 2;

                return error.CodePointOutOfRange;
            }
        } else {
            self.loc.span.start = escape_start;
            self.loc.src.col = escape_col;

            return error.InvalidUnicodeEscapeSequence;
        }
    }

    fn hexDigitToValue(digit: u8) u4 {
        assert(ascii.isHex(digit));

        return switch (digit) {
            '0'...'9' => @intCast(digit - '0'),
            'a'...'f' => @intCast(digit - 'a' + 10),
            'A'...'F' => @intCast(digit - 'A' + 10),
            else => unreachable,
        };
    }

    fn isBinDigit(c: u8) bool {
        return c == '0' or c == '1';
    }

    fn isOctDigit(c: u8) bool {
        return c >= '0' and c <= '7';
    }

    fn isValidUnicodeCodepoint(value: u21) bool {
        // Value must be <= 0x10FFFF and not in the surrogate range (0xD800..0xDFFF)
        if (value > 0x10FFFF) return false;
        if (value >= 0xD800 and value <= 0xDFFF) return false;

        return true;
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
        .{
            .source = "alias",
            .kind = .{ .keyword = .Alias },
            .lexeme = "alias",
        },
        .{
            .source = "as",
            .kind = .{ .keyword = .As },
            .lexeme = "as",
        },
        .{
            .source = "else",
            .kind = .{ .keyword = .Else },
            .lexeme = "else",
        },
        .{
            .source = "end",
            .kind = .{ .keyword = .End },
            .lexeme = "end",
        },
        .{
            .source = "exposing",
            .kind = .{ .keyword = .Exposing },
            .lexeme = "exposing",
        },
        .{
            .source = "foreign",
            .kind = .{ .keyword = .Foreign },
            .lexeme = "foreign",
        },
        .{
            .source = "hiding",
            .kind = .{ .keyword = .Hiding },
            .lexeme = "hiding",
        },
        .{
            .source = "if",
            .kind = .{ .keyword = .If },
            .lexeme = "if",
        },
        .{
            .source = "in",
            .kind = .{ .keyword = .In },
            .lexeme = "in",
        },
        .{
            .source = "include",
            .kind = .{ .keyword = .Include },
            .lexeme = "include",
        },
        .{
            .source = "infixl",
            .kind = .{ .keyword = .InfixLeft },
            .lexeme = "infixl",
        },
        .{
            .source = "infixn",
            .kind = .{ .keyword = .InfixNon },
            .lexeme = "infixn",
        },
        .{
            .source = "infixr",
            .kind = .{ .keyword = .InfixRight },
            .lexeme = "infixr",
        },
        .{
            .source = "let",
            .kind = .{ .keyword = .Let },
            .lexeme = "let",
        },
        .{
            .source = "match",
            .kind = .{ .keyword = .Match },
            .lexeme = "match",
        },
        .{
            .source = "module",
            .kind = .{ .keyword = .Module },
            .lexeme = "module",
        },
        .{
            .source = "on",
            .kind = .{ .keyword = .On },
            .lexeme = "on",
        },
        .{
            .source = "open",
            .kind = .{ .keyword = .Open },
            .lexeme = "open",
        },
        .{
            .source = "renaming",
            .kind = .{ .keyword = .Renaming },
            .lexeme = "renaming",
        },
        .{
            .source = "then",
            .kind = .{ .keyword = .Then },
            .lexeme = "then",
        },
        .{
            .source = "to",
            .kind = .{ .keyword = .To },
            .lexeme = "to",
        },
        .{
            .source = "type",
            .kind = .{ .keyword = .Type },
            .lexeme = "type",
        },
        .{
            .source = "using",
            .kind = .{ .keyword = .Using },
            .lexeme = "using",
        },
        .{
            .source = "when",
            .kind = .{ .keyword = .When },
            .lexeme = "when",
        },
    };

    for (cases) |case| {
        // Setup
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[delimiter]" {
    const cases = [_]TestCase{
        .{
            .source = ":",
            .kind = .{ .delimiter = .Colon },
            .lexeme = ":",
        },
        .{
            .source = ",",
            .kind = .{ .delimiter = .Comma },
            .lexeme = ",",
        },
        .{
            .source = ".",
            .kind = .{ .delimiter = .Dot },
            .lexeme = ".",
        },
        .{
            .source = "[",
            .kind = .{ .delimiter = .LeftBracket },
            .lexeme = "[",
        },
        .{
            .source = "{",
            .kind = .{ .delimiter = .LeftBrace },
            .lexeme = "{",
        },
        .{
            .source = "(",
            .kind = .{ .delimiter = .LeftParen },
            .lexeme = "(",
        },
        .{
            .source = "]",
            .kind = .{ .delimiter = .RightBracket },
            .lexeme = "]",
        },
        .{
            .source = "}",
            .kind = .{ .delimiter = .RightBrace },
            .lexeme = "}",
        },
        .{
            .source = ")",
            .kind = .{ .delimiter = .RightParen },
            .lexeme = ")",
        },
    };

    for (cases) |case| {
        // Setup
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[symbol]" {
    const cases = [_]TestCase{
        .{
            .source = "->",
            .kind = .{ .symbol = .ArrowRight },
            .lexeme = "->",
        },
        .{
            .source = "=>",
            .kind = .{ .symbol = .DoubleArrowRight },
            .lexeme = "=>",
        },
        .{
            .source = "|",
            .kind = .{ .symbol = .Pipe },
            .lexeme = "|",
        },
        .{
            .source = "_",
            .kind = .{ .symbol = .Underscore },
            .lexeme = "_",
        },
    };

    for (cases) |case| {
        // Setup
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[operator]" {
    const cases = [_]TestCase{
        .{
            .source = "++",
            .kind = .{ .operator = .ListConcat },
            .lexeme = "++",
        },
        .{
            .source = "..",
            .kind = .{ .operator = .Expand },
            .lexeme = "..",
        },
        .{
            .source = "::",
            .kind = .{ .operator = .Cons },
            .lexeme = "::",
        },
        .{
            .source = "<<",
            .kind = .{ .operator = .ComposeLeft },
            .lexeme = "<<",
        },
        .{
            .source = "<>",
            .kind = .{ .operator = .StrConcat },
            .lexeme = "<>",
        },
        .{
            .source = "<|",
            .kind = .{ .operator = .PipeLeft },
            .lexeme = "<|",
        },
        .{
            .source = "=",
            .kind = .{ .operator = .Equal },
            .lexeme = "=",
        },
        .{
            .source = ">>",
            .kind = .{ .operator = .ComposeRight },
            .lexeme = ">>",
        },
        .{
            .source = "\\",
            .kind = .{ .operator = .Lambda },
            .lexeme = "\\",
        },
        .{
            .source = "|>",
            .kind = .{ .operator = .PipeRight },
            .lexeme = "|>",
        },
    };

    for (cases) |case| {
        // Setup
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[arithmetic operator]" {
    const cases = [_]TestCase{
        .{
            .source = "**",
            .kind = .{ .operator = .Exp },
            .lexeme = "**",
        },
        .{
            .source = "+.",
            .kind = .{ .operator = .FloatAdd },
            .lexeme = "+.",
        },
        .{
            .source = "/.",
            .kind = .{ .operator = .FloatDiv },
            .lexeme = "/.",
        },
        .{
            .source = "*.",
            .kind = .{ .operator = .FloatMul },
            .lexeme = "*.",
        },
        .{
            .source = "-.",
            .kind = .{ .operator = .FloatSub },
            .lexeme = "-.",
        },
        .{
            .source = "+",
            .kind = .{ .operator = .IntAdd },
            .lexeme = "+",
        },
        .{
            .source = "/",
            .kind = .{ .operator = .IntDiv },
            .lexeme = "/",
        },
        .{
            .source = "*",
            .kind = .{ .operator = .IntMul },
            .lexeme = "*",
        },
        .{
            .source = "-",
            .kind = .{ .operator = .IntSub },
            .lexeme = "-",
        },
    };

    for (cases) |case| {
        // Setup
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[comparison operator]" {
    const cases = [_]TestCase{
        .{
            .source = "==",
            .kind = .{ .operator = .Equality },
            .lexeme = "==",
        },
        .{
            .source = ">",
            .kind = .{ .operator = .GreaterThan },
            .lexeme = ">",
        },
        .{
            .source = ">=",
            .kind = .{ .operator = .GreaterThanEqual },
            .lexeme = ">=",
        },
        .{
            .source = "<",
            .kind = .{ .operator = .LessThan },
            .lexeme = "<",
        },
        .{
            .source = "<=",
            .kind = .{ .operator = .LessThanEqual },
            .lexeme = "<=",
        },
        .{
            .source = "/=",
            .kind = .{ .operator = .NotEqual },
            .lexeme = "/=",
        },
    };

    for (cases) |case| {
        // Setup
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[logical operator]" {
    const cases = [_]TestCase{
        .{
            .source = "&&",
            .kind = .{ .operator = .LogicalAnd },
            .lexeme = "&&",
        },
        .{
            .source = "||",
            .kind = .{ .operator = .LogicalOr },
            .lexeme = "||",
        },
    };

    for (cases) |case| {
        // Setup
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[special]" {
    const cases = [_]TestCase{
        .{
            .source = "?",
            .kind = .{ .special = .Hole },
            .lexeme = "?",
        },
    };

    for (cases) |case| {
        // Setup
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[comment]" {
    const cases = [_]TestCase{
        .{
            .source = "# this is a comment",
            .kind = .{ .comment = .Regular },
            .lexeme = "# this is a comment",
        },
        .{
            .source = "## this is a doc comment",
            .kind = .{ .comment = .Doc },
            .lexeme = "## this is a doc comment",
        },
        .{
            .source = "# 你好，世界",
            .kind = .{ .comment = .Regular },
            .lexeme = "# 你好，世界",
        },
        .{
            .source = "## こんにちは",
            .kind = .{ .comment = .Doc },
            .lexeme = "## こんにちは",
        },
        .{
            .source = "# 🚀 👽 💣",
            .kind = .{ .comment = .Regular },
            .lexeme = "# 🚀 👽 💣",
        },
    };

    for (cases) |case| {
        // Setup
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[multiline string literal]" {
    const cases = [_]TestCase{
        .{ .source = 
        \\""" This is a
        \\multiline string with
        \\unicode: 你好, こんにちは
        \\"""
        , .kind = .{ .literal = .MultilineString }, .lexeme = 
        \\""" This is a
        \\multiline string with
        \\unicode: 你好, こんにちは
        \\"""
        },
    };

    for (cases) |case| {
        // Setup
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[multiline string literal] error.UnterminatedStrLiteral" {
    const invalid_cases = [_][]const u8{
        \\""" This is an
        \\unterminated multiline string with
        \\unicode: 你好, こんにちは
        \\
    };

    for (invalid_cases) |source| {
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.UnterminatedStrLiteral, result);
    }
}

test "[string literal]" {
    const cases = [_]TestCase{
        .{
            .source = "\"\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"\"",
        },
        .{
            .source = "\"foo\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"foo\"",
        },
        .{
            .source = "\"1\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"1\"",
        },
        .{
            .source = "\"$\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"$\"",
        },
        .{
            .source = "\"Backslash: \\\\\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"Backslash: \\\\\"",
        },
        .{
            .source = "\"Double quote: \\\"Hello!\\\"\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"Double quote: \\\"Hello!\\\"\"",
        },
        .{
            .source = "\"First line\\nSecond line\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"First line\\nSecond line\"",
        },
        .{
            .source = "\"Column1\\tColumn2\\tColumn3\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"Column1\\tColumn2\\tColumn3\"",
        },
        .{
            .source = "\"Carriage return\\rOverwritten text\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"Carriage return\\rOverwritten text\"",
        },
        .{
            .source = "\"Unicode test: \\u{1}\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"Unicode test: \\u{1}\"",
        },
        .{
            .source = "\"Unicode test: \\u{10}\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"Unicode test: \\u{10}\"",
        },
        .{
            .source = "\"Unicode test: \\u{100}\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"Unicode test: \\u{100}\"",
        },
        .{
            .source = "\"Unicode test: \\u{1000}\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"Unicode test: \\u{1000}\"",
        },
        .{
            .source = "\"Unicode test: \\u{10000}\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"Unicode test: \\u{10000}\"",
        },
        .{
            .source = "\"Unicode test: \\u{100000}\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"Unicode test: \\u{100000}\"",
        },
        .{
            .source = "\"Unicode test: \\u{10FFFF}\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"Unicode test: \\u{10FFFF}\"",
        },
        .{
            .source = "\"Unicode test: \\u{0000}\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"Unicode test: \\u{0000}\"",
        }, // edge case
        .{
            .source = "\"Unicode test: \\u{0020}\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"Unicode test: \\u{0020}\"",
        }, // edge case
        .{
            .source = "\"Unicode test: \\u{007F}\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"Unicode test: \\u{007F}\"",
        }, // edge case
        .{
            .source = "\"Unicode with extra: \\u{1234}Hello\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"Unicode with extra: \\u{1234}Hello\"",
        },
        .{
            .source = "\"✅\"",
            .kind = .{ .literal = .String },
            .lexeme = "\"✅\"",
        },
    };

    for (cases) |case| {
        // Setup
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[string literal] error.CodePointOutOfRange" {
    const invalid_cases = [_][]const u8{
        "\"\\u{110000}\"",
        "\"\\u{D800}\"", // high surrogate
        "\"\\u{DFFF}\"", // low surrogate
    };

    for (invalid_cases) |source| {
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.CodePointOutOfRange, result);
    }
}

test "[string literal] error.UnrecognizedStrEscapeSequence" {
    const invalid_cases = [_][]const u8{
        "\"\\q\"",
        "\"\\k\"",
    };

    for (invalid_cases) |source| {
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.UnrecognizedStrEscapeSequence, result);
    }
}

test "[string literal] error.InvalidUnicodeEscapeSequence" {
    const invalid_cases = [_][]const u8{
        "\"unicode missing digits: \\u{}\"", // Unicode escape needs at least 1 hex digit
        "\"invalid unicode: \\u{GHIJ}\"", // Unicode escape must only contain hex digits
    };

    for (invalid_cases) |source| {
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
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
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.UnterminatedStrLiteral, result);
    }
}

test "[char literal]" {
    const cases = [_]TestCase{
        .{
            .source = "'a'",
            .kind = .{ .literal = .Char },
            .lexeme = "'a'",
        },
        .{
            .source = "'1'",
            .kind = .{ .literal = .Char },
            .lexeme = "'1'",
        },
        .{
            .source = "'$'",
            .kind = .{ .literal = .Char },
            .lexeme = "'$'",
        },
        .{
            .source = "'\\n'",
            .kind = .{ .literal = .Char },
            .lexeme = "'\\n'",
        },
        .{
            .source = "'\\t'",
            .kind = .{ .literal = .Char },
            .lexeme = "'\\t'",
        },
        .{
            .source = "'\\r'",
            .kind = .{ .literal = .Char },
            .lexeme = "'\\r'",
        },
        .{
            .source = "'\\''",
            .kind = .{ .literal = .Char },
            .lexeme = "'\\''",
        },
        .{
            .source = "'\\\\'",
            .kind = .{ .literal = .Char },
            .lexeme = "'\\\\'",
        },
        .{
            .source = "'\\u{1}'",
            .kind = .{ .literal = .Char },
            .lexeme = "'\\u{1}'",
        },
        .{
            .source = "'\\u{10}'",
            .kind = .{ .literal = .Char },
            .lexeme = "'\\u{10}'",
        },
        .{
            .source = "'\\u{100}'",
            .kind = .{ .literal = .Char },
            .lexeme = "'\\u{100}'",
        },
        .{
            .source = "'\\u{1000}'",
            .kind = .{ .literal = .Char },
            .lexeme = "'\\u{1000}'",
        },
        .{
            .source = "'\\u{10000}'",
            .kind = .{ .literal = .Char },
            .lexeme = "'\\u{10000}'",
        },
        .{
            .source = "'\\u{100000}'",
            .kind = .{ .literal = .Char },
            .lexeme = "'\\u{100000}'",
        },
        .{
            .source = "'\\u{10FFFF}'",
            .kind = .{ .literal = .Char },
            .lexeme = "'\\u{10FFFF}'",
        },
        .{
            .source = "'\\u{0000}'",
            .kind = .{ .literal = .Char },
            .lexeme = "'\\u{0000}'",
        }, // edge case
        .{
            .source = "'\\u{0020}'",
            .kind = .{ .literal = .Char },
            .lexeme = "'\\u{0020}'",
        }, // edge case
        .{
            .source = "'\\u{007F}'",
            .kind = .{ .literal = .Char },
            .lexeme = "'\\u{007F}'",
        }, // edge case
    };

    for (cases) |case| {
        // Setup
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[char literal] error.EmptyCharLiteral" {
    const source = "''";

    // Setup
    var lexer = Lexer.init(source, TEST_FILE);

    // Action
    const result = lexer.nextToken();

    // Assertions
    // Verify error
    try testing.expectError(error.EmptyCharLiteral, result);
}

test "[char literal] error.CodePointOutOfRange" {
    const invalid_cases = [_][]const u8{
        "'\\u{110000}'",
        "'\\u{D800}'", // high surrogate
        "'\\u{DFFF}'", // low surrogate
    };

    for (invalid_cases) |source| {
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.CodePointOutOfRange, result);
    }
}

test "[char literal] error.UnrecognizedCharEscapeSequence" {
    const invalid_cases = [_][]const u8{
        "'\\q'",
        "'\\k'",
    };

    for (invalid_cases) |source| {
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
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
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
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
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.UnterminatedCharLiteral, result);
    }
}

test "[char literal] error.InvalidUnicodeEscapeSequence" {
    const invalid_cases = [_][]const u8{
        "'\\u{}'",
        "'\\u{g}'",
    };

    for (invalid_cases) |source| {
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.InvalidUnicodeEscapeSequence, result);
    }
}

test "[integer literal]" {
    const cases = [_]TestCase{
        .{
            .source = "42",
            .kind = .{ .literal = .Int },
            .lexeme = "42",
        },
        .{
            .source = "42_000_000",
            .kind = .{ .literal = .Int },
            .lexeme = "42_000_000",
        },
        .{
            .source = "0b101010",
            .kind = .{ .literal = .Int },
            .lexeme = "0b101010",
        },
        .{
            .source = "0b10_1010",
            .kind = .{ .literal = .Int },
            .lexeme = "0b10_1010",
        },
        .{
            .source = "0o52",
            .kind = .{ .literal = .Int },
            .lexeme = "0o52",
        },
        .{
            .source = "0o52_52",
            .kind = .{ .literal = .Int },
            .lexeme = "0o52_52",
        },
        .{
            .source = "0x2A",
            .kind = .{ .literal = .Int },
            .lexeme = "0x2A",
        },
        .{
            .source = "0x2A_2A",
            .kind = .{ .literal = .Int },
            .lexeme = "0x2A_2A",
        },
    };

    for (cases) |case| {
        // Setup
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
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
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.InvalidIntLiteral, result);
    }
}

test "[float literal]" {
    const cases = [_]TestCase{
        .{
            .source = "42.0",
            .kind = .{ .literal = .Float },
            .lexeme = "42.0",
        },
        .{
            .source = "42_000_000.0",
            .kind = .{ .literal = .Float },
            .lexeme = "42_000_000.0",
        },
        .{
            .source = "0.5",
            .kind = .{ .literal = .Float },
            .lexeme = "0.5",
        },
        .{
            .source = "1.23e3",
            .kind = .{ .literal = .Float },
            .lexeme = "1.23e3",
        },
        .{
            .source = "4.56e-2",
            .kind = .{ .literal = .Float },
            .lexeme = "4.56e-2",
        },
        .{
            .source = "3.141_592",
            .kind = .{ .literal = .Float },
            .lexeme = "3.141_592",
        },
    };

    for (cases) |case| {
        // Setup
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try std.testing.expectEqual(case.kind, token.kind);

        // Verify the token lexeme matches
        try std.testing.expectEqualStrings(case.lexeme, token.lexeme);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[float literal] error.InvalidFloat" {
    const invalid_cases = [_][]const u8{
        "1000._", // trailing underscore
        ".5e3", // must begin with a digit
    };

    for (invalid_cases) |source| {
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try std.testing.expectError(error.InvalidFloatLiteral, result);
    }
}

test "[identifier]" {
    const cases = [_]TestCase{
        .{
            .source = "Int",
            .kind = .{ .identifier = .Upper },
            .lexeme = "Int",
        },
        .{
            .source = "Float",
            .kind = .{ .identifier = .Upper },
            .lexeme = "Float",
        },
        .{
            .source = "Bool",
            .kind = .{ .identifier = .Upper },
            .lexeme = "Bool",
        },
        .{
            .source = "True",
            .kind = .{ .identifier = .Upper },
            .lexeme = "True",
        },
        .{
            .source = "False",
            .kind = .{ .identifier = .Upper },
            .lexeme = "False",
        },
        .{
            .source = "Unit",
            .kind = .{ .identifier = .Upper },
            .lexeme = "Unit",
        },
        .{
            .source = "foo",
            .kind = .{ .identifier = .Lower },
            .lexeme = "foo",
        },
        .{
            .source = "foo_bar",
            .kind = .{ .identifier = .Lower },
            .lexeme = "foo_bar",
        },
        .{
            .source = "_foo",
            .kind = .{ .identifier = .Lower },
            .lexeme = "_foo",
        },
        .{
            .source = "A",
            .kind = .{ .identifier = .Upper },
            .lexeme = "A",
        },
        .{
            .source = "a",
            .kind = .{ .identifier = .Lower },
            .lexeme = "a",
        },
        .{
            .source = "_",
            .kind = .{ .symbol = .Underscore },
            .lexeme = "_",
        },
        .{
            .source = "ABC123",
            .kind = .{ .identifier = .Upper },
            .lexeme = "ABC123",
        },
        .{
            .source = "abc123",
            .kind = .{ .identifier = .Lower },
            .lexeme = "abc123",
        },
        .{
            .source = "Foo_Bar",
            .kind = .{ .identifier = .Upper },
            .lexeme = "Foo_Bar",
        },
        .{
            .source = "_foo_BAR_123",
            .kind = .{ .identifier = .Lower },
            .lexeme = "_foo_BAR_123",
        },
        .{
            .source = "__foo",
            .kind = .{ .identifier = .Lower },
            .lexeme = "__foo",
        },
        .{
            .source = "foo?",
            .kind = .{ .identifier = .Lower },
            .lexeme = "foo?",
        },
    };

    for (cases) |case| {
        // Setup
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.lexeme, token.lexeme);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken();
        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
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
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken();

        // Assertions
        // Verify error
        try testing.expectError(error.InvalidIdentifier, result);
    }
}

test "[type variant]" {
    const source = "type FooBar = | Foo | Bar";

    const expected_tokens = [_]Token{
        Token.init(.{ .keyword = .Type }, "type", .{
            .filename = TEST_FILE,
            .span = .{ .start = 0, .end = 4 },
            .src = .{ .line = 1, .col = 1 },
        }),
        Token.init(.{ .identifier = .Upper }, "FooBar", .{
            .filename = TEST_FILE,
            .span = .{ .start = 5, .end = 11 },
            .src = .{ .line = 1, .col = 6 },
        }),
        Token.init(.{ .operator = .Equal }, "=", .{
            .filename = TEST_FILE,
            .span = .{ .start = 12, .end = 13 },
            .src = .{ .line = 1, .col = 13 },
        }),
        Token.init(.{ .symbol = .Pipe }, "|", .{
            .filename = TEST_FILE,
            .span = .{ .start = 14, .end = 15 },
            .src = .{ .line = 1, .col = 15 },
        }),
        Token.init(.{ .identifier = .Upper }, "Foo", .{
            .filename = TEST_FILE,
            .span = .{ .start = 16, .end = 19 },
            .src = .{ .line = 1, .col = 17 },
        }),
        Token.init(.{ .symbol = .Pipe }, "|", .{
            .filename = TEST_FILE,
            .span = .{ .start = 20, .end = 21 },
            .src = .{ .line = 1, .col = 21 },
        }),
        Token.init(.{ .identifier = .Upper }, "Bar", .{
            .filename = TEST_FILE,
            .span = .{ .start = 22, .end = 25 },
            .src = .{ .line = 1, .col = 23 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .span = .{ .start = 25, .end = 25 },
            .src = .{ .line = 1, .col = 26 },
        }),
    };

    // Setup
    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "[type alias]" {
    const source = "type alias Seconds = Int";

    const expected_tokens = [_]Token{
        Token.init(.{ .keyword = .Type }, "type", .{
            .filename = TEST_FILE,
            .span = .{ .start = 0, .end = 4 },
            .src = .{ .line = 1, .col = 1 },
        }),
        Token.init(.{ .keyword = .Alias }, "alias", .{
            .filename = TEST_FILE,
            .span = .{ .start = 5, .end = 10 },
            .src = .{ .line = 1, .col = 6 },
        }),
        Token.init(.{ .identifier = .Upper }, "Seconds", .{
            .filename = TEST_FILE,
            .span = .{ .start = 11, .end = 18 },
            .src = .{ .line = 1, .col = 12 },
        }),
        Token.init(.{ .operator = .Equal }, "=", .{
            .filename = TEST_FILE,
            .span = .{ .start = 19, .end = 20 },
            .src = .{ .line = 1, .col = 20 },
        }),
        Token.init(.{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .span = .{ .start = 21, .end = 24 },
            .src = .{ .line = 1, .col = 22 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .span = .{ .start = 24, .end = 24 },
            .src = .{ .line = 1, .col = 25 },
        }),
    };

    // Setup
    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "[record type]" {
    const source = "type FooBar = { foo : Int, bar : String }";

    const expected_tokens = [_]Token{
        Token.init(.{ .keyword = .Type }, "type", .{
            .filename = TEST_FILE,
            .span = .{ .start = 0, .end = 4 },
            .src = .{ .line = 1, .col = 1 },
        }),
        Token.init(.{ .identifier = .Upper }, "FooBar", .{
            .filename = TEST_FILE,
            .span = .{ .start = 5, .end = 11 },
            .src = .{ .line = 1, .col = 6 },
        }),
        Token.init(.{ .operator = .Equal }, "=", .{
            .filename = TEST_FILE,
            .span = .{ .start = 12, .end = 13 },
            .src = .{ .line = 1, .col = 13 },
        }),
        Token.init(.{ .delimiter = .LeftBrace }, "{", .{
            .filename = TEST_FILE,
            .span = .{ .start = 14, .end = 15 },
            .src = .{ .line = 1, .col = 15 },
        }),
        Token.init(.{ .identifier = .Lower }, "foo", .{
            .filename = TEST_FILE,
            .span = .{ .start = 16, .end = 19 },
            .src = .{ .line = 1, .col = 17 },
        }),
        Token.init(.{ .delimiter = .Colon }, ":", .{
            .filename = TEST_FILE,
            .span = .{ .start = 20, .end = 21 },
            .src = .{ .line = 1, .col = 21 },
        }),
        Token.init(.{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .span = .{ .start = 22, .end = 25 },
            .src = .{ .line = 1, .col = 23 },
        }),
        Token.init(.{ .delimiter = .Comma }, ",", .{
            .filename = TEST_FILE,
            .span = .{ .start = 25, .end = 26 },
            .src = .{ .line = 1, .col = 26 },
        }),
        Token.init(.{ .identifier = .Lower }, "bar", .{
            .filename = TEST_FILE,
            .span = .{ .start = 27, .end = 30 },
            .src = .{ .line = 1, .col = 28 },
        }),
        Token.init(.{ .delimiter = .Colon }, ":", .{
            .filename = TEST_FILE,
            .span = .{ .start = 31, .end = 32 },
            .src = .{ .line = 1, .col = 32 },
        }),
        Token.init(.{ .identifier = .Upper }, "String", .{
            .filename = TEST_FILE,
            .span = .{ .start = 33, .end = 39 },
            .src = .{ .line = 1, .col = 34 },
        }),
        Token.init(.{ .delimiter = .RightBrace }, "}", .{
            .filename = TEST_FILE,
            .span = .{ .start = 40, .end = 41 },
            .src = .{ .line = 1, .col = 41 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .span = .{ .start = 41, .end = 41 },
            .src = .{ .line = 1, .col = 42 },
        }),
    };

    // Setup
    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "[module definition]" {
    const source = "module Foo exposing (Foo(..), bar) end";

    const expected_tokens = [_]Token{
        Token.init(.{ .keyword = .Module }, "module", .{
            .filename = TEST_FILE,
            .span = .{ .start = 0, .end = 6 },
            .src = .{ .line = 1, .col = 1 },
        }),
        Token.init(.{ .identifier = .Upper }, "Foo", .{
            .filename = TEST_FILE,
            .span = .{ .start = 7, .end = 10 },
            .src = .{ .line = 1, .col = 8 },
        }),
        Token.init(.{ .keyword = .Exposing }, "exposing", .{
            .filename = TEST_FILE,
            .span = .{ .start = 11, .end = 19 },
            .src = .{ .line = 1, .col = 12 },
        }),
        Token.init(.{ .delimiter = .LeftParen }, "(", .{
            .filename = TEST_FILE,
            .span = .{ .start = 20, .end = 21 },
            .src = .{ .line = 1, .col = 21 },
        }),
        Token.init(.{ .identifier = .Upper }, "Foo", .{
            .filename = TEST_FILE,
            .span = .{ .start = 21, .end = 24 },
            .src = .{ .line = 1, .col = 22 },
        }),
        Token.init(.{ .delimiter = .LeftParen }, "(", .{
            .filename = TEST_FILE,
            .span = .{ .start = 24, .end = 25 },
            .src = .{ .line = 1, .col = 25 },
        }),
        Token.init(.{ .operator = .Expand }, "..", .{
            .filename = TEST_FILE,
            .span = .{ .start = 25, .end = 27 },
            .src = .{ .line = 1, .col = 26 },
        }),
        Token.init(.{ .delimiter = .RightParen }, ")", .{
            .filename = TEST_FILE,
            .span = .{ .start = 27, .end = 28 },
            .src = .{ .line = 1, .col = 28 },
        }),
        Token.init(.{ .delimiter = .Comma }, ",", .{
            .filename = TEST_FILE,
            .span = .{ .start = 28, .end = 29 },
            .src = .{ .line = 1, .col = 29 },
        }),
        Token.init(.{ .identifier = .Lower }, "bar", .{
            .filename = TEST_FILE,
            .span = .{ .start = 30, .end = 33 },
            .src = .{ .line = 1, .col = 31 },
        }),
        Token.init(.{ .delimiter = .RightParen }, ")", .{
            .filename = TEST_FILE,
            .span = .{ .start = 33, .end = 34 },
            .src = .{ .line = 1, .col = 34 },
        }),
        Token.init(.{ .keyword = .End }, "end", .{
            .filename = TEST_FILE,
            .span = .{ .start = 35, .end = 38 },
            .src = .{ .line = 1, .col = 36 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .span = .{ .start = 38, .end = 38 },
            .src = .{ .line = 1, .col = 39 },
        }),
    };

    // Setup
    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "[top level function definition]" {
    const source = "let add : Int -> Int -> Int = \\x y => x + y";

    const expected_tokens = [_]Token{
        Token.init(.{ .keyword = .Let }, "let", .{
            .filename = TEST_FILE,
            .span = .{ .start = 0, .end = 3 },
            .src = .{ .line = 1, .col = 1 },
        }),
        Token.init(.{ .identifier = .Lower }, "add", .{
            .filename = TEST_FILE,
            .span = .{ .start = 4, .end = 7 },
            .src = .{ .line = 1, .col = 5 },
        }),
        Token.init(.{ .delimiter = .Colon }, ":", .{
            .filename = TEST_FILE,
            .span = .{ .start = 8, .end = 9 },
            .src = .{ .line = 1, .col = 9 },
        }),
        Token.init(.{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .span = .{ .start = 10, .end = 13 },
            .src = .{ .line = 1, .col = 11 },
        }),
        Token.init(.{ .symbol = .ArrowRight }, "->", .{
            .filename = TEST_FILE,
            .span = .{ .start = 14, .end = 16 },
            .src = .{ .line = 1, .col = 15 },
        }),
        Token.init(.{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .span = .{ .start = 17, .end = 20 },
            .src = .{ .line = 1, .col = 18 },
        }),
        Token.init(.{ .symbol = .ArrowRight }, "->", .{
            .filename = TEST_FILE,
            .span = .{ .start = 21, .end = 23 },
            .src = .{ .line = 1, .col = 22 },
        }),
        Token.init(.{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .span = .{ .start = 24, .end = 27 },
            .src = .{ .line = 1, .col = 25 },
        }),
        Token.init(.{ .operator = .Equal }, "=", .{
            .filename = TEST_FILE,
            .span = .{ .start = 28, .end = 29 },
            .src = .{ .line = 1, .col = 29 },
        }),
        Token.init(.{ .operator = .Lambda }, "\\", .{
            .filename = TEST_FILE,
            .span = .{ .start = 30, .end = 31 },
            .src = .{ .line = 1, .col = 31 },
        }),
        Token.init(.{ .identifier = .Lower }, "x", .{
            .filename = TEST_FILE,
            .span = .{ .start = 31, .end = 32 },
            .src = .{ .line = 1, .col = 32 },
        }),
        Token.init(.{ .identifier = .Lower }, "y", .{
            .filename = TEST_FILE,
            .span = .{ .start = 33, .end = 34 },
            .src = .{ .line = 1, .col = 34 },
        }),
        Token.init(.{ .symbol = .DoubleArrowRight }, "=>", .{
            .filename = TEST_FILE,
            .span = .{ .start = 35, .end = 37 },
            .src = .{ .line = 1, .col = 36 },
        }),
        Token.init(.{ .identifier = .Lower }, "x", .{
            .filename = TEST_FILE,
            .span = .{ .start = 38, .end = 39 },
            .src = .{ .line = 1, .col = 39 },
        }),
        Token.init(.{ .operator = .IntAdd }, "+", .{
            .filename = TEST_FILE,
            .span = .{ .start = 40, .end = 41 },
            .src = .{ .line = 1, .col = 41 },
        }),
        Token.init(.{ .identifier = .Lower }, "y", .{
            .filename = TEST_FILE,
            .span = .{ .start = 42, .end = 43 },
            .src = .{ .line = 1, .col = 43 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .span = .{ .start = 43, .end = 43 },
            .src = .{ .line = 1, .col = 44 },
        }),
    };

    // Setup
    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "[pattern matching]" {
    const source = "match x on | Foo => 1 | Bar => 2 _ => 3";

    const expected_tokens = [_]Token{
        Token.init(.{ .keyword = .Match }, "match", .{
            .filename = TEST_FILE,
            .span = .{ .start = 0, .end = 5 },
            .src = .{ .line = 1, .col = 1 },
        }),
        Token.init(.{ .identifier = .Lower }, "x", .{
            .filename = TEST_FILE,
            .span = .{ .start = 6, .end = 7 },
            .src = .{ .line = 1, .col = 7 },
        }),
        Token.init(.{ .keyword = .On }, "on", .{
            .filename = TEST_FILE,
            .span = .{ .start = 8, .end = 10 },
            .src = .{ .line = 1, .col = 9 },
        }),
        Token.init(.{ .symbol = .Pipe }, "|", .{
            .filename = TEST_FILE,
            .span = .{ .start = 11, .end = 12 },
            .src = .{ .line = 1, .col = 12 },
        }),
        Token.init(.{ .identifier = .Upper }, "Foo", .{
            .filename = TEST_FILE,
            .span = .{ .start = 13, .end = 16 },
            .src = .{ .line = 1, .col = 14 },
        }),
        Token.init(.{ .symbol = .DoubleArrowRight }, "=>", .{
            .filename = TEST_FILE,
            .span = .{ .start = 17, .end = 19 },
            .src = .{ .line = 1, .col = 18 },
        }),
        Token.init(.{ .literal = .Int }, "1", .{
            .filename = TEST_FILE,
            .span = .{ .start = 20, .end = 21 },
            .src = .{ .line = 1, .col = 21 },
        }),
        Token.init(.{ .symbol = .Pipe }, "|", .{
            .filename = TEST_FILE,
            .span = .{ .start = 22, .end = 23 },
            .src = .{ .line = 1, .col = 23 },
        }),
        Token.init(.{ .identifier = .Upper }, "Bar", .{
            .filename = TEST_FILE,
            .span = .{ .start = 24, .end = 27 },
            .src = .{ .line = 1, .col = 25 },
        }),
        Token.init(.{ .symbol = .DoubleArrowRight }, "=>", .{
            .filename = TEST_FILE,
            .span = .{ .start = 28, .end = 30 },
            .src = .{ .line = 1, .col = 29 },
        }),
        Token.init(.{ .literal = .Int }, "2", .{
            .filename = TEST_FILE,
            .span = .{ .start = 31, .end = 32 },
            .src = .{ .line = 1, .col = 32 },
        }),
        Token.init(.{ .symbol = .Underscore }, "_", .{
            .filename = TEST_FILE,
            .span = .{ .start = 33, .end = 34 },
            .src = .{ .line = 1, .col = 34 },
        }),
        Token.init(.{ .symbol = .DoubleArrowRight }, "=>", .{
            .filename = TEST_FILE,
            .span = .{ .start = 35, .end = 37 },
            .src = .{ .line = 1, .col = 36 },
        }),
        Token.init(.{ .literal = .Int }, "3", .{
            .filename = TEST_FILE,
            .span = .{ .start = 38, .end = 39 },
            .src = .{ .line = 1, .col = 39 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .span = .{ .start = 39, .end = 39 },
            .src = .{ .line = 1, .col = 40 },
        }),
    };

    // Setup
    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "[let_in block]" {
    const source = "let x : Int = 42 in";

    const expected_tokens = [_]Token{
        Token.init(.{ .keyword = .Let }, "let", .{
            .filename = TEST_FILE,
            .span = .{ .start = 0, .end = 3 },
            .src = .{ .line = 1, .col = 1 },
        }),
        Token.init(.{ .identifier = .Lower }, "x", .{
            .filename = TEST_FILE,
            .span = .{ .start = 4, .end = 5 },
            .src = .{ .line = 1, .col = 5 },
        }),
        Token.init(.{ .delimiter = .Colon }, ":", .{
            .filename = TEST_FILE,
            .span = .{ .start = 6, .end = 7 },
            .src = .{ .line = 1, .col = 7 },
        }),
        Token.init(.{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .span = .{ .start = 8, .end = 11 },
            .src = .{ .line = 1, .col = 9 },
        }),
        Token.init(.{ .operator = .Equal }, "=", .{
            .filename = TEST_FILE,
            .span = .{ .start = 12, .end = 13 },
            .src = .{ .line = 1, .col = 13 },
        }),
        Token.init(.{ .literal = .Int }, "42", .{
            .filename = TEST_FILE,
            .span = .{ .start = 14, .end = 16 },
            .src = .{ .line = 1, .col = 15 },
        }),
        Token.init(.{ .keyword = .In }, "in", .{
            .filename = TEST_FILE,
            .span = .{ .start = 17, .end = 19 },
            .src = .{ .line = 1, .col = 18 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .span = .{ .start = 19, .end = 19 },
            .src = .{ .line = 1, .col = 20 },
        }),
    };

    // Setup
    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "[if_then_else statment]" {
    const source = "if x == 1 then True else False";

    const expected_tokens = [_]Token{
        Token.init(.{ .keyword = .If }, "if", .{
            .filename = TEST_FILE,
            .span = .{ .start = 0, .end = 2 },
            .src = .{ .line = 1, .col = 1 },
        }),
        Token.init(.{ .identifier = .Lower }, "x", .{
            .filename = TEST_FILE,
            .span = .{ .start = 3, .end = 4 },
            .src = .{ .line = 1, .col = 4 },
        }),
        Token.init(.{ .operator = .Equality }, "==", .{
            .filename = TEST_FILE,
            .span = .{ .start = 5, .end = 7 },
            .src = .{ .line = 1, .col = 6 },
        }),
        Token.init(.{ .literal = .Int }, "1", .{
            .filename = TEST_FILE,
            .span = .{ .start = 8, .end = 9 },
            .src = .{ .line = 1, .col = 9 },
        }),
        Token.init(.{ .keyword = .Then }, "then", .{
            .filename = TEST_FILE,
            .span = .{ .start = 10, .end = 14 },
            .src = .{ .line = 1, .col = 11 },
        }),
        Token.init(.{ .identifier = .Upper }, "True", .{
            .filename = TEST_FILE,
            .span = .{ .start = 15, .end = 19 },
            .src = .{ .line = 1, .col = 16 },
        }),
        Token.init(.{ .keyword = .Else }, "else", .{
            .filename = TEST_FILE,
            .span = .{ .start = 20, .end = 24 },
            .src = .{ .line = 1, .col = 21 },
        }),
        Token.init(.{ .identifier = .Upper }, "False", .{
            .filename = TEST_FILE,
            .span = .{ .start = 25, .end = 30 },
            .src = .{ .line = 1, .col = 26 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .span = .{ .start = 30, .end = 30 },
            .src = .{ .line = 1, .col = 31 },
        }),
    };

    // Setup
    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken();

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}
