const std = @import("std");

const ascii = std.ascii;
const assert = std.debug.assert;

pub const KeywordKind = enum {
    Alias,
    As,
    Else,
    End,
    Exposing,
    Fn,
    Foreign,
    Hiding,
    If,
    Include,
    InfixLeft,
    InfixNon,
    InfixRight,
    Let,
    Match,
    Module,
    On,
    Open,
    Then,
    Type,
    Using,
    When,
};

pub const OperatorKind = []const u8;

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
            .operator => |v| try writer.print(".operator = {s}", .{v}),
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
    /// The kind of token (e.g., literal, keyword).
    kind: TokenKind,

    /// The string representation of the token.
    lexeme: []const u8,

    /// The complete location information for the token.
    loc: TokenLoc,

    pub fn init(allocator: std.mem.Allocator, kind: TokenKind, lexeme: []const u8, loc: TokenLoc) !Token {
        // Assert location spans are valid
        assert(loc.span.start <= loc.span.end);
        // Assert line numbers are 1-based
        assert(loc.src.line > 0);
        // Assert column numbers are 1-based
        assert(loc.src.col > 0);
        // Assert lexeme length matches span
        // assert(lexeme.len == loc.span.end - loc.span.start); // this assertion breaks with we have a multiline string?
        // You commented out assert(lexeme.len == loc.span.end - loc.span.start) because it breaks with multiline strings. That’s a hint your lexer might normalize strings (e.g., stripping newlines or escapes), which could mean lexeme isn’t always a direct slice of the input. If lexeme is ever a modified or temporary string, duplicating it becomes even more critical to avoid dangling references.

        return .{
            .kind = kind,
            .lexeme = try allocator.dupe(u8, lexeme),
            .loc = loc,
        };
    }

    pub fn deinit(self: Token, allocator: std.mem.Allocator) void {
        allocator.free(self.lexeme);
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
    Unrecognized,
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
} || error{OutOfMemory};

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

    fn isOperator(c: u8) bool {
        return switch (c) {
            '+',
            '-',
            '*',
            '/',
            '<',
            '>',
            '|',
            '=',
            '.',
            ':',
            '@',
            '$',
            '^',
            '&',
            => true,
            else => false,
        };
    }

    /// Retrieves and returns the next token from the source code, advancing the lexer position.
    pub fn nextToken(self: *Lexer, allocator: std.mem.Allocator) LexerError!Token {
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
            const tok = try Token.init(allocator, .{ .special = .Eof }, "", .{
                .filename = self.loc.filename,
                .span = .{
                    .start = span_start,
                    .end = self.loc.span.start,
                },
                .src = .{
                    .line = start_line,
                    .col = start_col,
                },
            });
            errdefer tok.deinit(allocator);

            return tok;
        };

        switch (c) {
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

                const tok = try Token.init(allocator, kind, lexeme, .{
                    .filename = self.loc.filename,
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                });
                errdefer tok.deinit(allocator);

                return tok;
            },
            '"' => {
                self.advance();

                return self.scanStringLiteral(allocator, span_start);
            },
            '\'' => {
                const position_start = span_start;
                self.advance();

                if (self.peek() == '\'') {
                    return error.EmptyCharLiteral;
                }

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
                                'u' => try self.scanUnicodeEscape(),
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

                        const tok = try Token.init(allocator, .{ .literal = .Char }, lexeme, .{
                            .filename = self.loc.filename,
                            .span = .{
                                .start = position_start,
                                .end = self.loc.span.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        });
                        errdefer tok.deinit(allocator);

                        return tok;
                    } else {
                        return error.MultipleCharsInLiteral;
                    }
                } else {
                    return error.UnterminatedCharLiteral;
                }
            },
            // '0' => {
            //     self.advance();

            //     if (self.peek()) |next| {
            //         switch (next) {
            //             'b' => {
            //                 self.advance();

            //                 return self.scanNumber(.Binary);
            //             },
            //             'o' => {
            //                 self.advance();

            //                 return self.scanNumber(.Octal);
            //             },
            //             'x' => {
            //                 self.advance();

            //                 return self.scanNumber(.Hex);
            //             },
            //             else => return self.scanNumber(.Decimal),
            //         }
            //     }

            //     const lexeme = self.source[span_start..self.loc.span.end];

            //     return Token.init(.{ .literal = .Int }, lexeme, .{
            //         .filename = self.loc.filename,
            //         .span = .{
            //             .start = span_start,
            //             .end = self.loc.span.end,
            //         },
            //         .src = .{
            //             .line = start_line,
            //             .col = start_col,
            //         },
            //     });
            // },
            // '1'...'9' => {
            //     return self.scanNumber(.Decimal);
            // },
            '+',
            '-',
            '*',
            '/',
            '<',
            '>',
            '|',
            '=',
            '.',
            ':',
            '@',
            '$',
            '^',
            '&',
            => {
                const start = self.loc.span.start;

                while (self.peek()) |next| {
                    if (isOperator(next)) {
                        self.advance();
                    } else {
                        break;
                    }
                }

                const lexeme = self.source[start..self.loc.span.end];
                const loc = TokenLoc{
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

                if (std.mem.eql(u8, lexeme, ".")) {
                    const tok = try Token.init(allocator, .{ .delimiter = .Dot }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (std.mem.eql(u8, lexeme, ":")) {
                    const tok = try Token.init(allocator, .{ .delimiter = .Colon }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (std.mem.eql(u8, lexeme, "->")) {
                    const tok = try Token.init(allocator, .{ .symbol = .ArrowRight }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (std.mem.eql(u8, lexeme, "=>")) {
                    const tok = try Token.init(allocator, .{ .symbol = .DoubleArrowRight }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (std.mem.eql(u8, lexeme, "|")) {
                    const tok = try Token.init(allocator, .{ .symbol = .Pipe }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                const tok = try Token.init(allocator, .{ .operator = lexeme }, lexeme, loc);
                errdefer tok.deinit(allocator);

                return tok;
            },
            ',' => {
                self.advance();

                const tok = try Token.init(allocator, .{ .delimiter = .Comma }, ",", .{
                    .filename = self.loc.filename,
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                });
                errdefer tok.deinit(allocator);

                return tok;
            },
            '[' => {
                self.advance();

                const tok = try Token.init(allocator, .{ .delimiter = .LeftBracket }, "[", .{
                    .filename = self.loc.filename,
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                });
                errdefer tok.deinit(allocator);

                return tok;
            },
            '{' => {
                self.advance();

                const tok = try Token.init(allocator, .{ .delimiter = .LeftBrace }, "{", .{
                    .filename = self.loc.filename,
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                });
                errdefer tok.deinit(allocator);

                return tok;
            },
            '(' => {
                self.advance();

                const tok = try Token.init(allocator, .{ .delimiter = .LeftParen }, "(", .{
                    .filename = self.loc.filename,
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                });
                errdefer tok.deinit(allocator);

                return tok;
            },
            ']' => {
                self.advance();

                const tok = try Token.init(allocator, .{ .delimiter = .RightBracket }, "]", .{
                    .filename = self.loc.filename,
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                });
                errdefer tok.deinit(allocator);

                return tok;
            },
            '}' => {
                self.advance();

                const tok = try Token.init(allocator, .{ .delimiter = .RightBrace }, "}", .{
                    .filename = self.loc.filename,
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                });
                errdefer tok.deinit(allocator);

                return tok;
            },
            ')' => {
                self.advance();

                const tok = try Token.init(allocator, .{ .delimiter = .RightParen }, ")", .{
                    .filename = self.loc.filename,
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                });
                errdefer tok.deinit(allocator);

                return tok;
            },
            '?' => {
                self.advance();

                if (self.peek() == null) {
                    const tok = try Token.init(allocator, .{ .special = .Hole }, "?", .{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    });
                    errdefer tok.deinit(allocator);

                    return tok;
                } else {
                    return error.InvalidIdentifier;
                }
            },
            'a'...'z', 'A'...'Z', '_' => {
                if (c == '_') {
                    self.advance();

                    if (self.peek() == null) {
                        const lexeme = self.source[span_start..self.loc.span.end];

                        const tok = try Token.init(allocator, .{ .symbol = .Underscore }, lexeme, .{
                            .filename = self.loc.filename,
                            .span = .{
                                .start = span_start,
                                .end = self.loc.span.end,
                            },
                            .src = .{
                                .line = start_line,
                                .col = start_col,
                            },
                        });
                        errdefer tok.deinit(allocator);

                        return tok;
                    }

                    if (self.peek()) |next| {
                        if (ascii.isUpper(next)) {
                            return error.InvalidIdentifier;
                        }

                        if (ascii.isDigit(next)) {
                            return error.InvalidIntLiteral;
                        }

                        switch (next) {
                            'a'...'z', '_' => {
                                while (self.peek()) |next_char| {
                                    switch (next_char) {
                                        'a'...'z', 'A'...'Z', '_', '0'...'9' => self.advance(),
                                        '?' => {
                                            self.advance();

                                            if (self.peek()) |x| {
                                                if (!ascii.isWhitespace(x)) {
                                                    return error.InvalidIdentifier;
                                                }
                                            }

                                            break;
                                        },
                                        else => break,
                                    }
                                }

                                const lexeme = self.source[span_start..self.loc.span.end];

                                const tok = try Token.init(allocator, .{ .identifier = .Lower }, lexeme, .{
                                    .filename = self.loc.filename,
                                    .span = .{
                                        .start = span_start,
                                        .end = self.loc.span.end,
                                    },
                                    .src = .{
                                        .line = start_line,
                                        .col = start_col,
                                    },
                                });
                                errdefer tok.deinit(allocator);

                                return tok;
                            },
                            else => {
                                const tok = try Token.init(allocator, .{ .symbol = .Underscore }, "_", .{
                                    .filename = self.loc.filename,
                                    .span = .{
                                        .start = span_start,
                                        .end = self.loc.span.end,
                                    },
                                    .src = .{
                                        .line = start_line,
                                        .col = start_col,
                                    },
                                });
                                errdefer tok.deinit(allocator);

                                return tok;
                            },
                        }
                    }

                    const tok = try Token.init(allocator, .{ .symbol = .Underscore }, "_", .{
                        .filename = self.loc.filename,
                        .span = .{
                            .start = span_start,
                            .end = self.loc.span.end,
                        },
                        .src = .{
                            .line = start_line,
                            .col = start_col,
                        },
                    });
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                while (self.peek()) |next| {
                    switch (next) {
                        'a'...'z', 'A'...'Z', '_', '0'...'9' => self.advance(),
                        '?' => {
                            self.advance();

                            if (self.peek()) |x| {
                                if (!ascii.isWhitespace(x)) {
                                    return error.InvalidIdentifier;
                                }
                            }

                            break;
                        },
                        else => break,
                    }
                }

                const lexeme = self.source[span_start..self.loc.span.end];
                const loc = TokenLoc{
                    .filename = self.loc.filename,
                    .span = .{ .start = span_start, .end = self.loc.span.end },
                    .src = .{ .line = start_line, .col = start_col },
                };

                if (lexeme.len == 5 and std.mem.eql(u8, lexeme, "alias")) {
                    const tok = try Token.init(allocator, .{ .keyword = .Alias }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 2 and std.mem.eql(u8, lexeme, "as")) {
                    const tok = try Token.init(allocator, .{ .keyword = .As }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 4 and std.mem.eql(u8, lexeme, "else")) {
                    const tok = try Token.init(allocator, .{ .keyword = .Else }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 3 and std.mem.eql(u8, lexeme, "end")) {
                    const tok = try Token.init(allocator, .{ .keyword = .End }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 8 and std.mem.eql(u8, lexeme, "exposing")) {
                    const tok = try Token.init(allocator, .{ .keyword = .Exposing }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 2 and std.mem.eql(u8, lexeme, "fn")) {
                    const tok = try Token.init(allocator, .{ .keyword = .Fn }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 7 and std.mem.eql(u8, lexeme, "foreign")) {
                    const tok = try Token.init(allocator, .{ .keyword = .Foreign }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 6 and std.mem.eql(u8, lexeme, "hiding")) {
                    const tok = try Token.init(allocator, .{ .keyword = .Hiding }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 2 and std.mem.eql(u8, lexeme, "if")) {
                    const tok = try Token.init(allocator, .{ .keyword = .If }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 7 and std.mem.eql(u8, lexeme, "include")) {
                    const tok = try Token.init(allocator, .{ .keyword = .Include }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 6 and std.mem.eql(u8, lexeme, "infixl")) {
                    const tok = try Token.init(allocator, .{ .keyword = .InfixLeft }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 6 and std.mem.eql(u8, lexeme, "infixn")) {
                    const tok = try Token.init(allocator, .{ .keyword = .InfixNon }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 6 and std.mem.eql(u8, lexeme, "infixr")) {
                    const tok = try Token.init(allocator, .{ .keyword = .InfixRight }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 3 and std.mem.eql(u8, lexeme, "let")) {
                    const tok = try Token.init(allocator, .{ .keyword = .Let }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 5 and std.mem.eql(u8, lexeme, "match")) {
                    const tok = try Token.init(allocator, .{ .keyword = .Match }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 6 and std.mem.eql(u8, lexeme, "module")) {
                    const tok = try Token.init(allocator, .{ .keyword = .Module }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 2 and std.mem.eql(u8, lexeme, "on")) {
                    const tok = try Token.init(allocator, .{ .keyword = .On }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 4 and std.mem.eql(u8, lexeme, "open")) {
                    const tok = try Token.init(allocator, .{ .keyword = .Open }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 4 and std.mem.eql(u8, lexeme, "then")) {
                    const tok = try Token.init(allocator, .{ .keyword = .Then }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 4 and std.mem.eql(u8, lexeme, "type")) {
                    const tok = try Token.init(allocator, .{ .keyword = .Type }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 5 and std.mem.eql(u8, lexeme, "using")) {
                    const tok = try Token.init(allocator, .{ .keyword = .Using }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                if (lexeme.len == 4 and std.mem.eql(u8, lexeme, "when")) {
                    const tok = try Token.init(allocator, .{ .keyword = .When }, lexeme, loc);
                    errdefer tok.deinit(allocator);

                    return tok;
                }

                switch (lexeme[0]) {
                    'A'...'Z' => {
                        const tok = try Token.init(allocator, .{ .identifier = .Upper }, lexeme, loc);
                        errdefer tok.deinit(allocator);

                        return tok;
                    },
                    'a'...'z', '_' => {
                        const tok = try Token.init(allocator, .{ .identifier = .Lower }, lexeme, loc);
                        errdefer tok.deinit(allocator);

                        return tok;
                    },
                    else => {
                        const tok = try Token.init(allocator, .{ .special = .Unrecognized }, lexeme, loc);
                        errdefer tok.deinit(allocator);

                        return tok;
                    },
                }
            },
            else => {
                self.advance();

                const lexeme = self.source[span_start..self.loc.span.end];

                const tok = try Token.init(allocator, .{ .special = .Unrecognized }, lexeme, .{
                    .filename = self.loc.filename,
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                });
                errdefer tok.deinit(allocator);

                return tok;
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

    fn scanNumber(self: *Lexer, base: enum { Decimal, Hex, Octal, Binary }) LexerError!Token {
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
                if (next == '_') {
                    return error.InvalidIntLiteral;
                }
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

        if (last_was_underscore) {
            return error.InvalidIntLiteral;
        }

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

            return Token.init(.{ .literal = .Float }, lexeme, .{
                .filename = self.loc.filename,
                .span = .{
                    .start = position_start,
                    .end = self.loc.span.end,
                },
                .src = .{
                    .line = self.loc.src.line,
                    .col = col_offset,
                },
            });
        }

        const lexeme = self.source[position_start..self.loc.span.end];

        return Token.init(.{ .literal = .Int }, lexeme, .{
            .filename = self.loc.filename,
            .span = .{
                .start = position_start,
                .end = self.loc.span.end,
            },
            .src = .{
                .line = self.loc.src.line,
                .col = col_offset,
            },
        });
    }

    fn scanStringLiteral(self: *Lexer, allocator: std.mem.Allocator, span_start: usize) LexerError!Token {
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

                            const tok = try Token.init(allocator, .{ .literal = .MultilineString }, lexeme, .{
                                .filename = self.loc.filename,
                                .span = .{
                                    .start = span_start,
                                    .end = self.loc.span.end,
                                },
                                .src = .{
                                    .line = start_line,
                                    .col = start_col,
                                },
                            });
                            errdefer tok.deinit(allocator);

                            return tok;
                        } else {
                            return error.UnterminatedStrLiteral;
                        }
                    }
                }

                // Just two quotes - empty string
                const lexeme = self.source[span_start..self.loc.span.end];

                const tok = try Token.init(allocator, .{ .literal = .String }, lexeme, .{
                    .filename = self.loc.filename,
                    .span = .{
                        .start = span_start,
                        .end = self.loc.span.end,
                    },
                    .src = .{
                        .line = start_line,
                        .col = start_col,
                    },
                });
                errdefer tok.deinit(allocator);

                return tok;
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
                        'u' => try self.scanUnicodeEscape(),
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

            const tok = try Token.init(allocator, .{ .literal = .String }, lexeme, .{
                .filename = self.loc.filename,
                .span = .{
                    .start = span_start,
                    .end = self.loc.span.end,
                },
                .src = .{
                    .line = start_line,
                    .col = start_col,
                },
            });
            errdefer tok.deinit(allocator);

            return tok;
        } else {
            return error.UnterminatedStrLiteral;
        }
    }

    fn scanUnicodeEscape(self: *Lexer) LexerError!void {
        // Assert we have input to process
        assert(self.loc.span.end < self.source.len);
        // Spans must remain ordered
        assert(self.loc.span.end >= self.loc.span.start);
        assert(self.loc.src.line > 0);
        assert(self.loc.src.col > 0);

        self.advance();

        if (self.peek()) |next| {
            if (next != '{') {
                return error.InvalidUnicodeEscapeSequence;
            }

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

const TEST_FILE = "test.mn";

const TestCase = struct {
    source: []const u8,
    token: Token,
};

test "[keyword]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const cases = [_]TestCase{
        .{
            .source = "alias",
            .token = try Token.init(allocator, .{ .keyword = .Alias }, "alias", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 5 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "as",
            .token = try Token.init(allocator, .{ .keyword = .As }, "as", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "else",
            .token = try Token.init(allocator, .{ .keyword = .Else }, "else", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 4 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "end",
            .token = try Token.init(allocator, .{ .keyword = .End }, "end", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "exposing",
            .token = try Token.init(allocator, .{ .keyword = .Exposing }, "exposing", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 8 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "fn",
            .token = try Token.init(allocator, .{ .keyword = .Fn }, "fn", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "foreign",
            .token = try Token.init(allocator, .{ .keyword = .Foreign }, "foreign", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 7 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "hiding",
            .token = try Token.init(allocator, .{ .keyword = .Hiding }, "hiding", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 6 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "if",
            .token = try Token.init(allocator, .{ .keyword = .If }, "if", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "include",
            .token = try Token.init(allocator, .{ .keyword = .Include }, "include", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 7 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "infixl",
            .token = try Token.init(allocator, .{ .keyword = .InfixLeft }, "infixl", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 6 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "infixn",
            .token = try Token.init(allocator, .{ .keyword = .InfixNon }, "infixn", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 6 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "infixr",
            .token = try Token.init(allocator, .{ .keyword = .InfixRight }, "infixr", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 6 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "let",
            .token = try Token.init(allocator, .{ .keyword = .Let }, "let", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "match",
            .token = try Token.init(allocator, .{ .keyword = .Match }, "match", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 5 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "module",
            .token = try Token.init(allocator, .{ .keyword = .Module }, "module", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 6 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "on",
            .token = try Token.init(allocator, .{ .keyword = .On }, "on", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "open",
            .token = try Token.init(allocator, .{ .keyword = .Open }, "open", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 4 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "then",
            .token = try Token.init(allocator, .{ .keyword = .Then }, "then", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 4 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "type",
            .token = try Token.init(allocator, .{ .keyword = .Type }, "type", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 4 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "using",
            .token = try Token.init(allocator, .{ .keyword = .Using }, "using", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 5 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "when",
            .token = try Token.init(allocator, .{ .keyword = .When }, "when", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 4 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken(allocator);
        defer {
            token.deinit(allocator);
            case.token.deinit(allocator);
        }

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken(allocator);
        defer eof.deinit(allocator);

        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[delimiter]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const cases = [_]TestCase{
        .{
            .source = ":",
            .token = try Token.init(allocator, .{ .delimiter = .Colon }, ":", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = ",",
            .token = try Token.init(allocator, .{ .delimiter = .Comma }, ",", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = ".",
            .token = try Token.init(allocator, .{ .delimiter = .Dot }, ".", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "[",
            .token = try Token.init(allocator, .{ .delimiter = .LeftBracket }, "[", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "{",
            .token = try Token.init(allocator, .{ .delimiter = .LeftBrace }, "{", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "(",
            .token = try Token.init(allocator, .{ .delimiter = .LeftParen }, "(", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "]",
            .token = try Token.init(allocator, .{ .delimiter = .RightBracket }, "]", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "}",
            .token = try Token.init(allocator, .{ .delimiter = .RightBrace }, "}", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = ")",
            .token = try Token.init(allocator, .{ .delimiter = .RightParen }, ")", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken(allocator);
        defer {
            token.deinit(allocator);
            case.token.deinit(allocator);
        }

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken(allocator);
        defer eof.deinit(allocator);

        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[symbol]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const cases = [_]TestCase{
        .{
            .source = "->",
            .token = try Token.init(allocator, .{ .symbol = .ArrowRight }, "->", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "=>",
            .token = try Token.init(allocator, .{ .symbol = .DoubleArrowRight }, "=>", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "|",
            .token = try Token.init(allocator, .{ .symbol = .Pipe }, "|", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "_",
            .token = try Token.init(allocator, .{ .symbol = .Underscore }, "_", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken(allocator);
        defer {
            token.deinit(allocator);
            case.token.deinit(allocator);
        }

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken(allocator);
        defer eof.deinit(allocator);

        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[operator]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const cases = [_]TestCase{
        // Arithmetic
        .{
            .source = "**", // FloatExp
            .token = try Token.init(allocator, .{ .operator = "**" }, "**", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "+.", // FloatAdd
            .token = try Token.init(allocator, .{ .operator = "+." }, "+.", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "/.", // FloatDiv
            .token = try Token.init(allocator, .{ .operator = "/." }, "/.", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "*.", // FloatMul
            .token = try Token.init(allocator, .{ .operator = "*." }, "*.", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "-.", // FloatSub
            .token = try Token.init(allocator, .{ .operator = "-." }, "-.", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "^^", // IntExp
            .token = try Token.init(allocator, .{ .operator = "^^" }, "^^", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "+", // IntAdd
            .token = try Token.init(allocator, .{ .operator = "+" }, "+", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "/", // IntDiv
            .token = try Token.init(allocator, .{ .operator = "/" }, "/", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "*", // IntMul
            .token = try Token.init(allocator, .{ .operator = "*" }, "*", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "-", // IntSub
            .token = try Token.init(allocator, .{ .operator = "-" }, "-", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        // Comparison
        .{
            .source = "==", // Equality
            .token = try Token.init(allocator, .{ .operator = "==" }, "==", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = ">", // GreaterThan
            .token = try Token.init(allocator, .{ .operator = ">" }, ">", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = ">=", // GreaterThanEqual
            .token = try Token.init(allocator, .{ .operator = ">=" }, ">=", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "<", // LessThan
            .token = try Token.init(allocator, .{ .operator = "<" }, "<", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "<=", // LessThanEqual
            .token = try Token.init(allocator, .{ .operator = "<=" }, "<=", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "/=", // NotEqual
            .token = try Token.init(allocator, .{ .operator = "/=" }, "/=", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        // Logical
        .{
            .source = "&&", // LogicalAnd
            .token = try Token.init(allocator, .{ .operator = "&&" }, "&&", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "||", // LogicalOr
            .token = try Token.init(allocator, .{ .operator = "||" }, "||", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        // Other
        .{
            .source = "++", // ListConcat
            .token = try Token.init(allocator, .{ .operator = "++" }, "++", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "..", // Expand
            .token = try Token.init(allocator, .{ .operator = ".." }, "..", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "<>", // StrConcat
            .token = try Token.init(allocator, .{ .operator = "<>" }, "<>", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "=", // Equal
            .token = try Token.init(allocator, .{ .operator = "=" }, "=", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "|>", // PipeRight
            .token = try Token.init(allocator, .{ .operator = "|>" }, "|>", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = ">>=", // Bind
            .token = try Token.init(allocator, .{ .operator = ">>=" }, ">>=", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "<*>", // Apply
            .token = try Token.init(allocator, .{ .operator = "<*>" }, "<*>", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "<$>", // Fmap
            .token = try Token.init(allocator, .{ .operator = "<$>" }, "<$>", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken(allocator);
        defer {
            token.deinit(allocator);
            case.token.deinit(allocator);
        }

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify operator
        try testing.expectEqualStrings(case.token.kind.operator, token.kind.operator);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken(allocator);
        defer eof.deinit(allocator);

        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[special]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const cases = [_]TestCase{
        .{
            .source = "?",
            .token = try Token.init(allocator, .{ .special = .Hole }, "?", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken(allocator);
        defer {
            token.deinit(allocator);
            case.token.deinit(allocator);
        }

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken(allocator);
        defer eof.deinit(allocator);

        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[comment]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const cases = [_]TestCase{
        .{
            .source = "# this is a comment",
            .token = try Token.init(allocator, .{ .comment = .Regular }, "# this is a comment", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 19 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "## this is a doc comment",
            .token = try Token.init(allocator, .{ .comment = .Doc }, "## this is a doc comment", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 24 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "# 你好，世界",
            .token = try Token.init(allocator, .{ .comment = .Regular }, "# 你好，世界", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 17 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "## こんにちは",
            .token = try Token.init(allocator, .{ .comment = .Doc }, "## こんにちは", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 18 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "# 🚀 👽 💣",
            .token = try Token.init(allocator, .{ .comment = .Regular }, "# 🚀 👽 💣", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 16 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken(allocator);
        defer {
            token.deinit(allocator);
            case.token.deinit(allocator);
        }

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken(allocator);
        defer eof.deinit(allocator);

        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

// ?col locations are off!
test "[string literal]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const cases = [_]TestCase{
        .{
            .source = "\"\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"foo\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"foo\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 5 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"1\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"1\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"$\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"$\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"Backslash: \\\\\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"Backslash: \\\\\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 15 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"Double quote: \\\"Hello!\\\"\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"Double quote: \\\"Hello!\\\"\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 26 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"First line\\nSecond line\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"First line\\nSecond line\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 25 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"Column1\\tColumn2\\tColumn3\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"Column1\\tColumn2\\tColumn3\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 27 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"Carriage return\\rOverwritten text\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"Carriage return\\rOverwritten text\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 35 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{1}\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"Unicode test: \\u{1}\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 21 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{10}\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"Unicode test: \\u{10}\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 22 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{100}\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"Unicode test: \\u{100}\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 23 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{1000}\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"Unicode test: \\u{1000}\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 24 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{10000}\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"Unicode test: \\u{10000}\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 25 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{100000}\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"Unicode test: \\u{100000}\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 26 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{10FFFF}\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"Unicode test: \\u{10FFFF}\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 26 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"Unicode test: \\u{0000}\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"Unicode test: \\u{0000}\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 24 },
                .src = .{ .line = 1, .col = 2 },
            }),
        }, // edge case
        .{
            .source = "\"Unicode test: \\u{0020}\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"Unicode test: \\u{0020}\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 24 },
                .src = .{ .line = 1, .col = 2 },
            }),
        }, // edge case
        .{
            .source = "\"Unicode test: \\u{007F}\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"Unicode test: \\u{007F}\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 24 },
                .src = .{ .line = 1, .col = 2 },
            }),
        }, // edge case
        .{
            .source = "\"Unicode with extra: \\u{1234}Hello\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"Unicode with extra: \\u{1234}Hello\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 35 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"✅\"",
            .token = try Token.init(allocator, .{ .literal = .String }, "\"✅\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 5 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken(allocator);
        defer {
            token.deinit(allocator);
            case.token.deinit(allocator);
        }

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken(allocator);
        defer eof.deinit(allocator);

        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[string literal] error.CodePointOutOfRange" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const invalid_cases = [_][]const u8{
        "\"\\u{110000}\"",
        "\"\\u{D800}\"", // high surrogate
        "\"\\u{DFFF}\"", // low surrogate
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken(allocator);

        // Assertions
        // Verify error
        try testing.expectError(error.CodePointOutOfRange, result);
    }
}

test "[string literal] error.UnrecognizedStrEscapeSequence" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const invalid_cases = [_][]const u8{
        "\"\\q\"",
        "\"\\k\"",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken(allocator);

        // Assertions
        // Verify error
        try testing.expectError(error.UnrecognizedStrEscapeSequence, result);
    }
}

test "[string literal] error.InvalidUnicodeEscapeSequence" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const invalid_cases = [_][]const u8{
        "\"unicode missing digits: \\u{}\"", // Unicode escape needs at least 1 hex digit
        "\"invalid unicode: \\u{GHIJ}\"", // Unicode escape must only contain hex digits
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken(allocator);

        // Assertions
        // Verify error
        try testing.expectError(error.InvalidUnicodeEscapeSequence, result);
    }
}

test "[string literal] error.UnterminatedStrLiteral" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const invalid_cases = [_][]const u8{
        "\"no closing quote",
        "\"escape at end\\",
        "\"unicode escape at end\\u{123}",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken(allocator);

        // Assertions
        // Verify error
        try testing.expectError(error.UnterminatedStrLiteral, result);
    }
}

// locations seem off...
test "[multiline string literal]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const cases = [_]TestCase{
        .{
            .source = "\"\"\" This is a\n" ++
                "multiline string with\n" ++
                "unicode: 你好, こんにちは\n" ++
                "\"\"\"",
            .token = try Token.init(allocator, .{ .literal = .MultilineString }, "\"\"\" This is a\n" ++
                "multiline string with\n" ++
                "unicode: 你好, こんにちは\n" ++
                "\"\"\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 72 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
        .{
            .source = "\"\"\" This is a\n" ++
                "multiline string with\n" ++
                "unicode: 你好, こんにちは\n" ++
                "\"\"\"",
            .token = try Token.init(allocator, .{ .literal = .MultilineString }, "\"\"\" This is a\n" ++
                "multiline string with\n" ++
                "unicode: 你好, こんにちは\n" ++
                "\"\"\"", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 72 },
                .src = .{ .line = 1, .col = 2 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken(allocator);
        defer {
            token.deinit(allocator);
            case.token.deinit(allocator);
        }

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken(allocator);
        defer eof.deinit(allocator);

        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[multiline string literal] error.UnterminatedStrLiteral" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const invalid_cases = [_][]const u8{
        "\"\"\" This is an\n" ++
            "unterminated multiline string with\n" ++
            "unicode: 你好, こんにちは",
    };

    for (invalid_cases) |source| {
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken(allocator);

        // Assertions
        // Verify error
        try testing.expectError(error.UnterminatedStrLiteral, result);
    }
}

// locations seem off...
test "[char literal]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const cases = [_]TestCase{
        .{
            .source = "'a'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'a'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "'1'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'1'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "'$'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'$'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "'\\n'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'\\n'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "'\\t'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'\\t'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "'\\r'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'\\r'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "'\\''",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'\\''", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "'\\\\'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'\\\\'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "'\\u{1}'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'\\u{1}'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "'\\u{10}'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'\\u{10}'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "'\\u{100}'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'\\u{100}'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "'\\u{1000}'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'\\u{1000}'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "'\\u{10000}'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'\\u{10000}'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "'\\u{100000}'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'\\u{100000}'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "'\\u{10FFFF}'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'\\u{10FFFF}'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "'\\u{0000}'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'\\u{0000}'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        }, // edge case
        .{
            .source = "'\\u{0020}'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'\\u{0020}'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        }, // edge case
        .{
            .source = "'\\u{007F}'",
            .token = try Token.init(allocator, .{ .literal = .Char }, "'\\u{007F}'", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        }, // edge case
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken(allocator);
        defer {
            token.deinit(allocator);
            case.token.deinit(allocator);
        }

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken(allocator);
        defer eof.deinit(allocator);

        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[char literal] error.EmptyCharLiteral" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const source = "''";

    // Setup
    var lexer = Lexer.init(source, TEST_FILE);

    // Action
    const result = lexer.nextToken(allocator);

    // Assertions
    // Verify error
    try testing.expectError(error.EmptyCharLiteral, result);
}

test "[char literal] error.CodePointOutOfRange" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const invalid_cases = [_][]const u8{
        "'\\u{110000}'",
        "'\\u{D800}'", // high surrogate
        "'\\u{DFFF}'", // low surrogate
    };

    for (invalid_cases) |source| {
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken(allocator);

        // Assertions
        // Verify error
        try testing.expectError(error.CodePointOutOfRange, result);
    }
}

test "[char literal] error.UnrecognizedCharEscapeSequence" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const invalid_cases = [_][]const u8{
        "'\\q'",
        "'\\k'",
    };

    for (invalid_cases) |source| {
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken(allocator);

        // Assertions
        // Verify error
        try testing.expectError(error.UnrecognizedCharEscapeSequence, result);
    }
}

test "[char literal] error.MultipleCharsInLiteral" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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
        const result = lexer.nextToken(allocator);

        // Assertions
        // Verify error
        try testing.expectError(error.MultipleCharsInLiteral, result);
    }
}

test "[char literal] error.UnterminatedCharLiteral" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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
        const result = lexer.nextToken(allocator);

        // Assertions
        // Verify error
        try testing.expectError(error.UnterminatedCharLiteral, result);
    }
}

test "[char literal] error.InvalidUnicodeEscapeSequence" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const invalid_cases = [_][]const u8{
        "'\\u{}'",
        "'\\u{g}'",
    };

    for (invalid_cases) |source| {
        // Setup
        var lexer = Lexer.init(source, TEST_FILE);

        // Action
        const result = lexer.nextToken(allocator);

        // Assertions
        // Verify error
        try testing.expectError(error.InvalidUnicodeEscapeSequence, result);
    }
}

// test "[integer literal]" {
//     const cases = [_]TestCase{
//         .{
//             .source = "42",
//             .kind = .{ .literal = .Int },
//             .lexeme = "42",
//         },
//         .{
//             .source = "42_000_000",
//             .kind = .{ .literal = .Int },
//             .lexeme = "42_000_000",
//         },
//         .{
//             .source = "0b101010",
//             .kind = .{ .literal = .Int },
//             .lexeme = "0b101010",
//         },
//         .{
//             .source = "0b10_1010",
//             .kind = .{ .literal = .Int },
//             .lexeme = "0b10_1010",
//         },
//         .{
//             .source = "0o52",
//             .kind = .{ .literal = .Int },
//             .lexeme = "0o52",
//         },
//         .{
//             .source = "0o52_52",
//             .kind = .{ .literal = .Int },
//             .lexeme = "0o52_52",
//         },
//         .{
//             .source = "0x2A",
//             .kind = .{ .literal = .Int },
//             .lexeme = "0x2A",
//         },
//         .{
//             .source = "0x2A_2A",
//             .kind = .{ .literal = .Int },
//             .lexeme = "0x2A_2A",
//         },
//     };

//     for (cases) |case| {
//         // Setup
//         var lexer = Lexer.init(case.source, TEST_FILE);

//         // Action
//         const token = try lexer.nextToken();

//         // Assertions
//         // Verify the token kind matches
//         try testing.expectEqual(case.kind, token.kind);

//         // Verify the token lexeme matches
//         try testing.expectEqualStrings(case.lexeme, token.lexeme);

//         // Ensure we reached the end of the string
//         const eof = try lexer.nextToken();
//         try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
//     }
// }

// test "[integer literal] error.InvalidIntLiteral" {
//     const invalid_cases = [_][]const u8{
//         "10__000", // consecutive underscores
//         "_1000", // leading underscore
//         "1000_", // trailing underscore
//         "0x_1F", // underscore after prefix
//         "10__000.0", // fail before we even know it's a float
//         "_1000.0", // fail before we even know it's a float
//     };

//     for (invalid_cases) |source| {
//         // Setup
//         var lexer = Lexer.init(source, TEST_FILE);

//         // Action
//         const result = lexer.nextToken();

//         // Assertions
//         // Verify error
//         try testing.expectError(error.InvalidIntLiteral, result);
//     }
// }

// test "[float literal]" {
//     const cases = [_]TestCase{
//         .{
//             .source = "42.0",
//             .kind = .{ .literal = .Float },
//             .lexeme = "42.0",
//         },
//         .{
//             .source = "42_000_000.0",
//             .kind = .{ .literal = .Float },
//             .lexeme = "42_000_000.0",
//         },
//         .{
//             .source = "0.5",
//             .kind = .{ .literal = .Float },
//             .lexeme = "0.5",
//         },
//         .{
//             .source = "1.23e3",
//             .kind = .{ .literal = .Float },
//             .lexeme = "1.23e3",
//         },
//         .{
//             .source = "4.56e-2",
//             .kind = .{ .literal = .Float },
//             .lexeme = "4.56e-2",
//         },
//         .{
//             .source = "3.141_592",
//             .kind = .{ .literal = .Float },
//             .lexeme = "3.141_592",
//         },
//     };

//     for (cases) |case| {
//         // Setup
//         var lexer = Lexer.init(case.source, TEST_FILE);

//         // Action
//         const token = try lexer.nextToken();

//         // Assertions
//         // Verify the token kind matches
//         try std.testing.expectEqual(case.kind, token.kind);

//         // Verify the token lexeme matches
//         try std.testing.expectEqualStrings(case.lexeme, token.lexeme);

//         // Ensure we reached the end of the string
//         const eof = try lexer.nextToken();
//         try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
//     }
// }

// test "[float literal] error.InvalidFloat" {
//     const invalid_cases = [_][]const u8{
//         "1000._", // trailing underscore
//         ".5e3", // must begin with a digit
//     };

//     for (invalid_cases) |source| {
//         // Setup
//         var lexer = Lexer.init(source, TEST_FILE);

//         // Action
//         const result = lexer.nextToken();

//         // Assertions
//         // Verify error
//         try std.testing.expectError(error.InvalidFloatLiteral, result);
//     }
// }

test "[identifier]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const cases = [_]TestCase{
        .{
            .source = "Int",
            .token = try Token.init(allocator, .{ .identifier = .Upper }, "Int", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "Float",
            .token = try Token.init(allocator, .{ .identifier = .Upper }, "Float", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 5 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "Bool",
            .token = try Token.init(allocator, .{ .identifier = .Upper }, "Bool", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 4 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "True",
            .token = try Token.init(allocator, .{ .identifier = .Upper }, "True", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 4 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "False",
            .token = try Token.init(allocator, .{ .identifier = .Upper }, "False", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 5 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "Unit",
            .token = try Token.init(allocator, .{ .identifier = .Upper }, "Unit", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 4 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "foo",
            .token = try Token.init(allocator, .{ .identifier = .Lower }, "foo", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "foo_bar",
            .token = try Token.init(allocator, .{ .identifier = .Lower }, "foo_bar", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 7 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "_foo",
            .token = try Token.init(allocator, .{ .identifier = .Lower }, "_foo", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 4 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "A",
            .token = try Token.init(allocator, .{ .identifier = .Upper }, "A", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "a",
            .token = try Token.init(allocator, .{ .identifier = .Lower }, "a", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "_",
            .token = try Token.init(allocator, .{ .symbol = .Underscore }, "_", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "ABC123",
            .token = try Token.init(allocator, .{ .identifier = .Upper }, "ABC123", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 6 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "abc123",
            .token = try Token.init(allocator, .{ .identifier = .Lower }, "abc123", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 6 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "Foo_Bar",
            .token = try Token.init(allocator, .{ .identifier = .Upper }, "Foo_Bar", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 7 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "_foo_BAR_123",
            .token = try Token.init(allocator, .{ .identifier = .Lower }, "_foo_BAR_123", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 12 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "__foo",
            .token = try Token.init(allocator, .{ .identifier = .Lower }, "__foo", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 5 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
        .{
            .source = "foo?",
            .token = try Token.init(allocator, .{ .identifier = .Lower }, "foo?", .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 4 },
                .src = .{ .line = 1, .col = 1 },
            }),
        },
    };

    for (cases) |case| {
        var lexer = Lexer.init(case.source, TEST_FILE);

        // Action
        const token = try lexer.nextToken(allocator);
        defer {
            token.deinit(allocator);
            case.token.deinit(allocator);
        }

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(case.token.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(case.token.lexeme, token.lexeme);

        // Verify token locations
        try testing.expectEqual(case.token.loc.span.start, token.loc.span.start);
        try testing.expectEqual(case.token.loc.span.end, token.loc.span.end);
        try testing.expectEqual(case.token.loc.src.line, token.loc.src.line);
        try testing.expectEqual(case.token.loc.src.col, token.loc.src.col);

        // Ensure we reached the end of the string
        const eof = try lexer.nextToken(allocator);
        defer eof.deinit(allocator);

        try testing.expectEqual(TokenKind{ .special = .Eof }, eof.kind);
    }
}

test "[identifier] error.InvalidIdentifier" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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
        const result = lexer.nextToken(allocator);

        // Assertions
        // Verify error
        try testing.expectError(error.InvalidIdentifier, result);
    }
}

test "[type variant]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const source = "type FooBar = | Foo | Bar";

    const expected_tokens = [_]Token{
        try Token.init(allocator, .{ .keyword = .Type }, "type", .{
            .filename = TEST_FILE,
            .span = .{ .start = 0, .end = 4 },
            .src = .{ .line = 1, .col = 1 },
        }),
        try Token.init(allocator, .{ .identifier = .Upper }, "FooBar", .{
            .filename = TEST_FILE,
            .span = .{ .start = 5, .end = 11 },
            .src = .{ .line = 1, .col = 6 },
        }),
        try Token.init(allocator, .{ .operator = "=" }, "=", .{
            .filename = TEST_FILE,
            .span = .{ .start = 12, .end = 13 },
            .src = .{ .line = 1, .col = 13 },
        }),
        try Token.init(allocator, .{ .symbol = .Pipe }, "|", .{
            .filename = TEST_FILE,
            .span = .{ .start = 14, .end = 15 },
            .src = .{ .line = 1, .col = 15 },
        }),
        try Token.init(allocator, .{ .identifier = .Upper }, "Foo", .{
            .filename = TEST_FILE,
            .span = .{ .start = 16, .end = 19 },
            .src = .{ .line = 1, .col = 17 },
        }),
        try Token.init(allocator, .{ .symbol = .Pipe }, "|", .{
            .filename = TEST_FILE,
            .span = .{ .start = 20, .end = 21 },
            .src = .{ .line = 1, .col = 21 },
        }),
        try Token.init(allocator, .{ .identifier = .Upper }, "Bar", .{
            .filename = TEST_FILE,
            .span = .{ .start = 22, .end = 25 },
            .src = .{ .line = 1, .col = 23 },
        }),
        try Token.init(allocator, .{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .span = .{ .start = 25, .end = 25 },
            .src = .{ .line = 1, .col = 26 },
        }),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken(allocator);
        defer {
            token.deinit(allocator);
            expected.deinit(allocator);
        }

        std.debug.print("{any}\n", .{token});

        // Assertions
        // Verify the token kind matches
        try testing.expectEqual(expected.kind, token.kind);

        // Verify the token lexeme matches
        try testing.expectEqualStrings(expected.lexeme, token.lexeme);

        // Verify span matches lexeme length (your original intent)
        // try testing.expectEqual(token.loc.span.end - token.loc.span.start, token.lexeme.len);

        // Verify token locations
        try testing.expectEqual(expected.loc.span.start, token.loc.span.start);
        try testing.expectEqual(expected.loc.span.end, token.loc.span.end);
        try testing.expectEqual(expected.loc.src.line, token.loc.src.line);
        try testing.expectEqual(expected.loc.src.col, token.loc.src.col);
    }
}

test "[type alias]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const source = "type alias Seconds = Int";

    const expected_tokens = [_]Token{
        try Token.init(allocator, .{ .keyword = .Type }, "type", .{
            .filename = TEST_FILE,
            .span = .{ .start = 0, .end = 4 },
            .src = .{ .line = 1, .col = 1 },
        }),
        try Token.init(allocator, .{ .keyword = .Alias }, "alias", .{
            .filename = TEST_FILE,
            .span = .{ .start = 5, .end = 10 },
            .src = .{ .line = 1, .col = 6 },
        }),
        try Token.init(allocator, .{ .identifier = .Upper }, "Seconds", .{
            .filename = TEST_FILE,
            .span = .{ .start = 11, .end = 18 },
            .src = .{ .line = 1, .col = 12 },
        }),
        try Token.init(allocator, .{ .operator = "=" }, "=", .{
            .filename = TEST_FILE,
            .span = .{ .start = 19, .end = 20 },
            .src = .{ .line = 1, .col = 20 },
        }),
        try Token.init(allocator, .{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .span = .{ .start = 21, .end = 24 },
            .src = .{ .line = 1, .col = 22 },
        }),
        try Token.init(allocator, .{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .span = .{ .start = 24, .end = 24 },
            .src = .{ .line = 1, .col = 25 },
        }),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken(allocator);
        defer {
            token.deinit(allocator);
            expected.deinit(allocator);
        }

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
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const source = "type FooBar = { foo : Int, bar : String }";

    const expected_tokens = [_]Token{
        try Token.init(allocator, .{ .keyword = .Type }, "type", .{
            .filename = TEST_FILE,
            .span = .{ .start = 0, .end = 4 },
            .src = .{ .line = 1, .col = 1 },
        }),
        try Token.init(allocator, .{ .identifier = .Upper }, "FooBar", .{
            .filename = TEST_FILE,
            .span = .{ .start = 5, .end = 11 },
            .src = .{ .line = 1, .col = 6 },
        }),
        try Token.init(allocator, .{ .operator = "=" }, "=", .{
            .filename = TEST_FILE,
            .span = .{ .start = 12, .end = 13 },
            .src = .{ .line = 1, .col = 13 },
        }),
        try Token.init(allocator, .{ .delimiter = .LeftBrace }, "{", .{
            .filename = TEST_FILE,
            .span = .{ .start = 14, .end = 15 },
            .src = .{ .line = 1, .col = 15 },
        }),
        try Token.init(allocator, .{ .identifier = .Lower }, "foo", .{
            .filename = TEST_FILE,
            .span = .{ .start = 16, .end = 19 },
            .src = .{ .line = 1, .col = 17 },
        }),
        try Token.init(allocator, .{ .delimiter = .Colon }, ":", .{
            .filename = TEST_FILE,
            .span = .{ .start = 20, .end = 21 },
            .src = .{ .line = 1, .col = 21 },
        }),
        try Token.init(allocator, .{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .span = .{ .start = 22, .end = 25 },
            .src = .{ .line = 1, .col = 23 },
        }),
        try Token.init(allocator, .{ .delimiter = .Comma }, ",", .{
            .filename = TEST_FILE,
            .span = .{ .start = 25, .end = 26 },
            .src = .{ .line = 1, .col = 26 },
        }),
        try Token.init(allocator, .{ .identifier = .Lower }, "bar", .{
            .filename = TEST_FILE,
            .span = .{ .start = 27, .end = 30 },
            .src = .{ .line = 1, .col = 28 },
        }),
        try Token.init(allocator, .{ .delimiter = .Colon }, ":", .{
            .filename = TEST_FILE,
            .span = .{ .start = 31, .end = 32 },
            .src = .{ .line = 1, .col = 32 },
        }),
        try Token.init(allocator, .{ .identifier = .Upper }, "String", .{
            .filename = TEST_FILE,
            .span = .{ .start = 33, .end = 39 },
            .src = .{ .line = 1, .col = 34 },
        }),
        try Token.init(allocator, .{ .delimiter = .RightBrace }, "}", .{
            .filename = TEST_FILE,
            .span = .{ .start = 40, .end = 41 },
            .src = .{ .line = 1, .col = 41 },
        }),
        try Token.init(allocator, .{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .span = .{ .start = 41, .end = 41 },
            .src = .{ .line = 1, .col = 42 },
        }),
    };

    var lexer = Lexer.init(source, TEST_FILE);

    for (expected_tokens) |expected| {
        // Action
        const token = try lexer.nextToken(allocator);
        defer {
            token.deinit(allocator);
            expected.deinit(allocator);
        }

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
        Token.init(.{ .operator = ".." }, "..", .{
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

test "[top level function declaration]" {
    const source = "let add(x : Int, y : Int) -> Int = x + y";

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
        Token.init(.{ .delimiter = .LeftParen }, "(", .{
            .filename = TEST_FILE,
            .span = .{ .start = 7, .end = 8 },
            .src = .{ .line = 1, .col = 8 },
        }),
        Token.init(.{ .identifier = .Lower }, "x", .{
            .filename = TEST_FILE,
            .span = .{ .start = 8, .end = 9 },
            .src = .{ .line = 1, .col = 9 },
        }),
        Token.init(.{ .delimiter = .Colon }, ":", .{
            .filename = TEST_FILE,
            .span = .{ .start = 10, .end = 11 },
            .src = .{ .line = 1, .col = 11 },
        }),
        Token.init(.{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .span = .{ .start = 12, .end = 15 },
            .src = .{ .line = 1, .col = 13 },
        }),
        Token.init(.{ .delimiter = .Comma }, ",", .{
            .filename = TEST_FILE,
            .span = .{ .start = 15, .end = 16 },
            .src = .{ .line = 1, .col = 16 },
        }),
        Token.init(.{ .identifier = .Lower }, "y", .{
            .filename = TEST_FILE,
            .span = .{ .start = 17, .end = 18 },
            .src = .{ .line = 1, .col = 18 },
        }),
        Token.init(.{ .delimiter = .Colon }, ":", .{
            .filename = TEST_FILE,
            .span = .{ .start = 19, .end = 20 },
            .src = .{ .line = 1, .col = 20 },
        }),
        Token.init(.{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .span = .{ .start = 21, .end = 24 },
            .src = .{ .line = 1, .col = 22 },
        }),
        Token.init(.{ .delimiter = .RightParen }, ")", .{
            .filename = TEST_FILE,
            .span = .{ .start = 24, .end = 25 },
            .src = .{ .line = 1, .col = 25 },
        }),
        Token.init(.{ .symbol = .ArrowRight }, "->", .{
            .filename = TEST_FILE,
            .span = .{ .start = 26, .end = 28 },
            .src = .{ .line = 1, .col = 27 },
        }),
        Token.init(.{ .identifier = .Upper }, "Int", .{
            .filename = TEST_FILE,
            .span = .{ .start = 29, .end = 32 },
            .src = .{ .line = 1, .col = 30 },
        }),
        Token.init(.{ .operator = "=" }, "=", .{
            .filename = TEST_FILE,
            .span = .{ .start = 33, .end = 34 },
            .src = .{ .line = 1, .col = 34 },
        }),
        Token.init(.{ .identifier = .Lower }, "x", .{
            .filename = TEST_FILE,
            .span = .{ .start = 35, .end = 36 },
            .src = .{ .line = 1, .col = 36 },
        }),
        Token.init(.{ .operator = "+" }, "+", .{
            .filename = TEST_FILE,
            .span = .{ .start = 37, .end = 38 },
            .src = .{ .line = 1, .col = 38 },
        }),
        Token.init(.{ .identifier = .Lower }, "y", .{
            .filename = TEST_FILE,
            .span = .{ .start = 39, .end = 40 },
            .src = .{ .line = 1, .col = 40 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .span = .{ .start = 40, .end = 40 },
            .src = .{ .line = 1, .col = 41 },
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

test "[local let block]" {
    const source = "let x : Int = 42";

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
        Token.init(.{ .operator = "=" }, "=", .{
            .filename = TEST_FILE,
            .span = .{ .start = 12, .end = 13 },
            .src = .{ .line = 1, .col = 13 },
        }),
        Token.init(.{ .literal = .Int }, "42", .{
            .filename = TEST_FILE,
            .span = .{ .start = 14, .end = 16 },
            .src = .{ .line = 1, .col = 15 },
        }),
        Token.init(.{ .special = .Eof }, "", .{
            .filename = TEST_FILE,
            .span = .{ .start = 16, .end = 16 },
            .src = .{ .line = 1, .col = 17 },
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
        Token.init(.{ .operator = "==" }, "==", .{
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
