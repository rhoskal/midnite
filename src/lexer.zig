const std = @import("std");

pub const Token = struct {
    kind: Kind,
    lexeme: []const u8,

    pub const Kind = enum {
        // Coments
        COMMENT,
        DOC_COMMENT,
        // Keywords
        ALIAS,
        AS,
        ELSE,
        END,
        EOF,
        EXPOSING,
        FALSE,
        FOREIGN,
        HIDING,
        IF,
        INCLUDE,
        INFIX_LEFT,
        INFIX_NON,
        INFIX_RIGHT,
        LET,
        MATCH,
        MODULE,
        ON,
        OPEN,
        RENAMING,
        THEN,
        TO,
        TRUE,
        TYPE,
        UNIT,
        USING,
        WHEN,
        // Symbols
        BACKSLASH,
        BRACE_LEFT,
        BRACE_RIGHT,
        BRACK_LEFT,
        BRACK_RIGHT,
        COLON,
        COMMA,
        DOT,
        DOT_DOT,
        EQUAL,
        FAT_ARROW,
        PAREN_LEFT,
        PAREN_RIGHT,
        PIPE,
        RIGHT_ARROW,
        UNDERSCORE,
        // Literals
        LIT_INT,
    };
};

const Keyword = struct {
    text: []const u8,
    kind: Token.Kind,
};

const Symbol = struct {
    text: []const u8,
    kind: Token.Kind,
};

const IntegerBase = enum {
    Base2,
    Base8,
    Base10,
    Base16,
};

const ParseIntegerResult = struct {
    len: usize,
    is_negative: bool,
    kind: IntegerBase,
};

const keywords = [_]Keyword{
    .{ .text = "alias", .kind = .ALIAS },
    .{ .text = "as", .kind = .AS },
    .{ .text = "else", .kind = .ELSE },
    .{ .text = "end", .kind = .END },
    .{ .text = "exposing", .kind = .EXPOSING },
    .{ .text = "foreign", .kind = .FOREIGN },
    .{ .text = "False", .kind = .FALSE },
    .{ .text = "hiding", .kind = .HIDING },
    .{ .text = "if", .kind = .IF },
    .{ .text = "include", .kind = .INCLUDE },
    .{ .text = "infixl", .kind = .INFIX_LEFT },
    .{ .text = "infixn", .kind = .INFIX_NON },
    .{ .text = "infixr", .kind = .INFIX_RIGHT },
    .{ .text = "let", .kind = .LET },
    .{ .text = "match", .kind = .MATCH },
    .{ .text = "module", .kind = .MODULE },
    .{ .text = "on", .kind = .ON },
    .{ .text = "open", .kind = .OPEN },
    .{ .text = "renaming", .kind = .RENAMING },
    .{ .text = "then", .kind = .THEN },
    .{ .text = "to", .kind = .TO },
    .{ .text = "True", .kind = .TRUE },
    .{ .text = "type", .kind = .TYPE },
    .{ .text = "Unit", .kind = .UNIT },
    .{ .text = "using", .kind = .USING },
    .{ .text = "when", .kind = .WHEN },
};

const symbols = [_]Symbol{
    .{ .text = "\\", .kind = .BACKSLASH },
    .{ .text = "{", .kind = .BRACE_LEFT },
    .{ .text = "}", .kind = .BRACE_RIGHT },
    .{ .text = "[", .kind = .BRACK_LEFT },
    .{ .text = "]", .kind = .BRACK_RIGHT },
    .{ .text = ":", .kind = .COLON },
    .{ .text = ",", .kind = .COMMA },
    .{ .text = "..", .kind = .DOT_DOT },
    .{ .text = ".", .kind = .DOT },
    .{ .text = "=>", .kind = .FAT_ARROW },
    .{ .text = "=", .kind = .EQUAL },
    .{ .text = "(", .kind = .PAREN_LEFT },
    .{ .text = ")", .kind = .PAREN_RIGHT },
    .{ .text = "|", .kind = .PIPE },
    .{ .text = "->", .kind = .RIGHT_ARROW },
    .{ .text = "_", .kind = .UNDERSCORE },
};

fn parseDocComment(source: []const u8, i: usize) ?usize {
    if (!std.mem.startsWith(u8, source[i..], "##")) return null;

    var len: usize = 0;
    while (i + len < source.len and source[i + len] != '\n') : (len += 1) {}

    return len;
}

fn parseLineComment(source: []const u8, i: usize) ?usize {
    if (source[i] != '#' or std.mem.startsWith(u8, source[i..], "##")) return null;

    var len: usize = 0;
    while (i + len < source.len and source[i + len] != '\n') : (len += 1) {}

    return len;
}

fn matchKeyword(source: []const u8, i: usize) ?Keyword {
    for (keywords) |kw| {
        if (std.mem.startsWith(u8, source[i..], kw.text)) {
            return kw;
        }
    }

    return null;
}

fn matchSymbol(source: []const u8, i: usize) ?Symbol {
    for (symbols) |sym| {
        if (std.mem.startsWith(u8, source[i..], sym.text)) {
            return sym;
        }
    }

    return null;
}

fn parseHexDigit(c: u8) bool {
    return std.ascii.isDigit(c) or
        (c >= 'a' and c <= 'f') or
        (c >= 'A' and c <= 'F');
}

fn parseBinDigit(c: u8) bool {
    return c == '0' or c == '1';
}

fn parseOctDigit(c: u8) bool {
    return c >= '0' and c <= '7';
}

fn parseBase(comptime isValid: fn (u8) bool, source: []const u8, start: usize) ?usize {
    var length: usize = 0;
    var has_digit = false;

    while (start + length < source.len) {
        const c = source[start + length];

        if (isValid(c)) {
            has_digit = true;
            length += 1;
        } else if (c == '_' and has_digit and
            start + length + 1 < source.len and
            isValid(source[start + length + 1]))
        {
            length += 1;
        } else {
            break;
        }
    }

    return if (has_digit) length else null;
}

fn parseIntLiteral(source: []const u8, i: usize) ?ParseIntegerResult {
    var is_negative = false;
    var start = i;

    if (i < source.len and source[i] == '-') {
        is_negative = true;
        start += 1;
    }

    if (start + 2 < source.len) {
        if (source[start] == '0') {
            switch (source[start + 1]) {
                'x' => if (parseBase(parseHexDigit, source, start + 2)) |len| {
                    return .{
                        .len = len + 2 + if (is_negative) @as(usize, 1) else 0,
                        .is_negative = is_negative,
                        .kind = .Base16,
                    };
                },
                'o' => if (parseBase(parseOctDigit, source, start + 2)) |len| {
                    return .{
                        .len = len + 2 + if (is_negative) @as(usize, 1) else 0,
                        .is_negative = is_negative,
                        .kind = .Base8,
                    };
                },
                'b' => if (parseBase(parseBinDigit, source, start + 2)) |len| {
                    return .{
                        .len = len + 2 + if (is_negative) @as(usize, 1) else 0,
                        .is_negative = is_negative,
                        .kind = .Base2,
                    };
                },
                else => {},
            }
        }
    }

    if (parseBase(std.ascii.isDigit, source, start)) |len| {
        return .{
            .len = len + if (is_negative) @as(usize, 1) else 0,
            .is_negative = is_negative,
            .kind = .Base10,
        };
    }

    return null;
}

pub fn tokenize(source: []const u8) ![]Token {
    var tokens = std.ArrayList(Token).init(std.heap.page_allocator);
    defer tokens.deinit();

    var i: usize = 0;
    while (i < source.len) {
        if (parseDocComment(source, i)) |len| {
            try tokens.append(.{ .kind = .DOC_COMMENT, .lexeme = source[i .. i + len] });
            i += len;
        } else if (parseLineComment(source, i)) |len| {
            try tokens.append(.{ .kind = .COMMENT, .lexeme = source[i .. i + len] });
            i += len;
        } else if (matchKeyword(source, i)) |kw| {
            try tokens.append(.{ .kind = kw.kind, .lexeme = source[i .. i + kw.text.len] });
            i += kw.text.len;
        } else if (matchSymbol(source, i)) |sym| {
            try tokens.append(.{ .kind = sym.kind, .lexeme = source[i .. i + sym.text.len] });
            i += sym.text.len;
        } else if (parseIntLiteral(source, i)) |result| {
            try tokens.append(.{ .kind = .LIT_INT, .lexeme = source[i .. i + result.len] });
            i += result.len;
        } else if (std.ascii.isWhitespace(source[i])) {
            // Skip all whitespace characters
            i += 1;
            continue;
        } else {
            std.debug.print("Invalid character at position {}: '{c}' (byte: {d})\n", .{
                i, source[i], source[i],
            });

            return error.InvalidCharacter;
        }
    }

    try tokens.append(.{ .kind = .EOF, .lexeme = "" });

    return tokens.toOwnedSlice();
}

test "line comments" {
    const source = "# line comment";
    const tokens = try tokenize(source);

    try std.testing.expect(tokens[0].kind == .COMMENT);
    try std.testing.expect(tokens[1].kind == .EOF);
    try std.testing.expect(tokens.len == 2);
}

test "doc comments" {
    const source = "## doc comment";
    const tokens = try tokenize(source);

    try std.testing.expect(tokens[0].kind == .DOC_COMMENT);
    try std.testing.expect(tokens[1].kind == .EOF);
    try std.testing.expect(tokens.len == 2);
}

test "module keywords" {
    const source = "module exposing as end";
    const tokens = try tokenize(source);

    try std.testing.expect(tokens[0].kind == .MODULE);
    try std.testing.expect(tokens[1].kind == .EXPOSING);
    try std.testing.expect(tokens[2].kind == .AS);
    try std.testing.expect(tokens[3].kind == .END);
    try std.testing.expect(tokens[4].kind == .EOF);
    try std.testing.expect(tokens.len == 5);
}

test "infix keywords" {
    const source = "infixn infixl infixr";
    const tokens = try tokenize(source);

    try std.testing.expect(tokens[0].kind == .INFIX_NON);
    try std.testing.expect(tokens[1].kind == .INFIX_LEFT);
    try std.testing.expect(tokens[2].kind == .INFIX_RIGHT);
    try std.testing.expect(tokens[3].kind == .EOF);
    try std.testing.expect(tokens.len == 4);
}

test "import keywords" {
    const source = "open using renaming hiding include to";
    const tokens = try tokenize(source);

    try std.testing.expect(tokens[0].kind == .OPEN);
    try std.testing.expect(tokens[1].kind == .USING);
    try std.testing.expect(tokens[2].kind == .RENAMING);
    try std.testing.expect(tokens[3].kind == .HIDING);
    try std.testing.expect(tokens[4].kind == .INCLUDE);
    try std.testing.expect(tokens[5].kind == .TO);
    try std.testing.expect(tokens[6].kind == .EOF);
    try std.testing.expect(tokens.len == 7);
}

test "ffi keywords" {
    const source = "foreign";
    const tokens = try tokenize(source);

    try std.testing.expect(tokens[0].kind == .FOREIGN);
    try std.testing.expect(tokens[1].kind == .EOF);
    try std.testing.expect(tokens.len == 2);
}

test "control flow keywords" {
    const source = "if then else match on when";
    const tokens = try tokenize(source);

    try std.testing.expect(tokens[0].kind == .IF);
    try std.testing.expect(tokens[1].kind == .THEN);
    try std.testing.expect(tokens[2].kind == .ELSE);
    try std.testing.expect(tokens[3].kind == .MATCH);
    try std.testing.expect(tokens[4].kind == .ON);
    try std.testing.expect(tokens[5].kind == .WHEN);
    try std.testing.expect(tokens[6].kind == .EOF);
    try std.testing.expect(tokens.len == 7);
}

test "type keywords" {
    const source = "type alias";
    const tokens = try tokenize(source);

    try std.testing.expect(tokens[0].kind == .TYPE);
    try std.testing.expect(tokens[1].kind == .ALIAS);
    try std.testing.expect(tokens[2].kind == .EOF);
    try std.testing.expect(tokens.len == 3);
}

test "value keywords" {
    const source = "True False Unit";
    const tokens = try tokenize(source);

    try std.testing.expect(tokens[0].kind == .TRUE);
    try std.testing.expect(tokens[1].kind == .FALSE);
    try std.testing.expect(tokens[2].kind == .UNIT);
    try std.testing.expect(tokens[3].kind == .EOF);
    try std.testing.expect(tokens.len == 4);
}

test "function keywords" {
    const source = "let";
    const tokens = try tokenize(source);

    try std.testing.expect(tokens[0].kind == .LET);
    try std.testing.expect(tokens[1].kind == .EOF);
    try std.testing.expect(tokens.len == 2);
}

test "int literals" {
    const source = "42 -42 1_000_000 0x4F 0b00011 0o61";
    const tokens = try tokenize(source);

    try std.testing.expect(tokens[0].kind == .LIT_INT);
    try std.testing.expect(tokens[1].kind == .LIT_INT);
    try std.testing.expect(tokens[2].kind == .LIT_INT);
    try std.testing.expect(tokens[3].kind == .LIT_INT);
    try std.testing.expect(tokens[4].kind == .LIT_INT);
    try std.testing.expect(tokens[5].kind == .LIT_INT);
    try std.testing.expect(tokens[6].kind == .EOF);
    try std.testing.expect(tokens.len == 7);
}

test "symbols" {
    const source = "() {} [] : , . .. -> => | = _ \\";
    const tokens = try tokenize(source);

    try std.testing.expect(tokens[0].kind == .PAREN_LEFT);
    try std.testing.expect(tokens[1].kind == .PAREN_RIGHT);
    try std.testing.expect(tokens[2].kind == .BRACE_LEFT);
    try std.testing.expect(tokens[3].kind == .BRACE_RIGHT);
    try std.testing.expect(tokens[4].kind == .BRACK_LEFT);
    try std.testing.expect(tokens[5].kind == .BRACK_RIGHT);
    try std.testing.expect(tokens[6].kind == .COLON);
    try std.testing.expect(tokens[7].kind == .COMMA);
    try std.testing.expect(tokens[8].kind == .DOT);
    try std.testing.expect(tokens[9].kind == .DOT_DOT);
    try std.testing.expect(tokens[10].kind == .RIGHT_ARROW);
    try std.testing.expect(tokens[11].kind == .FAT_ARROW);
    try std.testing.expect(tokens[12].kind == .PIPE);
    try std.testing.expect(tokens[13].kind == .EQUAL);
    try std.testing.expect(tokens[14].kind == .UNDERSCORE);
    try std.testing.expect(tokens[15].kind == .BACKSLASH);
    try std.testing.expect(tokens[16].kind == .EOF);
    try std.testing.expect(tokens.len == 17);
}
