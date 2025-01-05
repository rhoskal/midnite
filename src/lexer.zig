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
        // Special Symbols
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

const Symbol = struct {
    text: []const u8,
    kind: Token.Kind,
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

fn parseIntLiteral(source: []const u8, i: usize) ?struct { len: usize, is_negative: bool } {
    var is_negative = false;
    var start = i;

    if (i < source.len and source[i] == '-') {
        is_negative = true;
        start += 1;
    }

    var length: usize = 0;
    var has_digit = false;

    while (start + length < source.len) {
        const c = source[start + length];
        if (std.ascii.isDigit(c)) {
            has_digit = true;
            length += 1;
        } else if (c == '_' and has_digit and
            start + length + 1 < source.len and
            std.ascii.isDigit(source[start + length + 1]))
        {
            length += 1;
        } else {
            break;
        }
    }

    return if (has_digit) .{
        .len = length + if (is_negative) @as(usize, 1) else 0,
        .is_negative = is_negative,
    } else null;
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
