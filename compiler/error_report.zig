const std = @import("std");

const lexer = @import("lexer.zig");
const style = @import("style.zig");
const LexerError = lexer.LexerError;
const TokenLoc = lexer.TokenLoc;
const Color = style.Color;

pub const ErrorReport = struct {
    problem: []const u8,
    hint: []const u8,
    snippet: []const u8,
    filename: []const u8,
    line: usize,
    col: usize,

    // The span of characters to highlight
    span_start: usize,
    span_end: usize,

    pub fn format(self: ErrorReport, raw_writer: anytype) !void {
        const writer = style.ColorWriter.init(raw_writer);

        try writer.styled(Color.Dim, "\n——");
        try writer.styled(Color.Blue, " SYNTAX ERROR ");
        try writer.styled(Color.Dim, "—————————————————————————————————————————————\n\n");

        try writer.styled(Color.Bold, self.problem);
        try writer.plain("\n\n");

        try writer.plain(" │\n");
        try writer.format("{d}│ {s}\n", .{ self.line, self.snippet });
        try writer.plain(" │ ");

        // Add spaces up to the error start position
        var i: usize = 0;
        while (i < self.col - 1) : (i += 1) {
            try writer.plain(" ");
        }

        const span_length = self.span_end - self.span_start;
        try writer.styled(Color.Red, "^");
        var j: usize = 1;
        while (j < span_length) : (j += 1) {
            try writer.styled(Color.Red, "^");
        }

        try writer.plain("\n └─ ");
        try writer.plain(self.filename);
        try writer.format(" (line: {d}, column: {d})\n\n", .{ self.line, self.col });

        if (self.hint.len > 0) {
            try writer.styled(Color.Cyan, "Hint: ");
            try writer.plain(self.hint);
            try writer.plain("\n\n");
        }
    }
};

fn toProblem(err: LexerError) []const u8 {
    return switch (err) {
        error.CodePointOutOfRange => "Found invalid Unicode code point:",
        error.EmptyCharLiteral => "Found empty character literal:",
        error.InvalidIdentifier => "Found invalid identifier:",
        error.InvalidIntLiteral => "Found invalid number:",
        error.InvalidUnicodeEscapeSequence => "Found invalid Unicode escape sequence:",
        error.MultipleCharsInLiteral => "Found multiple characters in character literal:",
        error.UnrecognizedCharEscapeSequence => "Unrecognized escape sequence:",
        error.UnrecognizedStrEscapeSequence => "Unrecognized escape sequence:",
        error.UnterminatedCharLiteral => "Found unterminated character literal:",
        error.UnterminatedStrLiteral => "Found unterminated string literal:",
    };
}

fn toHint(err: LexerError) []const u8 {
    return switch (err) {
        error.CodePointOutOfRange => "The Unicode value must be between 0x0000 and 0x10FFFF, excluding 0xD800–0xDFFF (surrogates).",
        error.EmptyCharLiteral => "Character literals must contain exactly one character, e.g., 'a', '1', or '\\n'.",
        error.InvalidIdentifier =>
        \\Identifiers must follow these rules:
        \\
        \\    - Start with a letter or an underscore (_)
        \\    - Continue with letters, numbers, or underscores (_)
        \\    - Optionally end with a question mark (?)
        \\
        \\Examples of valid identifiers:
        \\
        \\    my_variable
        \\    _temporaryValue
        \\    myFunction123
        \\    is_valid?
        ,
        error.InvalidIntLiteral =>
        \\Numbers are recognized in the following formats:
        \\
        \\    42
        \\    42_000
        \\    3.14
        \\    6.022e23
        \\    0b101010
        \\    0o52
        \\    0x2A
        \\
        \\So is there a way to write it like one of those?
        ,
        error.InvalidUnicodeEscapeSequence => "Unicode escape sequences must be in the format \\uXXXXXX where X is a valid hexadecimal digit.",
        error.MultipleCharsInLiteral => "Use string literals (\"\") for multiple characters or escape sequences for special characters.",
        error.UnrecognizedCharEscapeSequence =>
        \\In character literals, the only valid escape sequences are:
        \\
        \\    \n  - newline
        \\    \r  - carriage return
        \\    \t  - tab
        \\    \\  - backslash
        \\    \'  - single quote
        \\    \u  - Unicode escape (e.g., \u0061 for 'a')
        ,
        error.UnrecognizedStrEscapeSequence =>
        \\In string literals, the only valid escape sequences are:
        \\
        \\    \n  - newline
        \\    \r  - carriage return
        \\    \t  - tab
        \\    \b  - backspace
        \\    \\  - backslash
        \\    \"  - double quote
        \\    \u  - Unicode escape (e.g., \u0061 for "a")
        ,
        error.UnterminatedCharLiteral => "Character literals must be closed with a single quote ('). Did you forget the closing quote?",
        error.UnterminatedStrLiteral => "String literals must be closed with a double quote (\"). Did you forget the closing quote?",
    };
}

pub fn createErrorReport(err: LexerError, loc: TokenLoc, source: []const u8) ErrorReport {
    const problem = toProblem(err);
    const hint = toHint(err);

    var line_start: usize = loc.buf.start;
    while (line_start > 0 and source[line_start - 1] != '\n') {
        line_start -= 1;
    }

    var line_end: usize = loc.buf.end;
    while (line_end < source.len and source[line_end] != '\n') {
        line_end += 1;
    }

    return ErrorReport{
        .problem = problem,
        .hint = hint,
        .snippet = source[line_start..line_end],
        .filename = loc.filename,
        .line = loc.src.line,
        .col = loc.src.col,
        .span_start = loc.buf.start - line_start,
        .span_end = loc.buf.end - line_start,
    };
}
