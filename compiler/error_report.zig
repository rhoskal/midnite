const std = @import("std");

const lexer = @import("lexer.zig");
const style = @import("style.zig");
const LexerError = lexer.LexerError;
const TokenLoc = lexer.TokenLoc;
const Color = style.Color;

pub const ErrorReport = struct {
    message: []const u8,
    snippet: []const u8,
    filename: []const u8,
    line: usize,
    col: usize,

    // The span of characters to highlight
    span_start: usize,
    span_end: usize,

    pub fn format(self: ErrorReport, raw_writer: anytype) !void {
        const writer = style.ColorWriter.init(raw_writer);

        try writer.plain("\n——");
        try writer.styled(Color.Blue, " SYNTAX ERROR ");
        try writer.plain("—————————————————————————————————————————————\n\n");

        try writer.styled(Color.Bold, self.message);
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
    }
};

pub fn createErrorReport(err: LexerError, loc: TokenLoc, source: []const u8) ErrorReport {
    const message = switch (err) {
        error.CodePointOutOfRange => "Unicode code point out of range",
        error.EmptyCharLiteral => "Character literals cannot be empty",
        error.InvalidIdentifier => "Invalid identifier",
        error.InvalidIntLiteral => "Invalid integer literal",
        error.InvalidUnicodeEscapeSequence => "Invalid Unicode escape sequence",
        error.MultipleCharsInLiteral => "Character literal can only contain one character",
        error.UnrecognizedEscapeSequence => "Unrecognized escape sequence",
        error.UnterminatedCharLiteral => "Unterminated character literal",
        error.UnterminatedStrLiteral => "Unterminated string literal",
    };

    var line_start: usize = loc.buf.start;
    while (line_start > 0 and source[line_start - 1] != '\n') {
        line_start -= 1;
    }

    var line_end: usize = loc.buf.end;
    while (line_end < source.len and source[line_end] != '\n') {
        line_end += 1;
    }

    return ErrorReport{
        .message = message,
        .snippet = source[line_start..line_end],
        .filename = loc.filename,
        .line = loc.src.line,
        .col = loc.src.col,
        .span_start = loc.buf.start - line_start,
        .span_end = loc.buf.end - line_start,
    };
}
