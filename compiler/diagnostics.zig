const std = @import("std");

const lexer = @import("lexer.zig");
const term = @import("terminal.zig");

pub const Diagnostic = struct {
    problem: []const u8,
    hint: []const u8,
    snippet: []const u8,
    loc: lexer.TokenLoc,

    /// Creates a new ErrorReport from a LexerError and location information.
    pub fn create(err: lexer.LexerError, loc: lexer.TokenLoc, source: []const u8) Diagnostic {
        const problem = getProblemMessage(err);
        const hint = getHintMessage(err);

        var line_start: usize = loc.buf.start;
        while (line_start > 0 and source[line_start - 1] != '\n') {
            line_start -= 1;
        }

        var line_end: usize = loc.buf.end;
        while (line_end < source.len and source[line_end] != '\n') {
            line_end += 1;
        }

        return Diagnostic{
            .problem = problem,
            .hint = hint,
            .snippet = source[line_start..line_end],
            .loc = loc,
        };
    }

    /// Maps a LexerError to its user-facing problem message.
    fn getProblemMessage(err: lexer.LexerError) []const u8 {
        return switch (err) {
            error.CodePointOutOfRange => "Found invalid Unicode code point:",
            error.EmptyCharLiteral => "Found empty character literal:",
            error.InvalidFloatLiteral => "Found invalid number:",
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

    /// Maps a LexerError to its help text.
    fn getHintMessage(err: lexer.LexerError) []const u8 {
        return switch (err) {
            error.CodePointOutOfRange => "The Unicode value must be between 0x0000 and 0x10FFFF, excluding 0xD800–0xDFFF (surrogates).",
            error.EmptyCharLiteral => "Character literals must contain exactly one character, e.g., 'a', '1', or '\\n'.",
            error.InvalidFloatLiteral =>
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
            error.InvalidUnicodeEscapeSequence => "Unicode escape sequences must be in the format \\u{XXXXXX} where X is a valid hexadecimal digit.",
            error.MultipleCharsInLiteral => "Use string literals (\"\") for multiple characters or escape sequences for special characters.",
            error.UnrecognizedCharEscapeSequence =>
            \\In character literals, the only valid escape sequences are:
            \\
            \\    \n  - newline
            \\    \r  - carriage return
            \\    \t  - tab
            \\    \\  - backslash
            \\    \'  - single quote
            \\    \u  - Unicode escape (e.g., \u{0061} for 'a')
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
            \\    \u  - Unicode escape (e.g., \u{0061} for "a")
            ,
            error.UnterminatedCharLiteral => "Character literals must be closed with a single quote ('). Did you forget the closing quote?",
            error.UnterminatedStrLiteral => "String literals must be closed with a double quote (\"). Did you forget the closing quote?",
        };
    }

    const HEADER_DASH = "—";
    const VERTICAL_BAR = "│";
    const CORNER = "└─";
    const ERROR_MARKER = "^";

    pub fn format(self: Diagnostic, raw_writer: anytype) !void {
        const writer = term.ColorWriter.init(raw_writer);

        // Header
        const header_text = " SYNTAX ERROR ";
        const dash_count = 60 - header_text.len;
        const prefix_dashes = HEADER_DASH ** 2;
        const suffix_dashes = HEADER_DASH ** (dash_count - 2);

        try writer.format("\n", .{});
        try writer.styled(term.Color.Dim, prefix_dashes);
        try writer.styled(term.Color.Blue, header_text);
        try writer.styled(term.Color.Dim, suffix_dashes);
        try writer.format("\n\n", .{});

        // Error message
        try writer.styled(term.Color.Bold, self.problem);
        try writer.format("\n\n", .{});

        // Code snippet with line number
        try writer.format(" {s}\n", .{VERTICAL_BAR});
        try writer.format("{d}{s} {s}\n", .{ self.loc.src.line, VERTICAL_BAR, self.snippet });
        try writer.format(" {s} ", .{VERTICAL_BAR});

        // Error markers
        // Add spaces up to the error start position
        var i: usize = 0;
        while (i < self.loc.src.col - 1) : (i += 1) {
            try writer.plain(" ");
        }

        const span_length = self.loc.buf.end - self.loc.buf.start;
        var j: usize = 0;
        while (j < span_length) : (j += 1) {
            try writer.styled(term.Color.Red, "^");
        }

        try writer.format("\n", .{});

        // File location
        try writer.format(" {s} {s}", .{ CORNER, self.loc.filename });
        try writer.format(" (line: {d}, column: {d})\n\n", .{ self.loc.src.line, self.loc.src.col });

        // Optional hint
        if (self.hint.len > 0) {
            try writer.styled(term.Color.Cyan, "Hint: ");
            try writer.plain(self.hint);
            try writer.plain("\n\n");
        }
    }
};
