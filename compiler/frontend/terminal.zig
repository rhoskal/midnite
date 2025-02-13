const std = @import("std");

/// ANSI terminal colors and text styles.
pub const Color = enum {
    Reset,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    Bold,
    Dim,

    pub fn toString(self: Color) []const u8 {
        return switch (self) {
            .Reset => "\x1b[0m",
            .Red => "\x1b[31m",
            .Green => "\x1b[32m",
            .Yellow => "\x1b[33m",
            .Blue => "\x1b[34m",
            .Magenta => "\x1b[35m",
            .Cyan => "\x1b[36m",
            .Bold => "\x1b[1m",
            .Dim => "\x1b[2m",
        };
    }
};

/// A wrapper around a file writer that provides convenient methods
/// for writing colored/styled text to the terminal.
pub const ColorWriter = struct {
    inner: std.fs.File.Writer,

    pub fn init(writer: std.fs.File.Writer) ColorWriter {
        return .{
            .inner = writer,
        };
    }

    /// Writes text with the specified color/style, automatically resetting afterwards.
    pub fn styled(self: ColorWriter, color: Color, text: []const u8) !void {
        try self.inner.writeAll(color.toString());
        try self.inner.writeAll(text);
        try self.inner.writeAll(Color.Reset.toString());
    }

    /// Writes text without any color/style formatting.
    pub fn plain(self: ColorWriter, text: []const u8) !void {
        try self.inner.writeAll(text);
    }

    /// Formats and writes text using the given format string and arguments.
    pub fn format(self: ColorWriter, comptime fmt: []const u8, args: anytype) !void {
        try std.fmt.format(self.inner, fmt, args);
    }
};
