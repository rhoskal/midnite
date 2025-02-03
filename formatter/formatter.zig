const std = @import("std");

const ast = @import("compiler").frontend.ast;
const FormatError = @import("root.zig").FormatError;

/// The main formatter state, tracking indentation and handling output.
pub const Formatter = struct {
    /// The underlying writer for output.
    writer: std.ArrayList(u8).Writer,

    /// Current indentation level (number of levels deep).
    indent_level: usize,

    /// Number of spaces per indentation level.
    indent_width: usize,

    pub fn init(allocator: std.mem.Allocator) Formatter {
        return .{
            .writer = std.ArrayList(u8).init(allocator).writer(),
            .indent_level = 0,
            .indent_width = 4,
        };
    }

    pub fn deinit(self: *Formatter) void {
        self.writer.context.deinit();
    }

    /// Writes the current indentation level as spaces.
    fn writeIndent(self: *Formatter) FormatError!void {
        const spaces = self.indent_level * self.indent_width;
        var i: usize = 0;
        while (i < spaces) : (i += 1) {
            try self.writer.writeByte(' ');
        }
    }

    /// Helper to write a string.
    fn write(self: *Formatter, str: []const u8) FormatError!void {
        try self.writer.writeAll(str);
    }

    /// Helper to write a newline followed by indentation.
    fn writeNewlineAndIndent(self: *Formatter) FormatError!void {
        try self.write("\n");
        try self.writeIndent();
    }

    /// Formats a node and returns the formatted string.
    pub fn formatNode(self: *Formatter, node: *const ast.Node) FormatError!void {
        switch (node.*) {
            .int_literal => |lit| {
                // For integer literals, just write the lexeme directly
                try self.write(lit.token.lexeme);
            },
            .lower_identifier => |ident| {
                // For identifiers, write the name
                try self.write(ident.name);
            },
            .upper_identifier => |ident| {
                try self.write(ident.name);
            },
            .str_literal => |lit| {
                // For string literals, write with quotes
                try self.write(lit.token.lexeme);
            },
            .float_literal => |lit| {
                try self.write(lit.token.lexeme);
            },
            else => {
                // For now, skip other node types
                try self.write("<unimplemented>");
            },
        }
    }
};
