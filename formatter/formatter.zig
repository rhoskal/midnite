const std = @import("std");

const ast = @import("compiler").frontend.ast;
// const FormatError = @import("root.zig").FormatError;

/// The main formatter state, tracking indentation and handling output.
pub const Formatter = struct {
    /// The underlying writer for output.
    writer: std.ArrayList(u8).Writer,

    /// Current indentation level (number of levels deep).
    indent_level: usize,

    /// Number of spaces per indentation level.
    indent_width: usize,

    pub fn init(allocator: std.mem.Allocator) Formatter {
        var list = std.ArrayList(u8).init(allocator);

        return .{
            .writer = list.writer(),
            .indent_level = 0,
            .indent_width = 4,
        };
    }

    pub fn deinit(self: *Formatter) void {
        self.writer.context.deinit();
    }

    /// Writes the current indentation level as spaces.
    fn writeIndent(self: *Formatter) !void {
        const spaces = self.indent_level * self.indent_width;
        var i: usize = 0;
        while (i < spaces) : (i += 1) {
            try self.writer.writeByte(' ');
        }
    }

    /// Helper to write a string.
    fn write(self: *Formatter, str: []const u8) !void {
        try self.writer.writeAll(str);
    }

    /// Helper to write a newline followed by indentation.
    fn writeNewlineAndIndent(self: *Formatter) !void {
        try self.write("\n");
        try self.writeIndent();
    }

    /// Formats a node and returns the formatted string.
    pub fn formatNode(self: *Formatter, node: *const ast.Node) !void {
        switch (node.*) {
            .program => |prog| {
                for (prog.statements.items) |stmt| {
                    try self.formatNode(stmt);
                    try self.write("\n");
                }
            },
            .type_alias => |atype| {
                try self.write("type alias ");
                try self.write(atype.name);
                try self.write(" ");

                for (atype.type_params.items) |param| {
                    try self.write(param);
                    try self.write(" ");
                }

                try self.write("= ");
                try self.formatNode(atype.value);
            },
            .record_type => |rtype| {
                try self.write("type ");
                try self.write(rtype.name);
                try self.write(" ");

                // Format type parameters if any
                for (rtype.type_params.items) |param| {
                    try self.write(param);
                    try self.write(" ");
                }

                try self.write("=");
                try self.writeNewlineAndIndent();

                self.indent_level += 1;

                try self.writeIndent();
                try self.write("{ ");

                for (rtype.fields.items, 0..) |field, i| {
                    try self.write(field.name);
                    try self.write(" : ");
                    try self.formatNode(field.type);

                    if (i < rtype.fields.items.len - 1) {
                        try self.write("\n");
                        try self.writeIndent();
                        try self.write(", ");
                    }
                }

                try self.write("\n");
                try self.writeIndent();
                try self.write("}");

                self.indent_level -= 1;
            },
            .variant_type => |vtype| {
                try self.write("type ");
                try self.write(vtype.name);
                try self.write(" =");
                try self.write("\n");

                self.indent_level += 1;

                for (vtype.constructors.items, 0..) |constructor, i| {
                    try self.writeIndent();
                    try self.write("| ");
                    try self.write(constructor.name);

                    for (constructor.params.items) |param| {
                        try self.write(" ");
                        try self.formatNode(param);
                    }

                    if (i < vtype.constructors.items.len - 1) {
                        try self.write("\n");
                    }
                }

                self.indent_level -= 1;
            },
            .function_type => |ftype| {
                for (ftype.param_types.items, 0..) |param, i| {
                    try self.formatNode(param);

                    if (i < ftype.param_types.items.len - 1) {
                        try self.write(" -> ");
                    }
                }
            },
            .type_application => |app| {
                try self.formatNode(app.base);
                try self.write(" ");

                for (app.args.items, 0..) |arg, i| {
                    try self.formatNode(arg);

                    if (i < app.args.items.len - 1) {
                        try self.write(" ");
                    }
                }
            },
            .include => |inc| {
                try self.write("include ");

                for (inc.path.segments.items, 0..) |segment, i| {
                    try self.write(segment);

                    if (i < inc.path.segments.items.len - 1) {
                        try self.write(".");
                    }
                }
            },
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
            .doc_comment => |comment| {
                try self.write("## ");
                try self.write(comment.content);
            },
            .comment => |comment| {
                try self.write("# ");
                try self.write(comment.content);
            },
            else => {
                // For now, skip other node types
                std.debug.print("Node type: {any}\n", .{node.*});
            },
        }
    }
};
