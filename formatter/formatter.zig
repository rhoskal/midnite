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

    /// Formats a node and returns the formatted string.
    pub fn formatNode(self: *Formatter, node: *const ast.Node) !void {
        switch (node.*) {
            // Basic Literals
            .comment => |comment| {
                try self.write("# ");
                try self.write(comment.text);
            },
            .doc_comment => |comment| {
                try self.write("## ");
                try self.write(comment.text);
            },
            .int_literal => |lit| {
                try self.write(lit.token.lexeme);
            },
            .float_literal => |lit| {
                try self.write(lit.token.lexeme);
            },
            .char_literal => |lit| {
                try self.write(lit.token.lexeme);
            },
            .str_literal => |lit| {
                try self.write(lit.token.lexeme);
            },
            .multiline_str_literal => |lit| {
                try self.write(lit.token.lexeme);
            },

            // Identifiers
            .lower_identifier => |ident| {
                try self.write(ident.identifier);
            },
            .upper_identifier => |ident| {
                try self.write(ident.identifier);
            },

            // Basic Data Structures
            .list => |list| {
                try self.write("[");

                for (list.elements.items, 0..) |element, i| {
                    try self.formatNode(element);

                    if (i < list.elements.items.len - 1) {
                        try self.write(", ");
                    }
                }

                try self.write("]");
            },
            .tuple => |tuple| {
                try self.write("(");

                for (tuple.elements.items, 0..) |element, i| {
                    try self.formatNode(element);

                    if (i < tuple.elements.items.len - 1) {
                        try self.write(", ");
                    }
                }

                try self.write(")");
            },

            // Basic Expressions
            .unary_expr => |expr| {
                try self.write("-");
                try self.formatNode(expr.operand);
            },
            .arithmetic_expr => |expr| {
                try self.formatNode(expr.left);

                switch (expr.operator.kind) {
                    .operator => |op| {
                        if (op == .FloatAdd) {
                            try self.write(" +. ");
                        }

                        if (op == .FloatDiv) {
                            try self.write(" /. ");
                        }

                        if (op == .FloatMul) {
                            try self.write(" *. ");
                        }

                        if (op == .FloatSub) {
                            try self.write(" -. ");
                        }

                        if (op == .IntAdd) {
                            try self.write(" + ");
                        }

                        if (op == .IntDiv) {
                            try self.write(" / ");
                        }

                        if (op == .IntMul) {
                            try self.write(" * ");
                        }

                        if (op == .IntSub) {
                            try self.write(" - ");
                        }
                    },
                    else => {},
                }

                try self.formatNode(expr.right);
            },
            .logical_expr => |expr| {
                try self.formatNode(expr.left);

                switch (expr.operator.kind) {
                    .operator => |op| {
                        if (op == .LogicalAnd) {
                            try self.write(" && ");
                        }

                        if (op == .LogicalOr) {
                            try self.write(" || ");
                        }
                    },
                    else => {},
                }

                try self.formatNode(expr.right);
            },
            .comparison_expr => |expr| {
                try self.formatNode(expr.left);

                switch (expr.operator.kind) {
                    .operator => |op| {
                        if (op == .Equality) {
                            try self.write(" == ");
                        }

                        if (op == .GreaterThan) {
                            try self.write(" > ");
                        }

                        if (op == .GreaterThanEqual) {
                            try self.write(" >= ");
                        }

                        if (op == .LessThan) {
                            try self.write(" < ");
                        }

                        if (op == .LessThanEqual) {
                            try self.write(" <= ");
                        }

                        if (op == .NotEqual) {
                            try self.write(" /= ");
                        }
                    },
                    else => {},
                }

                try self.formatNode(expr.right);
            },

            // Pattern Matching
            .pattern => |pat| {
                switch (pat.*) {
                    .wildcard => {
                        try self.write("_");
                    },
                    .int_literal => |lit| {
                        try self.formatNode(&.{ .int_literal = lit });
                    },
                    .float_literal => |lit| {
                        try self.formatNode(&.{ .float_literal = lit });
                    },
                    .char_literal => |lit| {
                        try self.formatNode(&.{ .char_literal = lit });
                    },
                    .string_literal => |lit| {
                        try self.formatNode(&.{ .str_literal = lit });
                    },
                    .list => |list| {
                        try self.write("[");

                        for (list.patterns.items, 0..) |pattern, i| {
                            try self.formatNode(&.{ .pattern = pattern });

                            if (i < list.patterns.items.len - 1) {
                                try self.write(", ");
                            }
                        }

                        try self.write("]");
                    },
                    .variable => |var_pattern| {
                        try self.formatNode(&.{ .lower_identifier = var_pattern.name });
                    },
                    .constructor => |con| {
                        try self.write(con.name);

                        if (con.parameters.items.len > 0) {
                            try self.write("(");

                            for (con.parameters.items, 0..) |param, i| {
                                try self.formatNode(&.{ .pattern = param });

                                if (i < con.parameters.items.len - 1) {
                                    try self.write(", ");
                                }
                            }

                            try self.write(")");
                        }
                    },
                    .empty_list => {
                        try self.write("[]");
                    },
                    .cons => |cons| {
                        try self.formatNode(&.{ .pattern = cons.head });
                        try self.write(" :: ");
                        try self.formatNode(&.{ .pattern = cons.tail });
                    },
                }
            },
            .match_expr => |expr| {
                try self.write("match ");

                try self.formatNode(expr.subject);

                try self.write(" on\n");

                for (expr.cases.items) |case| {
                    try self.writeIndent();
                    try self.write("| ");
                    try self.formatNode(&.{ .pattern = case.pattern });

                    if (case.guard) |guard| {
                        try self.write(" when ");
                        try self.formatNode(guard.condition);
                    }

                    try self.write(" => ");

                    try self.formatNode(case.expression);

                    try self.write("\n");
                }
            },

            // Functions and Applications
            .function_signature => |sig| {
                try self.write("(");

                for (sig.parameter_types.items, 0..) |param_type, i| {
                    try self.formatNode(param_type);

                    if (i < sig.parameter_types.items.len - 1) {
                        try self.write(", ");
                    }
                }

                try self.write(") -> ");

                try self.formatNode(sig.return_type);
            },
            .lambda_expr => |expr| {
                try self.write("fn(");

                for (expr.parameters.items, 0..) |param, i| {
                    try self.formatNode(&.{ .lower_identifier = param.name });

                    if (param.type_annotation) |type_anno| {
                        try self.write(" : ");
                        try self.formatNode(type_anno);
                    }

                    if (i < expr.parameters.items.len - 1) {
                        try self.write(", ");
                    }
                }

                try self.write(") => ");

                try self.formatNode(expr.body);
            },
            .function_call => |call| {
                try self.formatNode(call.function);
                try self.write("(");

                for (call.arguments.items, 0..) |arg, i| {
                    try self.formatNode(arg);

                    if (i < call.arguments.items.len - 1) {
                        try self.write(", ");
                    }
                }

                try self.write(")");
            },

            // Advanced Expressions
            .cons_expr => |expr| {
                try self.formatNode(expr.head);
                try self.write(" :: ");
                try self.formatNode(expr.tail);
            },
            .str_concat_expr => |expr| {
                try self.formatNode(expr.left);
                try self.write(" <> ");
                try self.formatNode(expr.right);
            },
            .list_concat_expr => |expr| {
                try self.formatNode(expr.left);
                try self.write(" ++ ");
                try self.formatNode(expr.right);
            },
            .pipe_expr => |expr| {
                switch (expr.operator.kind) {
                    .operator => |op| {
                        if (op == .PipeRight) {
                            try self.formatNode(expr.value);
                            try self.write(" |> ");
                            try self.formatNode(expr.func);
                        }
                    },
                    else => {},
                }
            },

            // Control Flow
            .if_then_else_stmt => |stmt| {
                try self.write("if ");

                try self.formatNode(stmt.condition);

                try self.write(" then\n");
                self.indent_level += 1;

                try self.writeIndent();
                try self.formatNode(stmt.then_branch);
                try self.write("\n");

                self.indent_level -= 1;
                try self.writeIndent();

                try self.write("else\n");
                self.indent_level += 1;

                try self.writeIndent();
                try self.formatNode(stmt.else_branch);
                try self.write("\n");

                self.indent_level -= 1;
            },

            // Type System
            .typed_hole => {
                try self.write(" ? ");
            },
            .type_application => |app| {
                try self.formatNode(&ast.Node{ .upper_identifier = app.constructor });
                try self.write(" ");

                for (app.args.items, 0..) |arg, i| {
                    const needs_parens = (arg.* == .type_application);
                    if (needs_parens) {
                        try self.write("(");
                    }

                    try self.formatNode(arg);

                    if (needs_parens) {
                        try self.write(")");
                    }

                    if (i < app.args.items.len - 1) {
                        try self.write(" ");
                    }
                }
            },
            .type_alias => |atype| {
                try self.write("type alias ");

                try self.write(atype.name.identifier);

                try self.write(" ");

                for (atype.type_params.items) |param| {
                    try self.write(param);
                    try self.write(" ");
                }

                try self.write("= ");

                try self.formatNode(atype.value);

                try self.write("\n");
            },
            .variant_type => |vtype| {
                try self.write("type ");

                try self.write(vtype.name.identifier);

                try self.write(" ");

                for (vtype.type_params.items) |param| {
                    try self.write(param);
                    try self.write(" ");
                }

                try self.write("=");
                try self.write("\n");

                self.indent_level += 1;

                for (vtype.constructors.items, 0..) |constructor, i| {
                    try self.writeIndent();
                    try self.write("| ");
                    try self.write(constructor.name.identifier);

                    for (constructor.parameters.items) |param| {
                        try self.write(" ");

                        const needs_parens = (param.* == .type_application);
                        if (needs_parens) {
                            try self.write("(");
                        }

                        try self.formatNode(param);

                        if (needs_parens) {
                            try self.write(")");
                        }
                    }

                    if (i < vtype.constructors.items.len - 1) {
                        try self.write("\n");
                    }
                }

                self.indent_level -= 1;

                try self.write("\n");
            },
            .record_type => |rtype| {
                try self.write("type ");

                try self.write(rtype.name.identifier);

                try self.write(" ");

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
                    try self.write(field.name.identifier);
                    try self.write(" : ");
                    try self.formatNode(field.type);

                    if (i < rtype.fields.items.len - 1) {
                        try self.writeNewlineAndIndent();
                        try self.write(", ");
                    }
                }

                try self.writeNewlineAndIndent();
                try self.write("}");

                self.indent_level -= 1;

                try self.write("\n");
            },

            // Module System
            .module_path => |path| {
                for (path.segments.items, 0..) |segment, i| {
                    if (i > 0) {
                        try self.write(".");
                    }

                    try self.write(segment.identifier);
                }
            },
            .export_spec => |spec| {
                try self.write("exposing ");

                if (spec.exposing_all) {
                    try self.write("(..)");

                    return;
                }

                if (spec.items) |items| {
                    var sorted = try sortExportItems(items.allocator, items);
                    defer sorted.deinit();

                    if (sorted.items.len <= 2) {
                        try self.write("(");

                        for (sorted.items, 0..) |item, i| {
                            try self.write(item.name);

                            if (item.expose_constructors) {
                                try self.write("(..)");
                            }

                            if (i < sorted.items.len - 1) {
                                try self.write(", ");
                            }
                        }

                        try self.write(")");
                    } else {
                        try self.write("(\n");

                        self.indent_level += 1;

                        for (items.items, 0..) |item, i| {
                            try self.writeIndent();
                            try self.write(item.name);

                            if (item.expose_constructors) {
                                try self.write("(..)");
                            }

                            if (i < items.items.len - 1) {
                                try self.write(",\n");
                            }
                        }

                        self.indent_level -= 1;

                        try self.writeNewlineAndIndent();
                        try self.write(")");
                    }
                }
            },
            .import_spec => |spec| {
                try self.write("open ");

                for (spec.path.segments.items, 0..) |segment, i| {
                    if (i > 0) {
                        try self.write(".");
                    }

                    try self.write(segment.identifier);
                }

                switch (spec.kind) {
                    .Simple => {},
                    .Alias => {
                        try self.write(" as ");
                        try self.write(spec.alias.?.identifier);
                    },
                    .Using => {
                        try self.write(" using (");

                        if (spec.items) |items| {
                            var sorted = try sortImportItems(items.allocator, items);
                            defer sorted.deinit();

                            for (sorted.items, 0..) |item, i| {
                                switch (item.*) {
                                    .function => |f| {
                                        try self.write(f.name);

                                        if (f.alias) |alias| {
                                            try self.write(" as ");
                                            try self.write(alias);
                                        }
                                    },
                                    .operator => |op| {
                                        try self.write("(");
                                        try self.write(op.symbol);
                                        try self.write(")");

                                        if (op.alias) |alias| {
                                            try self.write(" as ");
                                            try self.write(alias);
                                        }
                                    },
                                    .type => |t| {
                                        try self.write(t.name);

                                        if (t.expose_constructors) {
                                            try self.write("(..)");
                                        }

                                        if (t.alias) |alias| {
                                            try self.write(" as ");
                                            try self.write(alias);
                                        }
                                    },
                                }

                                if (i < items.items.len - 1) {
                                    try self.write(", ");
                                }
                            }
                        }

                        try self.write(")");
                    },
                    .Hiding => {
                        try self.write(" hiding (");

                        if (spec.items) |items| {
                            var sorted = try sortImportItems(items.allocator, items);
                            defer sorted.deinit();

                            for (sorted.items, 0..) |item, i| {
                                switch (item.*) {
                                    .function => |f| {
                                        try self.write(f.name);
                                    },
                                    .operator => |op| {
                                        try self.write("(");
                                        try self.write(op.symbol);
                                        try self.write(")");
                                    },
                                    .type => |t| {
                                        try self.write(t.name);

                                        if (t.expose_constructors) {
                                            try self.write("(..)");
                                        }
                                    },
                                }

                                if (i < items.items.len - 1) {
                                    try self.write(", ");
                                }
                            }
                        }

                        try self.write(")");
                    },
                }

                try self.write("\n");
            },
            .include => |inc| {
                try self.write("include ");

                for (inc.path.segments.items, 0..) |segment, i| {
                    try self.write(segment.identifier);

                    if (i < inc.path.segments.items.len - 1) {
                        try self.write(".");
                    }
                }

                try self.write("\n");
            },

            // Top-Level Declarations
            .function_decl => |decl| {
                try self.write("let ");

                try self.formatNode(&.{ .lower_identifier = decl.name });
                try self.write("(");

                for (decl.parameters.items, 0..) |param, i| {
                    try self.formatNode(&.{ .lower_identifier = param.name });

                    if (param.type_annotation) |type_anno| {
                        try self.write(" : ");
                        try self.formatNode(type_anno);
                    }

                    if (i < decl.parameters.items.len - 1) {
                        try self.write(", ");
                    }
                }

                try self.write(")");

                if (decl.return_type) |ret_type| {
                    try self.write(" -> ");

                    try self.formatNode(ret_type);
                }

                try self.write(" =");

                self.indent_level += 1;
                try self.writeNewlineAndIndent();

                try self.formatNode(decl.value);

                self.indent_level -= 1;

                try self.write("\n");
            },
            .foreign_function_decl => |decl| {
                try self.write("foreign ");

                try self.formatNode(&.{ .lower_identifier = decl.name });
                try self.write("(");

                for (decl.parameters.items, 0..) |param, i| {
                    try self.formatNode(&.{ .lower_identifier = param.name });

                    if (param.type_annotation) |type_anno| {
                        try self.write(" : ");

                        try self.formatNode(type_anno);
                    }

                    if (i < decl.parameters.items.len - 1) {
                        try self.write(", ");
                    }
                }

                try self.write(")");

                try self.write(" -> ");

                try self.formatNode(decl.return_type);

                try self.write(" =");

                self.indent_level += 1;
                try self.writeNewlineAndIndent();

                try self.formatNode(&.{ .str_literal = decl.external_name });

                self.indent_level -= 1;

                try self.write("\n");
            },
            .module_decl => |decl| {
                try self.write("module ");

                for (decl.path.segments.items, 0..) |segment, i| {
                    if (i > 0) {
                        try self.write(".");
                    }

                    try self.formatNode(&.{ .upper_identifier = segment });
                }

                try self.write(" ");

                try self.formatNode(&.{ .export_spec = decl.exports });
                try self.write("\n\n");

                self.indent_level += 1;

                for (decl.declarations.items) |declaration| {
                    try self.formatNode(declaration);
                    try self.write("\n");

                    // Add extra newline between declarations for readability
                    if (declaration.* != .comment and declaration.* != .doc_comment and declaration.* != .foreign_function_decl) {
                        try self.write("\n");
                    }
                }

                self.indent_level -= 1;

                try self.write("end");
            },
            .program => |prog| {
                for (prog.statements.items) |stmt| {
                    try self.formatNode(stmt);
                    try self.write("\n");
                }
            },
        }
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

    /// Takes a list of export items and returns a new sorted list alphabetically by name.
    ///
    /// Memory: Caller owns the returned ArrayList and must call deinit on it.
    ///
    /// Example input: `[filter, Maybe(..), map]`
    /// Example output: `[Maybe(..), filter, map]`
    fn sortExportItems(
        allocator: std.mem.Allocator,
        items: std.ArrayList(ast.ExportItem),
    ) !std.ArrayList(ast.ExportItem) {
        var result = std.ArrayList(ast.ExportItem).init(allocator);
        errdefer result.deinit();

        try result.appendSlice(items.items);

        const customSort = struct {
            fn lessThan(_: void, a: ast.ExportItem, b: ast.ExportItem) bool {
                return std.mem.lessThan(u8, a.name, b.name);
            }
        }.lessThan;

        std.mem.sort(ast.ExportItem, result.items, {}, customSort);

        return result;
    }

    /// Takes a list of import items and returns a new sorted list where:
    /// - Types come first, sorted alphabetically
    /// - Functions come second, sorted alphabetically
    /// - Operators come last, sorted alphabetically
    /// All original items are preserved, only their order changes.
    ///
    /// Memory: Caller owns the returned ArrayList and must call deinit on it.
    ///
    /// Example input: `[(++), map, Maybe(..), filter, (>>=), Tree(..)]`
    /// Example output: `[Maybe(..), Tree(..), filter, map, (++), (>>=)]`
    fn sortImportItems(
        allocator: std.mem.Allocator,
        items: std.ArrayList(*ast.ImportItem),
    ) !std.ArrayList(*ast.ImportItem) {
        var types = std.ArrayList(*ast.ImportItem).init(allocator);
        defer types.deinit();

        var functions = std.ArrayList(*ast.ImportItem).init(allocator);
        defer functions.deinit();

        var operators = std.ArrayList(*ast.ImportItem).init(allocator);
        defer operators.deinit();

        for (items.items) |item| {
            switch (item.*) {
                .type => try types.append(item),
                .function => try functions.append(item),
                .operator => try operators.append(item),
            }
        }

        const typeSort = struct {
            fn lessThan(_: void, a: *ast.ImportItem, b: *ast.ImportItem) bool {
                return std.mem.lessThan(u8, a.type.name, b.type.name);
            }
        }.lessThan;

        const functionSort = struct {
            fn lessThan(_: void, a: *ast.ImportItem, b: *ast.ImportItem) bool {
                return std.mem.lessThan(u8, a.function.name, b.function.name);
            }
        }.lessThan;

        const operatorSort = struct {
            fn lessThan(_: void, a: *ast.ImportItem, b: *ast.ImportItem) bool {
                return std.mem.lessThan(u8, a.operator.symbol, b.operator.symbol);
            }
        }.lessThan;

        std.mem.sort(*ast.ImportItem, types.items, {}, typeSort);
        std.mem.sort(*ast.ImportItem, functions.items, {}, functionSort);
        std.mem.sort(*ast.ImportItem, operators.items, {}, operatorSort);

        var result = try std.ArrayList(*ast.ImportItem).initCapacity(
            allocator,
            types.items.len + functions.items.len + operators.items.len,
        );
        errdefer result.deinit();

        try result.appendSlice(types.items);
        try result.appendSlice(functions.items);
        try result.appendSlice(operators.items);

        return result;
    }
};
