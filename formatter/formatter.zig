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
                try self.write("\n");
            },
            .record_type => |rtype| {
                try self.write("type ");
                try self.write(rtype.name);
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
                    try self.write(field.name);
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
            .variant_type => |vtype| {
                try self.write("type ");
                try self.write(vtype.name);
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
                    try self.write(constructor.name);

                    for (constructor.params.items) |param| {
                        try self.write(" ");

                        const needs_parens = (param.* == .type_application);
                        if (needs_parens) try self.write("(");
                        try self.formatNode(param);
                        if (needs_parens) try self.write(")");
                    }

                    if (i < vtype.constructors.items.len - 1) {
                        try self.write("\n");
                    }
                }

                self.indent_level -= 1;

                try self.write("\n");
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
                    const needs_parens = (arg.* == .type_application);
                    if (needs_parens) try self.write("(");

                    try self.formatNode(arg);

                    if (needs_parens) try self.write(")");

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

                try self.write("\n");
            },
            .import_spec => |spec| {
                try self.write("open ");

                for (spec.path.segments.items, 0..) |segment, i| {
                    if (i > 0) try self.write(".");

                    try self.write(segment);
                }

                switch (spec.kind) {
                    .Simple => {},
                    .Alias => {
                        try self.write(" as ");
                        try self.write(spec.alias.?);
                    },
                    .Using => {
                        try self.write(" using (");

                        if (spec.items) |items| {
                            var sorted = try sortImportItems(items.allocator, items);
                            defer sorted.deinit();

                            for (sorted.items, 0..) |item, i| {
                                switch (item) {
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
                                switch (item) {
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
            .foreign_function_decl => |decl| {
                try self.write("foreign ");
                try self.write(decl.name);

                try self.write(" : ");
                try self.formatNode(decl.type_annotation);

                try self.write(" = ");
                try self.write("\"");
                try self.write(decl.external_name);
                try self.write("\"");

                try self.write("\n");
            },
            .function_decl => |decl| {
                try self.write("let ");
                try self.write(decl.name);

                if (decl.type_annotation) |anno| {
                    try self.write(" : ");
                    try self.formatNode(anno);
                }

                try self.write(" = ");
                try self.formatNode(decl.value);

                try self.write("\n");
            },
            .lambda_expr => |expr| {
                try self.write("\\");

                for (expr.params.items, 0..) |param, i| {
                    try self.write(param);

                    if (i < expr.params.items.len - 1) {
                        try self.write(" ");
                    }
                }

                try self.write(" => ");

                try self.formatNode(expr.body);
            },
            .function_application => |app| {
                try self.formatNode(app.function);

                try self.write(" ");

                const needs_parens = (app.argument.* == .function_application);
                if (needs_parens) try self.write("(");

                try self.formatNode(app.argument);

                if (needs_parens) try self.write(")");
            },
            .arithmetic_expr => |expr| {
                try self.formatNode(expr.left);

                switch (expr.operator.kind) {
                    .operator => |op| {
                        if (op == .FloatAdd) try self.write(" +. ");
                        if (op == .FloatDiv) try self.write(" /. ");
                        if (op == .FloatMul) try self.write(" *. ");
                        if (op == .FloatSub) try self.write(" -. ");
                        if (op == .IntAdd) try self.write(" + ");
                        if (op == .IntDiv) try self.write(" / ");
                        if (op == .IntMul) try self.write(" * ");
                        if (op == .IntSub) try self.write(" - ");
                    },
                    else => {},
                }

                try self.formatNode(expr.right);
            },
            .comparison_expr => |expr| {
                try self.formatNode(expr.left);

                switch (expr.operator.kind) {
                    .operator => |op| {
                        if (op == .Equality) try self.write(" == ");
                        if (op == .GreaterThan) try self.write(" > ");
                        if (op == .GreaterThanEqual) try self.write(" >= ");
                        if (op == .LessThan) try self.write(" < ");
                        if (op == .LessThanEqual) try self.write(" <= ");
                        if (op == .NotEqual) try self.write(" /= ");
                    },
                    else => {},
                }

                try self.formatNode(expr.right);
            },
            .composition_expr => |expr| {
                try self.formatNode(expr.first);

                switch (expr.operator.kind) {
                    .operator => |op| {
                        if (op == .ComposeLeft) try self.write(" << ");
                        if (op == .ComposeRight) try self.write(" >> ");
                    },
                    else => {},
                }

                try self.formatNode(expr.second);
            },
            .cons_expr => |expr| {
                try self.formatNode(expr.head);
                try self.write(" :: ");
                try self.formatNode(expr.tail);
            },
            .list_concat_expr => |expr| {
                try self.formatNode(expr.left);
                try self.write(" ++ ");
                try self.formatNode(expr.right);
            },
            .logical_expr => |expr| {
                try self.formatNode(expr.left);

                switch (expr.operator.kind) {
                    .operator => |op| {
                        if (op == .LogicalAnd) try self.write(" && ");
                        if (op == .LogicalOr) try self.write(" || ");
                    },
                    else => {},
                }

                try self.formatNode(expr.right);
            },
            .pipe_expr => |expr| {
                switch (expr.operator.kind) {
                    .operator => |op| {
                        if (op == .PipeLeft) {
                            try self.formatNode(expr.func);
                            try self.write(" <| ");
                            try self.formatNode(expr.value);
                        }

                        if (op == .PipeRight) {
                            try self.formatNode(expr.value);
                            try self.write(" |> ");
                            try self.formatNode(expr.func);
                        }
                    },
                    else => {},
                }
            },
            .str_concat_expr => |expr| {
                try self.formatNode(expr.left);
                try self.write(" <> ");
                try self.formatNode(expr.right);
            },
            .unary_expr => |expr| {
                try self.write("-");
                try self.formatNode(expr.operand);
            },
            .if_then_else_stmt => |stmt| {
                try self.write("if ");
                try self.formatNode(stmt.condition);

                try self.write(" then\n");

                self.indent_level += 1;

                try self.writeIndent();
                try self.formatNode(stmt.then_branch);
                try self.write("\n");

                self.indent_level -= 1;

                try self.write("else\n");

                self.indent_level += 1;

                try self.writeIndent();
                try self.formatNode(stmt.else_branch);
                try self.write("\n");

                self.indent_level -= 1;
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

    /// Takes a list of import items and returns a new sorted list where:
    /// - Types come first, sorted alphabetically
    /// - Functions come second, sorted alphabetically
    /// - Operators come last, sorted alphabetically
    /// All original items are preserved, only their order changes.
    ///
    /// Memory: Caller owns the returned ArrayList and must call deinit on it.
    ///
    /// Example input:  `[(++), map, Maybe(..), filter, (>>=), Tree(..)]`
    /// Example output: `[Maybe(..), Tree(..), filter, map, (++), (>>=)]`
    fn sortImportItems(
        allocator: std.mem.Allocator,
        items: std.ArrayList(ast.ImportItem),
    ) !std.ArrayList(ast.ImportItem) {
        var types = std.ArrayList(ast.ImportItem).init(allocator);
        defer types.deinit();

        var functions = std.ArrayList(ast.ImportItem).init(allocator);
        defer functions.deinit();

        var operators = std.ArrayList(ast.ImportItem).init(allocator);
        defer operators.deinit();

        var result = std.ArrayList(ast.ImportItem).init(allocator);
        errdefer result.deinit();

        for (items.items) |item| {
            switch (item) {
                .type => try types.append(item),
                .function => try functions.append(item),
                .operator => try operators.append(item),
            }
        }

        const typeSort = struct {
            fn lessThan(_: void, a: ast.ImportItem, b: ast.ImportItem) bool {
                const a_name = if (a == .type) a.type.name else unreachable;
                const b_name = if (b == .type) b.type.name else unreachable;

                return std.mem.lessThan(u8, a_name, b_name);
            }
        }.lessThan;

        const functionSort = struct {
            fn lessThan(_: void, a: ast.ImportItem, b: ast.ImportItem) bool {
                const a_name = if (a == .function) a.function.name else unreachable;
                const b_name = if (b == .function) b.function.name else unreachable;

                return std.mem.lessThan(u8, a_name, b_name);
            }
        }.lessThan;

        const operatorSort = struct {
            fn lessThan(_: void, a: ast.ImportItem, b: ast.ImportItem) bool {
                const a_sym = if (a == .operator) a.operator.symbol else unreachable;
                const b_sym = if (b == .operator) b.operator.symbol else unreachable;

                return std.mem.lessThan(u8, a_sym, b_sym);
            }
        }.lessThan;

        std.mem.sort(ast.ImportItem, types.items, {}, typeSort);
        std.mem.sort(ast.ImportItem, functions.items, {}, functionSort);
        std.mem.sort(ast.ImportItem, operators.items, {}, operatorSort);

        try result.appendSlice(types.items);
        try result.appendSlice(functions.items);
        try result.appendSlice(operators.items);

        return result;
    }
};
