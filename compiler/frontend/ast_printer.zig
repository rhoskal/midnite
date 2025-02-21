const std = @import("std");

const ast = @import("ast.zig");
const term = @import("terminal.zig");

/// A debug utility for printing AST nodes in a readable tree format.
/// Useful for debugging the parser and examining expression structure.
pub const AstPrinter = struct {
    /// The underlying writer for output.
    writer: term.ColorWriter,

    /// Current indentation level (number of levels deep).
    indent_level: usize,

    /// Memory allocator used for printing operations.
    allocator: std.mem.Allocator,

    /// Initialize a new AST printer that writes to the given writer.
    pub fn init(allocator: std.mem.Allocator, writer: std.fs.File.Writer) AstPrinter {
        return .{
            .writer = term.ColorWriter.init(writer),
            .indent_level = 0,
            .allocator = allocator,
        };
    }

    /// Print a complete AST node and its children.
    pub fn printNode(self: *AstPrinter, node: *const ast.Node) !void {
        switch (node.*) {
            // Basic Literals
            .comment => |comment| {
                try self.writer.styled(term.Color.Bold, "Comment");
                try self.writer.plain("(");
                try self.writer.styled(term.Color.Green, comment.text);
                try self.writer.plain(")\n");
            },
            .doc_comment => |comment| {
                try self.writer.styled(term.Color.Bold, "DocComment");
                try self.writer.plain("(");
                try self.writer.styled(term.Color.Green, comment.text);
                try self.writer.plain(")\n");
            },
            .int_literal => |lit| {
                try self.writer.styled(term.Color.Bold, "IntegerLiteral");
                try self.writer.plain("(");

                const formatted = try std.fmt.allocPrint(
                    self.allocator,
                    "{d}",
                    .{lit.value},
                );
                defer self.allocator.free(formatted);

                try self.writer.styled(term.Color.Green, formatted);
                try self.writer.plain(")\n");
            },
            .float_literal => |lit| {
                try self.writer.styled(term.Color.Bold, "FloatLiteral");
                try self.writer.plain("(");
                try self.writer.styled(
                    term.Color.Green,
                    try std.fmt.allocPrint(
                        self.allocator,
                        "{d}",
                        .{lit.value},
                    ),
                );
                try self.writer.plain(")\n");
            },
            .char_literal => |lit| {
                try self.writer.styled(term.Color.Bold, "CharacterLiteral");
                try self.writer.plain("('");
                try self.writer.styled(
                    term.Color.Green,
                    try std.fmt.allocPrint(
                        self.allocator,
                        "{u}",
                        .{lit.value},
                    ),
                );
                try self.writer.plain("')\n");
            },
            .str_literal => |lit| {
                try self.writer.styled(term.Color.Bold, "StringLiteral");
                try self.writer.plain("(\"");
                try self.writer.styled(term.Color.Green, lit.value);
                try self.writer.plain("\")\n");
            },
            .multiline_str_literal => |lit| {
                try self.writer.styled(term.Color.Bold, "MultilineStringLiteral");
                try self.writer.plain("(\"\"\"\n");
                try self.writer.styled(term.Color.Green, lit.value);
                try self.writer.plain("\n\"\"\")\n");
            },

            // Identifiers
            .lower_identifier => |ident| {
                try self.writer.styled(term.Color.Bold, "LowerIdent");
                try self.writer.plain("(");
                try self.writer.styled(term.Color.Magenta, ident.identifier);
                try self.writer.plain(")\n");
            },
            .upper_identifier => |ident| {
                try self.writer.styled(term.Color.Bold, "UpperIdent");
                try self.writer.plain("(");
                try self.writer.styled(term.Color.Magenta, ident.identifier);
                try self.writer.plain(")\n");
            },

            // Basic Data Structures
            .list => |list| {
                try self.writer.styled(term.Color.Bold, "List");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "elements: [\n");

                self.indent_level += 1;

                for (list.elements.items) |element| {
                    try self.printIndent();
                    try self.printNode(element);
                }

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.plain("]\n");

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .tuple => |tuple| {
                try self.writer.styled(term.Color.Bold, "Tuple");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "elements: [\n");

                self.indent_level += 1;

                for (tuple.elements.items) |element| {
                    try self.printIndent();
                    try self.printNode(element);
                }

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.plain("]\n");

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },

            // Basic Expressions
            .unary_expr => |expr| {
                try self.printIndent();
                try self.writer.styled(term.Color.Bold, "UnaryExpr");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "operator: ");
                try self.writer.styled(term.Color.Yellow, @tagName(expr.operator.kind));
                try self.writer.plain(" (");
                try self.writer.styled(term.Color.Yellow, expr.operator.lexeme);
                try self.writer.plain(")\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "operand: ");
                try self.printNode(expr.operand);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .arithmetic_expr => |expr| {
                try self.writer.styled(term.Color.Bold, "ArithmeticExpr");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "operator: ");
                try self.writer.styled(term.Color.Yellow, @tagName(expr.operator.kind));
                try self.writer.plain(" (");
                try self.writer.styled(term.Color.Yellow, expr.operator.lexeme);
                try self.writer.plain(")\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "left: ");
                try self.printNode(expr.left);

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "right: ");
                try self.printNode(expr.right);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .logical_expr => |expr| {
                try self.printIndent();
                try self.writer.styled(term.Color.Bold, "LogicalExpr");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "operator: ");
                try self.writer.styled(term.Color.Yellow, @tagName(expr.operator.kind));
                try self.writer.plain(" (");
                try self.writer.styled(term.Color.Yellow, expr.operator.lexeme);
                try self.writer.plain(")\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "left: ");
                try self.printNode(expr.left);

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "right: ");
                try self.printNode(expr.right);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .comparison_expr => |expr| {
                try self.writer.styled(term.Color.Bold, "ComparisonExpr");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "operator: ");
                try self.writer.styled(term.Color.Yellow, @tagName(expr.operator.kind));
                try self.writer.plain(" (");
                try self.writer.styled(term.Color.Yellow, expr.operator.lexeme);
                try self.writer.plain(")\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "left: ");
                try self.printNode(expr.left);

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "right: ");
                try self.printNode(expr.right);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },

            // Pattern Matching
            .pattern => |pat| {
                try self.printPattern(pat);
            },
            .match_expr => |expr| {
                try self.writer.styled(term.Color.Bold, "MatchExpr");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "subject: ");
                try self.printNode(expr.subject);

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "cases: [\n");

                self.indent_level += 1;

                for (expr.cases.items) |case| {
                    try self.printIndent();
                    try self.writer.styled(term.Color.Bold, "Case");
                    try self.writer.styled(term.Color.Dim, " {\n");

                    self.indent_level += 1;

                    try self.printIndent();
                    try self.writer.styled(term.Color.Cyan, "pattern: ");
                    try self.printPattern(case.pattern);

                    if (case.guard) |guard| {
                        try self.printIndent();
                        try self.writer.styled(term.Color.Cyan, "guard: ");
                        try self.printNode(guard.condition);
                    }

                    try self.printIndent();
                    try self.writer.styled(term.Color.Cyan, "expression: ");
                    try self.printNode(case.expression);

                    self.indent_level -= 1;

                    try self.printIndent();
                    try self.writer.styled(term.Color.Dim, "}\n");
                }

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.plain("]\n");

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },

            // Functions and Applications
            .function_type => |ftype| {
                try self.writer.styled(term.Color.Bold, "FunctionType");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "signature_types: [\n");

                self.indent_level += 1;

                for (ftype.signature_types.items) |stype| {
                    try self.printIndent();
                    try self.printNode(stype);
                }

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "]\n");

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .lambda_expr => |expr| {
                try self.writer.styled(term.Color.Bold, "LambdaExpr");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "params: [");

                for (expr.param_names.items, 0..) |param, i| {
                    if (i > 0) {
                        try self.writer.plain(", ");
                    }

                    try self.writer.styled(term.Color.Magenta, param);
                }

                try self.writer.plain("]\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "body: ");
                try self.printNode(expr.body);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .function_application => |expr| {
                try self.writer.styled(term.Color.Bold, "FunctionApplication");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "function: ");
                try self.printNode(expr.function);

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "argument: ");
                try self.printNode(expr.argument);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },

            // Advanced Expressions
            .cons_expr => |expr| {
                try self.writer.styled(term.Color.Bold, "ConsExpr");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "head: ");
                try self.printNode(expr.head);

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "tail: ");
                try self.printNode(expr.tail);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .str_concat_expr => |expr| {
                try self.writer.styled(term.Color.Bold, "StrConcatExpr");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "left: ");
                try self.printNode(expr.left);

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "right: ");
                try self.printNode(expr.right);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .list_concat_expr => |expr| {
                try self.writer.styled(term.Color.Bold, "ListConcatExpr");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "left: ");
                try self.printNode(expr.left);

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "right: ");
                try self.printNode(expr.right);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .composition_expr => |expr| {
                try self.writer.styled(term.Color.Bold, "CompositionExpr");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "operator: ");
                try self.writer.styled(term.Color.Yellow, @tagName(expr.operator.kind));
                try self.writer.plain(" (");
                try self.writer.styled(term.Color.Yellow, expr.operator.lexeme);
                try self.writer.plain(")\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "first: ");
                try self.printNode(expr.first);

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "second: ");
                try self.printNode(expr.second);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .pipe_expr => |expr| {
                try self.writer.styled(term.Color.Bold, "PipeExpr");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "operator: ");
                try self.writer.styled(term.Color.Yellow, @tagName(expr.operator.kind));
                try self.writer.plain(" (");
                try self.writer.styled(term.Color.Yellow, expr.operator.lexeme);
                try self.writer.plain(")\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "value: ");
                try self.printNode(expr.value);

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "func: ");
                try self.printNode(expr.func);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },

            // Control Flow
            .if_then_else_stmt => |stmt| {
                try self.writer.styled(term.Color.Bold, "IfThenElse");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "condition: ");
                try self.printNode(stmt.condition);

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "then: ");
                try self.printNode(stmt.then_branch);

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "else: ");
                try self.printNode(stmt.else_branch);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },

            // Type System
            .typed_hole => {
                try self.writer.styled(term.Color.Bold, "TypedHole");
                try self.writer.plain("(?)\n");
            },
            .type_application => |app| {
                try self.writer.styled(term.Color.Bold, "TypeApplication");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "constructor: ");
                try self.writer.styled(term.Color.Bold, "UpperIdent");
                try self.writer.plain("(");
                try self.writer.styled(term.Color.Magenta, app.constructor.identifier);
                try self.writer.plain(")\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "args: [\n");

                self.indent_level += 1;

                for (app.args.items) |arg| {
                    try self.printIndent();
                    try self.printNode(arg);
                }

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.plain("]\n");

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .type_alias => |alias| {
                try self.writer.styled(term.Color.Bold, "TypeAlias");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "name: ");
                try self.writer.styled(term.Color.Magenta, alias.name);
                try self.writer.plain("\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "type_params: [");

                for (alias.type_params.items, 0..) |param, i| {
                    if (i > 0) {
                        try self.writer.plain(", ");
                    }

                    try self.writer.styled(term.Color.Magenta, param);
                }

                try self.writer.plain("]\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "value: ");
                try self.printNode(alias.value);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .variant_type => |vtype| {
                try self.writer.styled(term.Color.Bold, "VariantType");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "name: ");
                try self.writer.styled(term.Color.Magenta, vtype.name);
                try self.writer.plain("\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "type_params: [");

                for (vtype.type_params.items, 0..) |param, i| {
                    if (i > 0) {
                        try self.writer.plain(", ");
                    }

                    try self.writer.styled(term.Color.Magenta, param);
                }

                try self.writer.plain("]\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "constructors: [\n");

                self.indent_level += 1;

                for (vtype.constructors.items) |constructor| {
                    try self.printIndent();
                    try self.writer.styled(term.Color.Bold, "Constructor");
                    try self.writer.styled(term.Color.Dim, " {\n");

                    self.indent_level += 1;

                    try self.printIndent();
                    try self.writer.styled(term.Color.Cyan, "name: ");
                    try self.writer.styled(term.Color.Magenta, constructor.name);
                    try self.writer.plain("\n");

                    try self.printIndent();
                    try self.writer.styled(term.Color.Cyan, "params: [\n");

                    self.indent_level += 1;

                    for (constructor.parameters.items) |param| {
                        try self.printIndent();
                        try self.printNode(param);
                    }

                    self.indent_level -= 1;

                    try self.printIndent();
                    try self.writer.plain("]\n");

                    self.indent_level -= 1;

                    try self.printIndent();
                    try self.writer.styled(term.Color.Dim, "}\n");
                }

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.plain("]\n");

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .record_type => |rtype| {
                try self.writer.styled(term.Color.Bold, "RecordType");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "name: ");
                try self.writer.styled(term.Color.Magenta, rtype.name);
                try self.writer.plain("\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "type_params: [");

                for (rtype.type_params.items, 0..) |param, i| {
                    if (i > 0) {
                        try self.writer.plain(", ");
                    }

                    try self.writer.styled(term.Color.Magenta, param);
                }

                try self.writer.plain("]\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "fields: [\n");

                self.indent_level += 1;

                for (rtype.fields.items) |field| {
                    try self.printIndent();
                    try self.writer.styled(term.Color.Bold, "Field");
                    try self.writer.styled(term.Color.Dim, " {\n");

                    self.indent_level += 1;

                    try self.printIndent();
                    try self.writer.styled(term.Color.Cyan, "name: ");
                    try self.writer.styled(term.Color.Magenta, field.name);
                    try self.writer.plain("\n");

                    try self.printIndent();
                    try self.writer.styled(term.Color.Cyan, "type: ");
                    try self.printNode(field.type);

                    self.indent_level -= 1;

                    try self.printIndent();
                    try self.writer.styled(term.Color.Dim, "}\n");
                }

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.plain("]\n");

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },

            // Module System
            .module_path => |path| {
                try self.writer.styled(term.Color.Bold, "ModulePath");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "segments: [");

                for (path.segments.items, 0..) |segment, i| {
                    if (i > 0) {
                        try self.writer.plain(".");
                    }

                    try self.writer.styled(term.Color.Magenta, segment);
                }

                try self.writer.plain("]\n");

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .export_spec => |spec| {
                try self.writer.styled(term.Color.Bold, "ExportSpec");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "exposing_all: ");
                try self.writer.styled(term.Color.Yellow, if (spec.exposing_all) "true" else "false");
                try self.writer.plain("\n");

                if (spec.items) |items| {
                    try self.printIndent();
                    try self.writer.styled(term.Color.Cyan, "items: [\n");

                    self.indent_level += 1;

                    for (items.items) |item| {
                        try self.printIndent();
                        try self.writer.styled(term.Color.Bold, "ExportItem");
                        try self.writer.styled(term.Color.Dim, " {\n");

                        self.indent_level += 1;

                        try self.printIndent();
                        try self.writer.styled(term.Color.Cyan, "name: ");
                        try self.writer.styled(term.Color.Magenta, item.name);
                        try self.writer.plain("\n");

                        try self.printIndent();
                        try self.writer.styled(term.Color.Cyan, "expose_constructors: ");
                        try self.writer.styled(term.Color.Yellow, if (item.expose_constructors) "true" else "false");
                        try self.writer.plain("\n");

                        self.indent_level -= 1;

                        try self.printIndent();
                        try self.writer.styled(term.Color.Dim, "}\n");
                    }

                    self.indent_level -= 1;

                    try self.printIndent();
                    try self.writer.plain("]\n");
                }

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .import_spec => |spec| {
                try self.writer.styled(term.Color.Bold, "ImportSpec");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "kind: ");
                try self.writer.styled(term.Color.Yellow, @tagName(spec.kind));
                try self.writer.plain("\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "path: ");

                for (spec.path.segments.items, 0..) |segment, i| {
                    if (i > 0) {
                        try self.writer.plain(".");
                    }

                    try self.writer.styled(term.Color.Magenta, segment);
                }

                try self.writer.plain("\n");

                if (spec.alias) |alias| {
                    try self.printIndent();
                    try self.writer.styled(term.Color.Cyan, "alias: ");
                    try self.writer.styled(term.Color.Magenta, alias);
                    try self.writer.plain("\n");
                }

                if (spec.items) |items| {
                    try self.printIndent();
                    try self.writer.styled(term.Color.Cyan, "items: [\n");

                    self.indent_level += 1;

                    for (items.items) |item| {
                        try self.printIndent();

                        switch (item.*) {
                            .function => |f| {
                                try self.writer.styled(term.Color.Bold, "Function");
                                try self.writer.styled(term.Color.Dim, " {\n");

                                self.indent_level += 1;

                                try self.printIndent();
                                try self.writer.styled(term.Color.Cyan, "name: ");
                                try self.writer.styled(term.Color.Magenta, f.name);
                                try self.writer.plain("\n");

                                if (f.alias) |alias| {
                                    try self.printIndent();
                                    try self.writer.styled(term.Color.Cyan, "alias: ");
                                    try self.writer.styled(term.Color.Magenta, alias);
                                    try self.writer.plain("\n");
                                }

                                self.indent_level -= 1;

                                try self.printIndent();
                                try self.writer.styled(term.Color.Dim, "}\n");
                            },
                            .operator => |op| {
                                try self.writer.styled(term.Color.Bold, "Operator");
                                try self.writer.styled(term.Color.Dim, " {\n");

                                self.indent_level += 1;

                                try self.printIndent();
                                try self.writer.styled(term.Color.Cyan, "symbol: ");
                                try self.writer.styled(term.Color.Yellow, op.symbol);
                                try self.writer.plain("\n");

                                if (op.alias) |alias| {
                                    try self.printIndent();
                                    try self.writer.styled(term.Color.Cyan, "alias: ");
                                    try self.writer.styled(term.Color.Magenta, alias);
                                    try self.writer.plain("\n");
                                }

                                self.indent_level -= 1;

                                try self.printIndent();
                                try self.writer.styled(term.Color.Dim, "}\n");
                            },
                            .type => |t| {
                                try self.writer.styled(term.Color.Bold, "Type");
                                try self.writer.styled(term.Color.Dim, " {\n");

                                self.indent_level += 1;

                                try self.printIndent();
                                try self.writer.styled(term.Color.Cyan, "name: ");
                                try self.writer.styled(term.Color.Magenta, t.name);
                                try self.writer.plain("\n");

                                try self.printIndent();
                                try self.writer.styled(term.Color.Cyan, "expose_constructors: ");
                                try self.writer.styled(term.Color.Yellow, if (t.expose_constructors) "true" else "false");
                                try self.writer.plain("\n");

                                if (t.alias) |alias| {
                                    try self.printIndent();
                                    try self.writer.styled(term.Color.Cyan, "alias: ");
                                    try self.writer.styled(term.Color.Magenta, alias);
                                    try self.writer.plain("\n");
                                }

                                self.indent_level -= 1;

                                try self.printIndent();
                                try self.writer.styled(term.Color.Dim, "}\n");
                            },
                        }
                    }

                    self.indent_level -= 1;

                    try self.printIndent();
                    try self.writer.plain("]\n");
                }

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .include => |inc| {
                try self.writer.styled(term.Color.Bold, "Include");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "path: [\n");

                self.indent_level += 1;

                for (inc.path.segments.items, 0..) |segment, i| {
                    try self.printIndent();

                    if (i > 0) {
                        try self.writer.plain(".");
                    }

                    try self.writer.styled(term.Color.Magenta, segment);
                    try self.writer.plain("\n");
                }

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.plain("]\n");

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },

            // Top-Level Declarations
            .function_decl => |decl| {
                try self.writer.styled(term.Color.Bold, "FunctionDecl");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "name: ");
                try self.writer.styled(term.Color.Magenta, decl.name);
                try self.writer.plain("\n");

                if (decl.type_annotation) |type_annot| {
                    try self.printIndent();
                    try self.writer.styled(term.Color.Cyan, "type: ");
                    const type_node = ast.Node{ .function_type = type_annot };
                    try self.printNode(&type_node);
                }

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "value: ");
                try self.printNode(decl.value);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .foreign_function_decl => |decl| {
                try self.writer.styled(term.Color.Bold, "ForeignFunctionDecl");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "name: ");
                try self.writer.styled(term.Color.Magenta, decl.name);
                try self.writer.plain("\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "external_name: ");
                try self.writer.styled(term.Color.Green, decl.external_name);
                try self.writer.plain("\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "type: ");
                const type_node = ast.Node{ .function_type = decl.type_annotation };
                try self.printNode(&type_node);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .module_decl => |decl| {
                try self.writer.styled(term.Color.Bold, "ModuleDecl");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "path: [");

                for (decl.path.segments.items, 0..) |segment, i| {
                    if (i > 0) {
                        try self.writer.plain(".");
                    }

                    try self.writer.styled(term.Color.Magenta, segment);
                }

                try self.writer.plain("]\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "exports: ");
                try self.printNode(&.{ .export_spec = decl.exports });

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "declarations: [\n");

                self.indent_level += 1;

                for (decl.declarations.items) |declaration| {
                    try self.printIndent();
                    try self.printNode(declaration);
                }

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.plain("]\n");

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .program => |prog| {
                try self.writer.styled(term.Color.Bold, "Program");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                for (prog.statements.items) |stmt| {
                    try self.printIndent();
                    try self.printNode(stmt);
                }

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
        }
    }

    /// Print the current indentation level.
    fn printIndent(self: *const AstPrinter) !void {
        var i: usize = 0;
        while (i < self.indent_level) : (i += 1) {
            try self.writer.styled(term.Color.Dim, "    ");
        }
    }

    fn printPattern(self: *AstPrinter, pattern: *const ast.PatternNode) !void {
        switch (pattern.*) {
            .wildcard => {
                try self.writer.styled(term.Color.Bold, "WildcardPattern");
                try self.writer.plain("(_)\n");
            },
            .int_literal => |lit| {
                try self.writer.styled(term.Color.Bold, "IntPattern");
                try self.writer.plain("(");
                try self.writer.styled(term.Color.Green, try std.fmt.allocPrint(
                    self.allocator,
                    "{d}",
                    .{lit.value},
                ));
                try self.writer.plain(")\n");
            },
            .float_literal => |lit| {
                try self.writer.styled(term.Color.Bold, "FloatPattern");
                try self.writer.plain("(");
                try self.writer.styled(term.Color.Green, try std.fmt.allocPrint(
                    self.allocator,
                    "{d}",
                    .{lit.value},
                ));
                try self.writer.plain(")\n");
            },
            .char_literal => |lit| {
                try self.writer.styled(term.Color.Bold, "CharPattern");
                try self.writer.plain("('");
                try self.writer.styled(term.Color.Green, try std.fmt.allocPrint(
                    self.allocator,
                    "{u}",
                    .{lit.value},
                ));
                try self.writer.plain("')\n");
            },
            .string_literal => |lit| {
                try self.writer.styled(term.Color.Bold, "StringPattern");
                try self.writer.plain("(\"");
                try self.writer.styled(term.Color.Green, lit.value);
                try self.writer.plain("\")\n");
            },
            .list => |list| {
                try self.writer.styled(term.Color.Bold, "ListPattern");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                for (list.patterns.items) |pat| {
                    try self.printIndent();
                    try self.printPattern(pat);
                }

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .variable => |var_pattern| {
                try self.writer.styled(term.Color.Bold, "VariablePattern");
                try self.writer.plain("(");
                try self.writer.styled(term.Color.Magenta, var_pattern.name);
                try self.writer.plain(")\n");
            },
            .constructor => |con| {
                try self.writer.styled(term.Color.Bold, "ConstructorPattern");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "name: ");
                try self.writer.styled(term.Color.Magenta, con.name);
                try self.writer.plain("\n");

                if (con.parameters.items.len > 0) {
                    try self.printIndent();
                    try self.writer.styled(term.Color.Cyan, "args: [\n");

                    self.indent_level += 1;

                    for (con.parameters.items) |param| {
                        try self.printIndent();
                        try self.printPattern(param);
                    }

                    self.indent_level -= 1;

                    try self.printIndent();
                    try self.writer.plain("]\n");
                }

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .empty_list => {
                try self.writer.styled(term.Color.Bold, "EmptyListPattern");
                try self.writer.plain("([])\n");
            },
            .cons => |cons| {
                try self.writer.styled(term.Color.Bold, "ConsPattern");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "head: ");
                try self.printPattern(cons.head);

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "tail: ");
                try self.printPattern(cons.tail);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
        }
    }
};

const testing = std.testing;

const lexer = @import("lexer.zig");

const TEST_FILE = "test.mox";

test "example" {
    if (true) return error.SkipZigTest;

    const allocator = testing.allocator;

    // Create a simple expression: 1 + 2 * 3

    const two = try allocator.create(ast.Node);
    defer allocator.destroy(two);

    two.* = .{
        .int_literal = .{
            .value = 2,
            .token = lexer.Token{
                .kind = .{ .literal = .Int },
                .lexeme = "2",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 4, .end = 5 },
                    .src = .{ .line = 1, .col = 5 },
                },
            },
        },
    };

    const three = try allocator.create(ast.Node);
    defer allocator.destroy(three);

    three.* = .{
        .int_literal = .{
            .value = 3,
            .token = lexer.Token{
                .kind = .{ .literal = .Int },
                .lexeme = "3",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 8, .end = 9 },
                    .src = .{ .line = 1, .col = 9 },
                },
            },
        },
    };

    const mul = try allocator.create(ast.Node);
    defer allocator.destroy(mul);

    mul.* = .{
        .arithmetic_expr = .{
            .left = two,
            .right = three,
            .operator = lexer.Token{
                .kind = .{ .operator = .IntMul },
                .lexeme = "*",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 6, .end = 7 },
                    .src = .{ .line = 1, .col = 7 },
                },
            },
        },
    };

    const one = try allocator.create(ast.Node);
    defer allocator.destroy(one);

    one.* = .{
        .int_literal = .{
            .value = 1,
            .token = lexer.Token{
                .kind = .{ .literal = .Int },
                .lexeme = "1",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    const add = try allocator.create(ast.Node);
    defer allocator.destroy(add);

    add.* = .{
        .arithmetic_expr = .{
            .left = one,
            .right = mul,
            .operator = lexer.Token{
                .kind = .{ .operator = .IntAdd },
                .lexeme = "+",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 2, .end = 3 },
                    .src = .{ .line = 1, .col = 3 },
                },
            },
        },
    };

    var printer = AstPrinter.init(allocator, std.io.getStdOut().writer());
    try printer.printNode(add);
}
