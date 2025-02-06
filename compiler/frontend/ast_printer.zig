const std = @import("std");

const ast = @import("ast.zig");
const term = @import("terminal.zig");

/// A debug utility for printing AST nodes in a readable tree format.
/// Useful for debugging the parser and examining expression structure.
pub const AstPrinter = struct {
    writer: term.ColorWriter,
    indent_level: usize = 0,
    indent_str: []const u8 = "  ",
    allocator: std.mem.Allocator,

    /// Initialize a new AST printer that writes to the given writer.
    pub fn init(allocator: std.mem.Allocator, writer: std.fs.File.Writer) AstPrinter {
        return .{
            .writer = term.ColorWriter.init(writer),
            .allocator = allocator,
        };
    }

    /// Print a complete AST node and its children.
    pub fn printNode(self: *AstPrinter, node: *const ast.Node) !void {
        switch (node.*) {
            .char_literal => |lit| {
                try self.writer.styled(term.Color.Bold, "CharacterLiteral");
                try self.writer.plain("('");
                try self.writer.styled(term.Color.Green, try std.fmt.allocPrint(
                    self.allocator,
                    "{u}",
                    .{lit.value},
                ));
                try self.writer.plain("')\n");
            },
            .float_literal => |lit| {
                try self.writer.styled(term.Color.Bold, "FloatLiteral");
                try self.writer.plain("(");
                try self.writer.styled(term.Color.Green, try std.fmt.allocPrint(
                    self.allocator,
                    "{d}",
                    .{lit.value},
                ));
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
            .str_literal => |lit| {
                try self.writer.styled(term.Color.Bold, "StringLiteral");
                try self.writer.plain("(\"");
                try self.writer.styled(term.Color.Green, lit.value);
                try self.writer.plain("\")\n");
            },
            .list => {
                try self.writer.styled(term.Color.Bold, "List");
                try self.writer.plain("(");
                try self.writer.styled(term.Color.Green, "type");
                try self.writer.plain(")\n");
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
            .lower_identifier => |id| {
                try self.writer.styled(term.Color.Bold, "LowerIdent");
                try self.writer.plain("(");
                try self.writer.styled(term.Color.Magenta, id.name);
                try self.writer.plain(")\n");
            },
            .upper_identifier => |id| {
                try self.writer.styled(term.Color.Bold, "UpperIdent");
                try self.writer.plain("(");
                try self.writer.styled(term.Color.Magenta, id.name);
                try self.writer.plain(")\n");
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
            .comment => |comment| {
                try self.writer.styled(term.Color.Bold, "Comment");
                try self.writer.plain("(");
                try self.writer.styled(term.Color.Green, comment.content);
                try self.writer.plain(")\n");
            },
            .doc_comment => |comment| {
                try self.writer.styled(term.Color.Bold, "DocComment");
                try self.writer.plain("(");
                try self.writer.styled(term.Color.Green, comment.content);
                try self.writer.plain(")\n");
            },
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
                    try self.printNode(type_annot);
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
                try self.printNode(decl.type_annotation);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .function_type => |type_node| {
                try self.writer.styled(term.Color.Bold, "FunctionType");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "params: [\n");

                self.indent_level += 1;

                for (type_node.param_types.items) |param_type| {
                    try self.printIndent();
                    try self.printNode(param_type);
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

                for (expr.params.items, 0..) |param, i| {
                    if (i > 0) try self.writer.plain(", ");

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
            .type_alias => |decl| {
                try self.writer.styled(term.Color.Bold, "TypeAlias");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "name: ");
                try self.writer.styled(term.Color.Magenta, decl.name);
                try self.writer.plain("\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "type_params: [");

                for (decl.type_params.items, 0..) |param, i| {
                    if (i > 0) try self.writer.plain(", ");

                    try self.writer.styled(term.Color.Magenta, param);
                }

                try self.writer.plain("]\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "value: ");
                try self.printNode(decl.value);

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
            .type_application => |app| {
                try self.writer.styled(term.Color.Bold, "TypeApplication");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "base: ");
                try self.printNode(app.base);

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
            .variant_type => |decl| {
                try self.writer.styled(term.Color.Bold, "VariantType");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "name: ");
                try self.writer.styled(term.Color.Magenta, decl.name);
                try self.writer.plain("\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "type_params: [");

                for (decl.type_params.items, 0..) |param, i| {
                    if (i > 0) try self.writer.plain(", ");

                    try self.writer.styled(term.Color.Magenta, param);
                }

                try self.writer.plain("]\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "constructors: [\n");

                self.indent_level += 1;

                for (decl.constructors.items) |constructor| {
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

                    for (constructor.params.items) |param| {
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
            .record_type => |decl| {
                try self.writer.styled(term.Color.Bold, "RecordType");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "name: ");
                try self.writer.styled(term.Color.Magenta, decl.name);
                try self.writer.plain("\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "type_params: [");

                for (decl.type_params.items, 0..) |param, i| {
                    if (i > 0) try self.writer.plain(", ");

                    try self.writer.styled(term.Color.Magenta, param);
                }

                try self.writer.plain("]\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "fields: [\n");

                self.indent_level += 1;

                for (decl.fields.items) |field| {
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
            .include => |inc| {
                try self.writer.styled(term.Color.Bold, "Include");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "path: [\n");

                self.indent_level += 1;

                for (inc.path.segments.items, 0..) |segment, i| {
                    try self.printIndent();

                    if (i > 0) try self.writer.plain(".");

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
            .module_path => |path| {
                try self.writer.styled(term.Color.Bold, "ModulePath");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "segments: [");

                for (path.segments.items, 0..) |segment, i| {
                    if (i > 0) try self.writer.plain(".");

                    try self.writer.styled(term.Color.Magenta, segment);
                }

                try self.writer.plain("]\n");

                self.indent_level -= 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Dim, "}\n");
            },
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
            .import_spec => |spec| {
                try self.writer.styled(term.Color.Bold, "ImportSpec");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "kind: ");
                try self.writer.styled(term.Color.Yellow, @tagName(spec.kind));
                try self.writer.plain("\n");

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "path: [");

                for (spec.path.segments.items, 0..) |segment, i| {
                    if (i > 0) try self.writer.plain(".");

                    try self.writer.styled(term.Color.Magenta, segment);
                }

                try self.writer.plain("]\n");

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

                        switch (item) {
                            .name => |name| {
                                try self.writer.styled(term.Color.Bold, "Name");
                                try self.writer.plain("(");
                                try self.writer.styled(term.Color.Magenta, name);
                                try self.writer.plain(")");
                            },
                            .rename => |rename| {
                                try self.writer.styled(term.Color.Bold, "Rename");
                                try self.writer.plain("(");
                                try self.writer.styled(term.Color.Magenta, rename.old_name);
                                try self.writer.plain(" to ");
                                try self.writer.styled(term.Color.Magenta, rename.new_name);
                                try self.writer.plain(")");
                            },
                        }

                        try self.writer.plain("\n");
                    }

                    self.indent_level -= 1;

                    try self.printIndent();
                    try self.writer.plain("]\n");
                }

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
            .module_decl => |decl| {
                try self.writer.styled(term.Color.Bold, "ModuleDecl");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "path: [");

                for (decl.path.segments.items, 0..) |segment, i| {
                    if (i > 0) try self.writer.plain(".");

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
            .match_expr => |expr| {
                try self.writer.styled(term.Color.Bold, "MatchExpr");
                try self.writer.styled(term.Color.Dim, " {\n");

                self.indent_level += 1;

                try self.printIndent();
                try self.writer.styled(term.Color.Cyan, "value: ");
                try self.printNode(expr.value);

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
            else => {
                try self.writer.styled(term.Color.Bold, "Node");
                try self.writer.plain("(");
                try self.writer.styled(term.Color.Red, @tagName(node.*));
                try self.writer.plain(")\n");
            },
        }
    }

    /// Print the current indentation level.
    fn printIndent(self: *const AstPrinter) !void {
        var i: usize = 0;
        while (i < self.indent_level) : (i += 1) {
            try self.writer.styled(term.Color.Dim, self.indent_str);
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

                for (list.elements.items) |element| {
                    try self.printIndent();
                    try self.printPattern(element);
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

                if (con.args.items.len > 0) {
                    try self.printIndent();
                    try self.writer.styled(term.Color.Cyan, "args: [\n");

                    self.indent_level += 1;

                    for (con.args.items) |arg| {
                        try self.printIndent();
                        try self.printPattern(arg);
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
            .operator = lexer.Token{
                .kind = .{ .operator = .IntMul },
                .lexeme = "*",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 6, .end = 7 },
                    .src = .{ .line = 1, .col = 7 },
                },
            },
            .right = three,
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
            .operator = lexer.Token{
                .kind = .{ .operator = .IntAdd },
                .lexeme = "+",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 2, .end = 3 },
                    .src = .{ .line = 1, .col = 3 },
                },
            },
            .right = mul,
        },
    };

    var printer = AstPrinter.init(allocator, std.io.getStdOut().writer());
    try printer.printNode(add);
}
