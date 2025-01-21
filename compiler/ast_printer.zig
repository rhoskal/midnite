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
    pub fn init(writer: std.fs.File.Writer, allocator: std.mem.Allocator) AstPrinter {
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
                try self.printIndent();
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
                .kind = .LitInt,
                .lexeme = "2",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 4, .end = 5 },
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
                .kind = .LitInt,
                .lexeme = "3",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 8, .end = 9 },
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
                .kind = .OpIntMul,
                .lexeme = "*",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 6, .end = 7 },
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
                .kind = .LitInt,
                .lexeme = "1",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 0, .end = 1 },
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
                .kind = .OpIntAdd,
                .lexeme = "+",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 2, .end = 3 },
                    .src = .{ .line = 1, .col = 3 },
                },
            },
            .right = mul,
        },
    };

    var printer = AstPrinter.init(std.io.getStdOut().writer(), allocator);
    try printer.printNode(add);
}
