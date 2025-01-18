const std = @import("std");

const lexer = @import("lexer.zig");

/// Represents a regular, single line comment.
pub const CommentNode = struct {
    content: []const u8,
    token: lexer.Token,
};

/// Represents a documentation comment that will be processed as markdown.
pub const DocCommentNode = struct {
    content: []const u8,
    token: lexer.Token,
};

/// Represents a literal integer value.
pub const IntLiteralNode = struct {
    value: i64,
    token: lexer.Token,
};

/// Represents a literal floating-point value.
pub const FloatLiteralNode = struct {
    value: f64,
    token: lexer.Token,
};

/// Represents a string literal value.
pub const StrLiteralNode = struct {
    value: []const u8,
    token: lexer.Token,
};

/// Represents a multiline string literal value.
pub const MultilineStrLiteralNode = struct {
    value: []const u8,
    token: lexer.Toke,
};

/// Represents a char literal value.
pub const CharLiteralNode = struct {
    value: u21,
    token: lexer.Token,
};

/// Common structure for binary operations.
pub const BinaryOp = struct {
    left: *Node,
    operator: lexer.Token,
    right: *Node,
};

/// A binary operation node representing arithmetic operations.
///
/// Examples:
/// - Integer arithmetic: (+, -, *, /)
/// - Floating-point arithmetic: (+., -., *., /., **)
pub const ArithmeticExprNode = BinaryOp;

/// A binary operation node representing logical operations.
///
/// Examples:
/// - Logical: (&&, ||)
pub const LogicalExprNode = BinaryOp;

/// A binary operation node representing comparisons.
///
/// Examples:
/// - Comparison: (<, >, <=, >=)
/// - Equality: (==, !=)
pub const ComparisonExprNode = BinaryOp;

/// A unary operation node representing operations with only one operand.
///
/// Examples:
/// - Negation: (-)
pub const UnaryExprNode = struct {
    operator: lexer.Token,
    operand: *Node,
};

pub const IfThenElseStmtNode = struct {
    condition: *Node,
    then_branch: *Node,
    else_branch: *Node,
};

/// Represents a function type annotation.
///
/// Examples:
/// - `Int -> Int -> Int`
pub const FunctionTypeNode = struct {
    /// The parameter types in order.
    /// For curried functions like Int -> Int -> Int,
    /// this would contain [Int, Int, Int] where the last
    /// one is the return type.
    param_types: std.ArrayList(*Node),
    token: lexer.Token,
};

/// Represents a lambda expression.
///
/// Examples:
/// - `\x y => x + y`
pub const LambdaExprNode = struct {
    params: std.ArrayList([]const u8), // Order matters!
    body: *Node,
    token: lexer.Token,
};

/// Represents a top-level function definition.
///
/// Examples:
/// - `let add : Int -> Int -> Int = \x y => x + y`
pub const FunctionDeclNode = struct {
    name: []const u8,
    type_annotation: ?*Node,
    // doc_comments: []*DocCommentNode,
    value: *Node,
    token: lexer.Token,
};

/// Represents a lowercase identifier reference (variable names, function names, etc).
pub const LowerIdentifierNode = struct {
    name: []const u8,
    token: lexer.Token,
};

/// Represents an uppercase identifier reference (type names, type constructors, etc).
pub const UpperIdentifierNode = struct {
    name: []const u8,
    token: lexer.Token,
};

/// Represents a typed hole - a placeholder for a type that should be inferred.
pub const TypedHoleNode = struct {
    token: lexer.Token,
};

/// The root node of the AST containing all top-level declarations.
pub const ProgramNode = struct {
    statements: std.ArrayList(*Node),
    token: lexer.Token,
};

pub const Node = union(enum) {
    // Comments
    comment: CommentNode,
    doc_comment: DocCommentNode,

    // Literal Types
    integer_literal: IntLiteralNode,
    float_literal: FloatLiteralNode,
    string_literal: StrLiteralNode,
    char_literal: CharLiteralNode,

    // Expressions
    arithmetic_expr: ArithmeticExprNode,
    logical_expr: LogicalExprNode,
    comparison_expr: ComparisonExprNode,
    unary_expr: UnaryExprNode,
    lambda_expr: LambdaExprNode,

    // Statements
    if_then_else_stmt: IfThenElseStmtNode,

    // Declarations
    function_decl: FunctionDeclNode,

    // Types
    function_type: FunctionTypeNode,
    typed_hole: TypedHoleNode,

    // Identifiers
    lower_identifier: LowerIdentifierNode,
    upper_identifier: UpperIdentifierNode,

    // Other
    program: ProgramNode,

    /// Cleans up resources associated with this node.
    ///
    /// - `allocator`: The memory allocator used to deallocate child nodes.
    ///
    /// Recursively deinitializes any child nodes that this node owns,
    /// ensuring no memory leaks occur.
    pub fn deinit(self: *Node, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .arithmetic_expr => |*expr| {
                expr.left.deinit(allocator);
                expr.right.deinit(allocator);
                allocator.destroy(expr.left);
                allocator.destroy(expr.right);
            },
            .logical_expr => |*expr| {
                expr.left.deinit(allocator);
                expr.right.deinit(allocator);
                allocator.destroy(expr.left);
                allocator.destroy(expr.right);
            },
            .comparison_expr => |*expr| {
                expr.left.deinit(allocator);
                expr.right.deinit(allocator);
                allocator.destroy(expr.left);
                allocator.destroy(expr.right);
            },
            .unary_expr => |*expr| {
                expr.operand.deinit(allocator);
                allocator.destroy(expr.operand);
            },
            .lambda_expr => |*expr| {
                expr.params.deinit();
                expr.body.deinit(allocator);
                allocator.destroy(expr.body);
            },
            .if_then_else_stmt => |*stmt| {
                stmt.condition.deinit(allocator);
                stmt.else_branch.deinit(allocator);
                stmt.then_branch.deinit(allocator);
                allocator.destroy(stmt.condition);
                allocator.destroy(stmt.else_branch);
                allocator.destroy(stmt.then_branch);
            },
            .function_decl => |*decl| {
                if (decl.type_annotation) |type_annotation| {
                    type_annotation.deinit(allocator);
                    allocator.destroy(type_annotation);
                }

                decl.value.deinit(allocator);
                allocator.destroy(decl.value);
            },
            .function_type => |*ftype| {
                for (ftype.param_types.items) |t| {
                    t.deinit(allocator);
                    allocator.destroy(t);
                }

                ftype.param_types.deinit();
                allocator.destroy(ftype);
            },
            .program => |*prog| {
                for (prog.statements.items) |stmt| {
                    stmt.deinit(allocator);
                    allocator.destroy(stmt);
                }

                prog.statements.deinit();
            },
            else => {}, // Other nodes don't own any memory
        }
    }
};

const testing = std.testing;

const TEST_FILE = "test.mox";

test "BinaryExprNode memory management" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // 42 + 24

    const left = try allocator.create(Node);
    left.* = .{
        .integer_literal = .{
            .value = 42,
            .token = lexer.Token{
                .kind = .LitInt,
                .lexeme = "42",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 0, .end = 2 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    const right = try allocator.create(Node);
    right.* = .{
        .integer_literal = .{
            .value = 24,
            .token = lexer.Token{
                .kind = .LitInt,
                .lexeme = "24",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 6, .end = 8 },
                    .src = .{ .line = 1, .col = 7 },
                },
            },
        },
    };

    var binary = try allocator.create(Node);
    binary.* = .{
        .arithmetic_expr = .{
            .left = left,
            .operator = lexer.Token{
                .kind = .OpIntAdd,
                .lexeme = "+",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 3, .end = 3 },
                    .src = .{ .line = 1, .col = 4 },
                },
            },
            .right = right,
        },
    };

    try testing.expect(binary.arithmetic_expr.left.integer_literal.value == 42);
    try testing.expect(binary.arithmetic_expr.right.integer_literal.value == 24);
    try testing.expectEqual(lexer.TokenKind.OpIntAdd, binary.arithmetic_expr.operator.kind);

    binary.deinit(allocator);
    allocator.destroy(binary);
}

test "UnaryExprNode memory management" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // -42

    const operand = try allocator.create(Node);
    operand.* = .{
        .integer_literal = .{
            .value = 42,
            .token = lexer.Token{
                .kind = .LitInt,
                .lexeme = "42",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 1, .end = 3 },
                    .src = .{ .line = 1, .col = 2 },
                },
            },
        },
    };

    var unary = try allocator.create(Node);
    unary.* = .{
        .unary_expr = .{
            .operator = lexer.Token{
                .kind = .OpIntSub,
                .lexeme = "-",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
            .operand = operand,
        },
    };

    try testing.expectEqual(lexer.TokenKind.OpIntSub, unary.unary_expr.operator.kind);
    try testing.expectEqualStrings("-", unary.unary_expr.operator.lexeme);
    try testing.expectEqual(lexer.TokenKind.LitInt, unary.unary_expr.operand.integer_literal.token.kind);
    try testing.expect(unary.unary_expr.operand.integer_literal.value == 42);

    unary.deinit(allocator);
    allocator.destroy(unary);
}

test "LambdaExprNode memory management" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // \x y => x + y

    var params = std.ArrayList([]const u8).init(allocator);
    try params.append("x");
    try params.append("y");

    const left = try allocator.create(Node);
    left.* = .{
        .lower_identifier = .{
            .name = "x",
            .token = lexer.Token{
                .kind = .LowerIdent,
                .lexeme = "x",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 7, .end = 8 },
                    .src = .{ .line = 1, .col = 8 },
                },
            },
        },
    };

    const right = try allocator.create(Node);
    right.* = .{
        .lower_identifier = .{
            .name = "y",
            .token = lexer.Token{
                .kind = .LowerIdent,
                .lexeme = "y",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 10, .end = 11 },
                    .src = .{ .line = 1, .col = 11 },
                },
            },
        },
    };

    const body = try allocator.create(Node);
    body.* = .{
        .arithmetic_expr = .{
            .left = left,
            .operator = lexer.Token{
                .kind = .OpIntAdd,
                .lexeme = "+",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 8, .end = 9 },
                    .src = .{ .line = 1, .col = 9 },
                },
            },
            .right = right,
        },
    };

    const lambda = try allocator.create(Node);
    lambda.* = .{
        .lambda_expr = .{
            .params = params,
            .body = body,
            .token = lexer.Token{
                .kind = .OpLambda,
                .lexeme = "\\",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    try testing.expectEqual(@as(usize, 2), lambda.lambda_expr.params.items.len);
    try testing.expectEqualStrings("x", lambda.lambda_expr.params.items[0]);
    try testing.expectEqualStrings("y", lambda.lambda_expr.params.items[1]);

    try testing.expectEqual(lexer.TokenKind.OpLambda, lambda.lambda_expr.token.kind);
    try testing.expectEqualStrings("\\", lambda.lambda_expr.token.lexeme);

    const _body = lambda.lambda_expr.body.arithmetic_expr;
    try testing.expectEqual(lexer.TokenKind.OpIntAdd, _body.operator.kind);
    try testing.expectEqualStrings("+", _body.operator.lexeme);

    try testing.expect(_body.left.* == .lower_identifier);
    try testing.expectEqualStrings("x", _body.left.lower_identifier.name);
    try testing.expectEqual(lexer.TokenKind.LowerIdent, _body.left.lower_identifier.token.kind);

    lambda.deinit(allocator);
    allocator.destroy(lambda);
}

test "IfThenElseStmtNode memory management" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // if x == y then True else False

    const left = try allocator.create(Node);
    left.* = .{
        .lower_identifier = .{
            .name = "x",
            .token = lexer.Token{
                .kind = .LowerIdent,
                .lexeme = "x",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 3, .end = 4 },
                    .src = .{ .line = 1, .col = 4 },
                },
            },
        },
    };

    const right = try allocator.create(Node);
    right.* = .{
        .lower_identifier = .{
            .name = "y",
            .token = lexer.Token{
                .kind = .LowerIdent,
                .lexeme = "y",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 8, .end = 9 },
                    .src = .{ .line = 1, .col = 9 },
                },
            },
        },
    };

    const condition = try allocator.create(Node);
    condition.* = .{
        .comparison_expr = .{
            .left = left,
            .operator = lexer.Token{
                .kind = .OpEquality,
                .lexeme = "==",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 5, .end = 7 },
                    .src = .{ .line = 1, .col = 6 },
                },
            },
            .right = right,
        },
    };

    const then_branch = try allocator.create(Node);
    then_branch.* = .{
        .upper_identifier = .{
            .name = "True",
            .token = lexer.Token{
                .kind = .UpperIdent,
                .lexeme = "True",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 15, .end = 19 },
                    .src = .{ .line = 1, .col = 16 },
                },
            },
        },
    };

    const else_branch = try allocator.create(Node);
    else_branch.* = .{
        .upper_identifier = .{
            .name = "False",
            .token = lexer.Token{
                .kind = .UpperIdent,
                .lexeme = "False",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 25, .end = 30 },
                    .src = .{ .line = 1, .col = 26 },
                },
            },
        },
    };

    const if_then_else = try allocator.create(Node);
    if_then_else.* = .{
        .if_then_else_stmt = .{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
        },
    };

    // Test condition (x == y)
    const _condition = if_then_else.if_then_else_stmt.condition.comparison_expr;
    try testing.expect(if_then_else.if_then_else_stmt.condition.* == .comparison_expr);
    try testing.expectEqual(lexer.TokenKind.OpEquality, _condition.operator.kind);
    try testing.expectEqualStrings("==", _condition.operator.lexeme);

    // Test left side of condition (x)
    try testing.expect(_condition.left.* == .lower_identifier);
    try testing.expectEqualStrings("x", _condition.left.lower_identifier.name);
    try testing.expectEqual(lexer.TokenKind.LowerIdent, _condition.left.lower_identifier.token.kind);

    // Test right side of condition (y)
    try testing.expect(_condition.right.* == .lower_identifier);
    try testing.expectEqualStrings("y", _condition.right.lower_identifier.name);
    try testing.expectEqual(lexer.TokenKind.LowerIdent, _condition.right.lower_identifier.token.kind);

    // Test then branch (True)
    try testing.expect(if_then_else.if_then_else_stmt.then_branch.* == .upper_identifier);
    try testing.expectEqualStrings("True", if_then_else.if_then_else_stmt.then_branch.upper_identifier.name);
    try testing.expectEqual(lexer.TokenKind.UpperIdent, if_then_else.if_then_else_stmt.then_branch.upper_identifier.token.kind);

    // Test else branch (False)
    try testing.expect(if_then_else.if_then_else_stmt.else_branch.* == .upper_identifier);
    try testing.expectEqualStrings("False", if_then_else.if_then_else_stmt.else_branch.upper_identifier.name);
    try testing.expectEqual(lexer.TokenKind.UpperIdent, if_then_else.if_then_else_stmt.else_branch.upper_identifier.token.kind);

    if_then_else.deinit(allocator);
    allocator.destroy(if_then_else);
}

test "FunctionTypeNode memory management" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // : Int -> Int -> Int

    var param_types = std.ArrayList(*Node).init(allocator);

    const int_type1 = try allocator.create(Node);
    int_type1.* = .{
        .upper_identifier = .{
            .name = "Int",
            .token = lexer.Token{
                .kind = .UpperIdent,
                .lexeme = "Int",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 2, .end = 5 },
                    .src = .{ .line = 1, .col = 3 },
                },
            },
        },
    };

    const int_type2 = try allocator.create(Node);
    int_type2.* = .{
        .upper_identifier = .{
            .name = "Int",
            .token = lexer.Token{
                .kind = .UpperIdent,
                .lexeme = "Int",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 9, .end = 12 },
                    .src = .{ .line = 1, .col = 10 },
                },
            },
        },
    };

    const int_type3 = try allocator.create(Node);
    int_type3.* = .{
        .upper_identifier = .{
            .name = "Int",
            .token = lexer.Token{
                .kind = .UpperIdent,
                .lexeme = "Int",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 16, .end = 19 },
                    .src = .{ .line = 1, .col = 17 },
                },
            },
        },
    };

    try param_types.append(int_type1);
    try param_types.append(int_type2);
    try param_types.append(int_type3);

    var func_type = try allocator.create(Node);
    func_type.* = .{
        .function_type = .{
            .param_types = param_types,
            .token = lexer.Token{
                .kind = .DelColon,
                .lexeme = ":",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    // Test the structure
    try testing.expectEqual(@as(usize, 3), func_type.function_type.param_types.items.len);
    try testing.expectEqual(lexer.TokenKind.DelColon, func_type.function_type.token.kind);
    try testing.expectEqualStrings(":", func_type.function_type.token.lexeme);

    // Test each Int type
    for (func_type.function_type.param_types.items) |type_node| {
        try testing.expect(type_node.* == .upper_identifier);
        try testing.expectEqualStrings("Int", type_node.upper_identifier.name);
        try testing.expectEqual(lexer.TokenKind.UpperIdent, type_node.upper_identifier.token.kind);
        try testing.expectEqualStrings("Int", type_node.upper_identifier.token.lexeme);
    }

    // Test specific positions of each Int
    const first_int = func_type.function_type.param_types.items[0];
    try testing.expectEqual(@as(usize, 2), first_int.upper_identifier.token.loc.buf.start);
    try testing.expectEqual(@as(usize, 3), first_int.upper_identifier.token.loc.src.col);

    const second_int = func_type.function_type.param_types.items[1];
    try testing.expectEqual(@as(usize, 9), second_int.upper_identifier.token.loc.buf.start);
    try testing.expectEqual(@as(usize, 10), second_int.upper_identifier.token.loc.src.col);

    const third_int = func_type.function_type.param_types.items[2];
    try testing.expectEqual(@as(usize, 16), third_int.upper_identifier.token.loc.buf.start);
    try testing.expectEqual(@as(usize, 17), third_int.upper_identifier.token.loc.src.col);

    func_type.deinit(allocator);
    allocator.destroy(func_type);
}

test "FunctionDeclNode memory management" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // let add : Int -> Int -> Int = \x y => x + y

    var param_types = std.ArrayList(*Node).init(allocator);

    const int_type1 = try allocator.create(Node);
    int_type1.* = .{
        .upper_identifier = .{
            .name = "Int",
            .token = lexer.Token{
                .kind = .UpperIdent,
                .lexeme = "Int",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 9, .end = 12 },
                    .src = .{ .line = 1, .col = 10 },
                },
            },
        },
    };

    const int_type2 = try allocator.create(Node);
    int_type2.* = .{
        .upper_identifier = .{
            .name = "Int",
            .token = lexer.Token{
                .kind = .UpperIdent,
                .lexeme = "Int",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 16, .end = 19 },
                    .src = .{ .line = 1, .col = 17 },
                },
            },
        },
    };

    const int_type3 = try allocator.create(Node);
    int_type3.* = .{
        .upper_identifier = .{
            .name = "Int",
            .token = lexer.Token{
                .kind = .UpperIdent,
                .lexeme = "Int",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 23, .end = 26 },
                    .src = .{ .line = 1, .col = 24 },
                },
            },
        },
    };

    try param_types.append(int_type1);
    try param_types.append(int_type2);
    try param_types.append(int_type3);

    const func_type = try allocator.create(Node);
    func_type.* = .{
        .function_type = .{
            .param_types = param_types,
            .token = lexer.Token{
                .kind = .DelColon,
                .lexeme = ":",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 7, .end = 8 },
                    .src = .{ .line = 1, .col = 8 },
                },
            },
        },
    };

    // Create lambda expression (\x y => x + y)
    var params = std.ArrayList([]const u8).init(allocator);
    try params.append("x");
    try params.append("y");

    const left = try allocator.create(Node);
    left.* = .{
        .lower_identifier = .{
            .name = "x",
            .token = lexer.Token{
                .kind = .LowerIdent,
                .lexeme = "x",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 37, .end = 38 },
                    .src = .{ .line = 1, .col = 38 },
                },
            },
        },
    };

    const right = try allocator.create(Node);
    right.* = .{
        .lower_identifier = .{
            .name = "y",
            .token = lexer.Token{
                .kind = .LowerIdent,
                .lexeme = "y",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 41, .end = 42 },
                    .src = .{ .line = 1, .col = 42 },
                },
            },
        },
    };

    const body = try allocator.create(Node);
    body.* = .{
        .arithmetic_expr = .{
            .left = left,
            .operator = lexer.Token{
                .kind = .OpIntAdd,
                .lexeme = "+",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 39, .end = 40 },
                    .src = .{ .line = 1, .col = 40 },
                },
            },
            .right = right,
        },
    };

    const lambda = try allocator.create(Node);
    lambda.* = .{
        .lambda_expr = .{
            .params = params,
            .body = body,
            .token = lexer.Token{
                .kind = .OpLambda,
                .lexeme = "\\",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 30, .end = 31 },
                    .src = .{ .line = 1, .col = 31 },
                },
            },
        },
    };

    var func_decl = try allocator.create(Node);
    func_decl.* = .{
        .function_decl = .{
            .name = "add",
            .type_annotation = func_type,
            .value = lambda,
            .token = lexer.Token{
                .kind = .KwLet,
                .lexeme = "let",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 0, .end = 3 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    // Test overall structure
    try testing.expectEqualStrings("add", func_decl.function_decl.name);
    try testing.expectEqual(lexer.TokenKind.KwLet, func_decl.function_decl.token.kind);
    try testing.expectEqualStrings("let", func_decl.function_decl.token.lexeme);

    // Test type annotation
    const type_annot = func_decl.function_decl.type_annotation.?;
    try testing.expect(type_annot.* == .function_type);
    try testing.expectEqual(@as(usize, 3), type_annot.function_type.param_types.items.len);
    try testing.expectEqual(lexer.TokenKind.DelColon, type_annot.function_type.token.kind);

    // Test each Int in type signature
    for (type_annot.function_type.param_types.items) |type_node| {
        try testing.expect(type_node.* == .upper_identifier);
        try testing.expectEqualStrings("Int", type_node.upper_identifier.name);
        try testing.expectEqual(lexer.TokenKind.UpperIdent, type_node.upper_identifier.token.kind);
    }

    // Test lambda expression
    const lambda_value = func_decl.function_decl.value;
    try testing.expect(lambda_value.* == .lambda_expr);
    try testing.expectEqual(@as(usize, 2), lambda_value.lambda_expr.params.items.len);
    try testing.expectEqualStrings("x", lambda_value.lambda_expr.params.items[0]);
    try testing.expectEqualStrings("y", lambda_value.lambda_expr.params.items[1]);
    try testing.expectEqual(lexer.TokenKind.OpLambda, lambda_value.lambda_expr.token.kind);

    // Test lambda body
    const lambda_body = lambda_value.lambda_expr.body;
    try testing.expect(lambda_body.* == .arithmetic_expr);
    try testing.expectEqual(lexer.TokenKind.OpIntAdd, lambda_body.arithmetic_expr.operator.kind);
    try testing.expectEqualStrings("+", lambda_body.arithmetic_expr.operator.lexeme);

    const body_left = lambda_body.arithmetic_expr.left;
    try testing.expect(body_left.* == .lower_identifier);
    try testing.expectEqualStrings("x", body_left.lower_identifier.name);

    const body_right = lambda_body.arithmetic_expr.right;
    try testing.expect(body_right.* == .lower_identifier);
    try testing.expectEqualStrings("y", body_right.lower_identifier.name);

    func_decl.deinit(allocator);
    allocator.destroy(func_decl);
}
