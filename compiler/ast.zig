const std = @import("std");

const lexer = @import("lexer.zig");
const Token = lexer.Token;

/// Represents a regular, single line comment.
pub const CommentNode = struct {
    content: []const u8,
    token: Token,
};

/// Represents a documentation comment that will be processed as markdown.
pub const DocCommentNode = struct {
    content: []const u8,
    token: Token,
};

/// Represents a literal integer value.
pub const IntegerLiteralNode = struct {
    value: i64,
    token: Token,
};

/// Represents a literal floating-point value.
pub const FloatLiteralNode = struct {
    value: f64,
    token: Token,
};

/// Represents a string literal value.
pub const StringLiteralNode = struct {
    value: []const u8,
    token: Token,
};

/// Represents a char literal value.
pub const CharLiteralNode = struct {
    value: u21, // Unicode codepoint
    token: Token,
};

/// Common structure for binary operations.
pub const BinaryOp = struct {
    left: *Node,
    operator: Token,
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
/// - Negation: `-`
pub const UnaryExprNode = struct {
    operator: Token,
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
    token: Token,
};

/// Represents a lambda expression.
///
/// Examples:
/// - `\x y => x + y`
pub const LambdaExprNode = struct {
    params: std.ArrayList([]const u8), // Order matters!
    body: *Node,
    token: Token,
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
    token: Token,
};

/// Represents a lowercase identifier reference (variable names, function names, etc).
pub const LowerIdentifierNode = struct {
    name: []const u8,
    token: Token,
};

/// Represents an uppercase identifier reference (type names, type constructors, etc).
pub const UpperIdentifierNode = struct {
    name: []const u8,
    token: Token,
};

/// Represents a typed hole - a placeholder for a type that should be inferred.
pub const TypedHoleNode = struct {
    token: Token,
};

/// The root node of the AST containing all top-level declarations.
pub const ProgramNode = struct {
    statements: std.ArrayList(*Node),
    token: Token,
};

pub const Node = union(enum) {
    // Comments
    comment: CommentNode,
    doc_comment: DocCommentNode,

    // Literal Types
    integer_literal: IntegerLiteralNode,
    float_literal: FloatLiteralNode,
    string_literal: StringLiteralNode,
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

test "BinaryExprNode memory management" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // 42 + 24

    const left = try allocator.create(Node);
    left.* = .{
        .integer_literal = .{
            .value = 42,
            .token = Token{
                .kind = .LitInt,
                .lexeme = "42",
                .line = 1,
                .column = 1,
            },
        },
    };

    const right = try allocator.create(Node);
    right.* = .{
        .integer_literal = .{
            .value = 24,
            .token = Token{
                .kind = .LitInt,
                .lexeme = "24",
                .line = 1,
                .column = 5,
            },
        },
    };

    var binary = try allocator.create(Node);
    binary.* = .{
        .arithmetic_expr = .{
            .left = left,
            .operator = Token{
                .kind = .OpIntAdd,
                .lexeme = "+",
                .line = 1,
                .column = 3,
            },
            .right = right,
        },
    };

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
            .token = Token{
                .kind = .LitInt,
                .lexeme = "42",
                .line = 1,
                .column = 2,
            },
        },
    };

    var unary = try allocator.create(Node);
    unary.* = .{
        .unary_expr = .{
            .operator = Token{
                .kind = .OpIntSub,
                .lexeme = "-",
                .line = 1,
                .column = 1,
            },
            .operand = operand,
        },
    };

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
            .token = Token{
                .kind = .LowerIdent,
                .lexeme = "x",
                .line = 1,
                .column = 9,
            },
        },
    };

    const right = try allocator.create(Node);
    right.* = .{
        .lower_identifier = .{
            .name = "y",
            .token = Token{
                .kind = .LowerIdent,
                .lexeme = "y",
                .line = 1,
                .column = 13,
            },
        },
    };

    const body = try allocator.create(Node);
    body.* = .{
        .arithmetic_expr = .{
            .left = left,
            .operator = Token{
                .kind = .OpIntAdd,
                .lexeme = "+",
                .line = 1,
                .column = 11,
            },
            .right = right,
        },
    };

    const lambda = try allocator.create(Node);
    lambda.* = .{
        .lambda_expr = .{
            .params = params,
            .body = body,
            .token = Token{
                .kind = .OpLambda,
                .lexeme = "\\",
                .line = 1,
                .column = 1,
            },
        },
    };

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
            .token = Token{
                .kind = .LowerIdent,
                .lexeme = "x",
                .line = 1,
                .column = 4,
            },
        },
    };

    const right = try allocator.create(Node);
    right.* = .{
        .lower_identifier = .{
            .name = "y",
            .token = Token{
                .kind = .LowerIdent,
                .lexeme = "y",
                .line = 1,
                .column = 9,
            },
        },
    };

    const condition = try allocator.create(Node);
    condition.* = .{
        .comparison_expr = .{
            .left = left,
            .operator = Token{
                .kind = .OpEquality,
                .lexeme = "==",
                .line = 1,
                .column = 6,
            },
            .right = right,
        },
    };

    const then_branch = try allocator.create(Node);
    then_branch.* = .{
        .upper_identifier = .{
            .name = "True",
            .token = Token{
                .kind = .UpperIdent,
                .lexeme = "True",
                .line = 1,
                .column = 16,
            },
        },
    };

    const else_branch = try allocator.create(Node);
    else_branch.* = .{
        .upper_identifier = .{
            .name = "False",
            .token = Token{
                .kind = .UpperIdent,
                .lexeme = "False",
                .line = 1,
                .column = 26,
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

    if_then_else.deinit(allocator);
    allocator.destroy(if_then_else);
}

test "FunctionTypeNode memory management" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // : Int -> Int -> Int

    var param_types = std.ArrayList(*Node).init(allocator);

    for (0..3) |i| {
        const int_type = try allocator.create(Node);
        int_type.* = .{
            .upper_identifier = .{
                .name = "Int",
                .token = Token{
                    .kind = .UpperIdent,
                    .lexeme = "Int",
                    .line = 1,
                    .column = 3 + i * 7, // 3, 10, 17
                },
            },
        };

        try param_types.append(int_type);
    }

    var func_type = try allocator.create(Node);
    func_type.* = .{
        .function_type = .{
            .param_types = param_types,
            .token = Token{
                .kind = .DelColon,
                .lexeme = ":",
                .line = 1,
                .column = 1,
            },
        },
    };

    func_type.deinit(allocator);
    allocator.destroy(func_type);
}

test "FunctionDeclNode memory management" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // let add : Int -> Int -> Int = \x y => x + y

    var param_types = std.ArrayList(*Node).init(allocator);

    for (0..3) |i| {
        const int_type = try allocator.create(Node);
        int_type.* = .{
            .upper_identifier = .{
                .name = "Int",
                .token = Token{
                    .kind = .UpperIdent,
                    .lexeme = "Int",
                    .line = 1,
                    .column = 11 + i * 7, // 11, 18, 25
                },
            },
        };

        try param_types.append(int_type);
    }

    const func_type = try allocator.create(Node);
    func_type.* = .{
        .function_type = .{
            .param_types = param_types,
            .token = Token{
                .kind = .DelColon,
                .lexeme = ":",
                .line = 1,
                .column = 9,
            },
        },
    };

    var params = std.ArrayList([]const u8).init(allocator);
    try params.append("x");
    try params.append("y");

    const left = try allocator.create(Node);
    left.* = .{
        .lower_identifier = .{
            .name = "x",
            .token = Token{
                .kind = .LowerIdent,
                .lexeme = "x",
                .line = 1,
                .column = 9,
            },
        },
    };

    const right = try allocator.create(Node);
    right.* = .{
        .lower_identifier = .{
            .name = "y",
            .token = Token{
                .kind = .LowerIdent,
                .lexeme = "y",
                .line = 1,
                .column = 13,
            },
        },
    };

    const body = try allocator.create(Node);
    body.* = .{
        .arithmetic_expr = .{
            .left = left,
            .operator = Token{
                .kind = .OpIntAdd,
                .lexeme = "+",
                .line = 1,
                .column = 11,
            },
            .right = right,
        },
    };

    const lambda = try allocator.create(Node);
    lambda.* = .{
        .lambda_expr = .{
            .params = params,
            .body = body,
            .token = Token{
                .kind = .OpLambda,
                .lexeme = "\\",
                .line = 1,
                .column = 1,
            },
        },
    };

    var func_decl = try allocator.create(Node);
    func_decl.* = .{
        .function_decl = .{
            .name = "add",
            .type_annotation = func_type,
            .value = lambda,
            .token = Token{
                .kind = .KwLet,
                .lexeme = "let",
                .line = 1,
                .column = 1,
            },
        },
    };

    func_decl.deinit(allocator);
    allocator.destroy(func_decl);
}

// test "ProgramNode memory management" {}
