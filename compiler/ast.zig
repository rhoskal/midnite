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

/// Represents a list of comma-separated expressions surrounded by square brackets.
///
/// Examples:
/// - `[1, 2, 3]`
/// - `[]` (empty list)
/// - `[True, False, True]`
pub const ListNode = struct {
    elements: std.ArrayList(*Node),
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

/// Represents a complete match expression that tests a value against multiple patterns.
/// Each pattern is paired with an expression to evaluate when that pattern matches.
///
/// Examples:
/// - `match x on | 0 => "zero" | _ => "other"`
/// - `match list on | [] => 0 | x :: xs => 1 + length xs`
/// - `match result on | Ok v => v | Err e => default`
pub const MatchExprNode = struct {
    value: *Node,
    cases: std.ArrayList(MatchCase),
    token: lexer.Token,
};

/// Represents a single case in a match expression, consisting of
/// a pattern to test and an expression to evaluate on match.
pub const MatchCase = struct {
    pattern: *PatternNode,
    expression: *Node,
    guard: ?*GuardNode,
    token: lexer.Token,
};

/// Represents all possible forms of patterns in pattern matching.
/// Patterns can match literals, destructure data types, bind variables,
/// or combine these behaviors.
pub const PatternNode = union(enum) {
    wildcard: struct {
        token: lexer.Token,
    },
    int_literal: IntLiteralNode,
    float_literal: FloatLiteralNode,
    char_literal: CharLiteralNode,
    string_literal: StrLiteralNode,
    list: struct {
        elements: std.ArrayList(*PatternNode),
        token: lexer.Token,
    },
    variable: struct {
        name: []const u8,
        token: lexer.Token,
    },
    constructor: struct {
        name: []const u8,
        args: std.ArrayList(*PatternNode),
        token: lexer.Token,
    },
    empty_list: struct {
        token: lexer.Token,
    },
    cons: struct {
        head: *PatternNode,
        tail: *PatternNode,
        token: lexer.Token,
    },
};

/// Represents a guard condition in a match case that must evaluate
/// to true for the pattern to match.
///
/// Examples:
/// - `when x > 0`
/// - `when is_valid? name`
pub const GuardNode = struct {
    condition: *Node,
    token: lexer.Token,
};

/// Represents a conditional expression with a condition, then branch, and else branch.
/// The condition must evaluate to a boolean value. If the condition is true,
/// the then branch is evaluated, otherwise the else branch is evaluated.
///
/// Examples:
/// - `if x > 0 then 1 else 0`
/// - `if empty? list then None else Some (head list)`
/// - `if even? n then n / 2 else 3 * n + 1`
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
    params: std.ArrayList([]const u8),
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

/// Represents a binary operation that constructs a list by prepending
/// an element to the front of a list.
///
/// Examples:
/// - `1 :: [2, 3]` evaluates to `[1, 2, 3]`
/// - `x :: xs` pattern matches head and tail of list
pub const ConsExprNode = struct {
    head: *Node,
    operator: lexer.Token,
    tail: *Node,
};

/// Represents a binary operation that concatenates two strings.
///
/// Examples:
/// - `"Hello" <> "World"` evaluates to `"HelloWorld"`
/// - `name <> "!"` concatenates a string variable with a literal
pub const StrConcatExprNode = BinaryOp;

/// Represents a binary operation that concatenates two lists.
///
/// Examples:
/// - `[1, 2] ++ [3, 4]` evaluates to `[1, 2, 3, 4]`
/// - `xs ++ ys` combines two list variables
/// - `[] ++ xs` concatenating with empty list
pub const ListConcatExprNode = BinaryOp;

/// Represents a binary operation that combines functions through composition.
///
/// Examples:
/// - `f >> g` applies g after f (forward composition)
/// - `f << g` applies f after g (backward composition)
pub const CompositionExprNode = struct {
    first: *Node,
    operator: lexer.Token,
    second: *Node,
};

/// Represents a binary operation that passes a value through a pipeline
/// of function applications.
///
/// Examples:
/// - `x |> f` applies f to x (forward pipe)
/// - `f <| x` applies f to x (backward pipe)
pub const PipeExprNode = struct {
    value: *Node,
    operator: lexer.Token,
    func: *Node,
};

/// Represents a method of exposing items from a module.
/// This can be everything (..), specific items, or nothing.
///
/// Examples:
/// - `exposing (..)`
/// - `exposing (func1, Type1, Type2(..))`
/// - `exposing ()`
pub const ExportSpecNode = struct {
    exposing_all: bool,
    items: ?std.ArrayList(ExportItem),
    token: lexer.Token,
};

pub const ExportItem = struct {
    name: []const u8,
    expose_constructors: bool,
    token: lexer.Token,
};

/// Represents a module path reference, which can be a single identifier
/// or a qualified path of dot-separated identifiers.
///
/// Examples:
/// - `MyModule`
/// - `MyModule.SubModule`
/// - `Std.List`
pub const ModulePathNode = struct {
    segments: std.ArrayList([]const u8),
    token: lexer.Token,
};

/// Represents a module declaration with its name, exports, and contents.
/// A module groups related functions, types, and values together
/// and controls their visibility.
///
/// Examples:
/// - `module MyModule exposing (..) ... end`
/// - `module Std.List exposing (map, filter) ... end`
/// - `module Parser exposing (Parser, run, map) ... end`
pub const ModuleDeclNode = struct {
    path: ModulePathNode,
    exports: ExportSpecNode,
    declarations: std.ArrayList(*Node),
    token: lexer.Token,
};

/// Represents an import specification that controls how a module is imported.
/// Can include renaming, selective imports, hiding, or using clauses.
///
/// Examples:
/// - `open MyModule`
/// - `open MyModule as M`
/// - `open MyModule using (map, filter)`
/// - `open Std.List renaming (map to list_map)`
/// - `open MyModule hiding (internal_func)`
pub const ImportSpecNode = struct {
    const ImportKind = enum {
        Simple,
        Alias,
        Using,
        Renaming,
        Hiding,
    };

    const RenameItem = struct {
        old_name: []const u8,
        new_name: []const u8,
        token: lexer.Token,
    };

    path: ModulePathNode,
    kind: ImportKind,
    alias: ?[]const u8,
    items: ?std.ArrayList(union(enum) {
        name: []const u8,
        rename: RenameItem,
    }),
    token: lexer.Token,
};

/// Represents a module include declaration that imports and re-exports
/// all contents from another module.
///
/// Examples:
/// - `include MyModule`
/// - `include Data.List`
/// - `include Parser.Internal`
pub const IncludeNode = struct {
    path: ModulePathNode,
    token: lexer.Token,
};

/// Represents a typed hole - a placeholder for a type that should be inferred.
pub const TypedHoleNode = struct {
    token: lexer.Token,
};

/// Represents a constructor for a variant type with an optional list of type parameters.
/// Each constructor can have zero or more type parameters.
///
/// Examples:
/// - `None` (no parameters)
/// - `Some a` (one type parameter)
/// - `Entry k v` (two type parameters)
/// - `Node a (Tree a)` (nested type parameters)
pub const VariantConstructorNode = struct {
    name: []const u8,
    params: std.ArrayList(*Node),
    token: lexer.Token,
};

/// Represents a variant type declaration with a list of constructors.
/// Each variant type can have zero or more type parameters that can be used
/// in its constructors.
///
/// Examples:
/// - `type Maybe a = None | Some a`
/// - `type List a = Nil | Cons a (List a)`
/// - `type Tree a = Leaf | Branch (Tree a) a (Tree a)`
/// - `type Result e a = Err e | Ok a`
pub const VariantTypeNode = struct {
    name: []const u8,
    type_params: std.ArrayList([]const u8),
    constructors: std.ArrayList(VariantConstructorNode),
    token: lexer.Token,
};

/// Represents a type alias declaration that creates a new name for an existing type.
/// Type aliases can be used to create more descriptive type names or to
/// simplify complex type signatures.
///
/// Examples:
/// - `type alias UserId = String`
/// - `type alias Reader r a = r -> a`
/// - `type alias Parser = String -> Maybe Expr`
pub const TypeAliasNode = struct {
    name: []const u8,
    value: *Node,
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
    char_literal: CharLiteralNode,
    float_literal: FloatLiteralNode,
    int_literal: IntLiteralNode,
    str_literal: StrLiteralNode,

    // Data Structures
    list: ListNode,

    // Expressions
    arithmetic_expr: ArithmeticExprNode,
    comparison_expr: ComparisonExprNode,
    composition_expr: CompositionExprNode,
    cons_expr: ConsExprNode,
    lambda_expr: LambdaExprNode,
    list_concat_expr: ListConcatExprNode,
    logical_expr: LogicalExprNode,
    match_expr: MatchExprNode,
    pipe_expr: PipeExprNode,
    str_concat_expr: StrConcatExprNode,
    unary_expr: UnaryExprNode,

    pattern: PatternNode,

    // Statements
    if_then_else_stmt: IfThenElseStmtNode,

    // Declarations
    function_decl: FunctionDeclNode,
    module_decl: ModuleDeclNode,

    export_spec: ExportSpecNode,
    import_spec: ImportSpecNode,
    include: IncludeNode,
    module_path: ModulePathNode,

    // Types
    function_type: FunctionTypeNode,
    type_alias: TypeAliasNode,
    typed_hole: TypedHoleNode,
    variant_type: VariantTypeNode,

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
            .str_literal => |lit| {
                allocator.free(lit.value);
            },
            .list => |*list| {
                for (list.elements.items) |element| {
                    element.deinit(allocator);
                    allocator.destroy(element);
                }

                list.elements.deinit();
            },
            .arithmetic_expr => |*expr| {
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
            .composition_expr => |*expr| {
                expr.first.deinit(allocator);
                expr.second.deinit(allocator);
                allocator.destroy(expr.first);
                allocator.destroy(expr.second);
            },
            .cons_expr => |*expr| {
                expr.head.deinit(allocator);
                expr.tail.deinit(allocator);
            },
            .lambda_expr => |*expr| {
                expr.params.deinit();
                expr.body.deinit(allocator);
                allocator.destroy(expr.body);
            },
            .list_concat_expr => |*expr| {
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
            .match_expr => |*expr| {
                expr.value.deinit(allocator);
                allocator.destroy(expr.value);

                for (expr.cases.items) |*case| {
                    deinitPattern(allocator, case.pattern);

                    allocator.destroy(case.pattern);

                    case.expression.deinit(allocator);
                    allocator.destroy(case.expression);

                    if (case.guard) |guard| {
                        guard.condition.deinit(allocator);
                        allocator.destroy(guard.condition);
                        allocator.destroy(guard);
                    }
                }

                expr.cases.deinit();
            },
            .pattern => |*pat| {
                deinitPattern(allocator, pat);
            },
            .pipe_expr => |*expr| {
                expr.value.deinit(allocator);
                expr.func.deinit(allocator);
                allocator.destroy(expr.value);
                allocator.destroy(expr.func);
            },
            .str_concat_expr => |*expr| {
                expr.left.deinit(allocator);
                expr.right.deinit(allocator);
                allocator.destroy(expr.left);
                allocator.destroy(expr.right);
            },
            .unary_expr => |*expr| {
                expr.operand.deinit(allocator);
                allocator.destroy(expr.operand);
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
            .module_decl => |*decl| {
                for (decl.path.segments.items) |segment| {
                    allocator.free(segment);
                }

                decl.path.segments.deinit();

                if (decl.exports.items) |*items| {
                    items.deinit();
                }

                for (decl.declarations.items) |declaration| {
                    declaration.deinit(allocator);
                    allocator.destroy(declaration);
                }

                decl.declarations.deinit();
            },
            .export_spec => |*spec| {
                if (spec.items) |*items| {
                    items.deinit();
                }
            },
            .import_spec => |*spec| {
                for (spec.path.segments.items) |segment| {
                    allocator.free(segment);
                }

                spec.path.segments.deinit();

                if (spec.alias) |alias| {
                    allocator.free(alias);
                }

                if (spec.items) |*items| {
                    for (items.items) |item| {
                        switch (item) {
                            .name => |name| {
                                allocator.free(name);
                            },
                            .rename => |rename| {
                                allocator.free(rename.old_name);
                                allocator.free(rename.new_name);
                            },
                        }
                    }

                    items.deinit();
                }
            },
            .include => |*inc| {
                for (inc.path.segments.items) |segment| {
                    allocator.free(segment);
                }

                inc.path.segments.deinit();
            },
            .module_path => |*path| {
                for (path.segments.items) |segment| {
                    allocator.free(segment);
                }

                path.segments.deinit();
            },
            .type_alias => |*alias| {
                allocator.free(alias.name);
                alias.value.deinit(allocator);
                allocator.destroy(alias.value);
            },
            .variant_type => |*vtype| {
                allocator.free(vtype.name);

                for (vtype.type_params.items) |param| {
                    allocator.free(param);
                }

                vtype.type_params.deinit();

                for (vtype.constructors.items) |*constructor| {
                    for (constructor.params.items) |param| {
                        param.deinit(allocator);
                        allocator.destroy(param);
                    }

                    constructor.params.deinit();
                }

                vtype.constructors.deinit();
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

    fn deinitPattern(allocator: std.mem.Allocator, pattern: *PatternNode) void {
        switch (pattern.*) {
            .wildcard,
            .int_literal,
            .float_literal,
            .char_literal,
            .empty_list,
            => {},
            .string_literal => |lit| {
                allocator.free(lit.value);
            },
            .variable => |variable| {
                allocator.free(variable.name);
            },
            .constructor => |*con| {
                allocator.free(con.name);

                for (con.args.items) |arg| {
                    deinitPattern(allocator, arg);
                    allocator.destroy(arg);
                }

                con.args.deinit();
            },
            .cons => |*cons| {
                deinitPattern(allocator, cons.head);
                deinitPattern(allocator, cons.tail);
                allocator.destroy(cons.head);
                allocator.destroy(cons.tail);
            },
            .list => |*list| {
                for (list.elements.items) |element| {
                    deinitPattern(allocator, element);
                    allocator.destroy(element);
                }

                list.elements.deinit();
            },
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
        .int_literal = .{
            .value = 42,
            .token = lexer.Token{
                .kind = .LitInt,
                .lexeme = "42",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 2 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    const right = try allocator.create(Node);
    right.* = .{
        .int_literal = .{
            .value = 24,
            .token = lexer.Token{
                .kind = .LitInt,
                .lexeme = "24",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 6, .end = 8 },
                    .src = .{ .line = 1, .col = 7 },
                },
            },
        },
    };

    var binary = try allocator.create(Node);
    defer {
        binary.deinit(allocator);
        allocator.destroy(binary);
    }

    binary.* = .{
        .arithmetic_expr = .{
            .left = left,
            .operator = lexer.Token{
                .kind = .OpIntAdd,
                .lexeme = "+",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 3, .end = 3 },
                    .src = .{ .line = 1, .col = 4 },
                },
            },
            .right = right,
        },
    };

    try testing.expectEqual(lexer.TokenKind.OpIntAdd, binary.arithmetic_expr.operator.kind);
    try testing.expect(binary.arithmetic_expr.left.int_literal.value == 42);
    try testing.expect(binary.arithmetic_expr.right.int_literal.value == 24);
}

test "UnaryExprNode memory management" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // -42

    const operand = try allocator.create(Node);
    operand.* = .{
        .int_literal = .{
            .value = 42,
            .token = lexer.Token{
                .kind = .LitInt,
                .lexeme = "42",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 1, .end = 3 },
                    .src = .{ .line = 1, .col = 2 },
                },
            },
        },
    };

    var unary = try allocator.create(Node);
    defer {
        unary.deinit(allocator);
        allocator.destroy(unary);
    }

    unary.* = .{
        .unary_expr = .{
            .operator = lexer.Token{
                .kind = .OpIntSub,
                .lexeme = "-",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
            .operand = operand,
        },
    };

    try testing.expectEqual(lexer.TokenKind.OpIntSub, unary.unary_expr.operator.kind);
    try testing.expectEqualStrings("-", unary.unary_expr.operator.lexeme);
    try testing.expectEqual(lexer.TokenKind.LitInt, unary.unary_expr.operand.int_literal.token.kind);
    try testing.expect(unary.unary_expr.operand.int_literal.value == 42);
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
                    .span = .{ .start = 7, .end = 8 },
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
                    .span = .{ .start = 10, .end = 11 },
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
                    .span = .{ .start = 8, .end = 9 },
                    .src = .{ .line = 1, .col = 9 },
                },
            },
            .right = right,
        },
    };

    const lambda = try allocator.create(Node);
    defer {
        lambda.deinit(allocator);
        allocator.destroy(lambda);
    }

    lambda.* = .{
        .lambda_expr = .{
            .params = params,
            .body = body,
            .token = lexer.Token{
                .kind = .OpLambda,
                .lexeme = "\\",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
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

    const lambda_body = lambda.lambda_expr.body.arithmetic_expr;
    try testing.expectEqual(lexer.TokenKind.OpIntAdd, lambda_body.operator.kind);
    try testing.expectEqualStrings("+", lambda_body.operator.lexeme);

    try testing.expect(lambda_body.left.* == .lower_identifier);
    try testing.expectEqualStrings("x", lambda_body.left.lower_identifier.name);
    try testing.expectEqual(lexer.TokenKind.LowerIdent, lambda_body.left.lower_identifier.token.kind);
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
                    .span = .{ .start = 3, .end = 4 },
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
                    .span = .{ .start = 8, .end = 9 },
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
                    .span = .{ .start = 5, .end = 7 },
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
                    .span = .{ .start = 15, .end = 19 },
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
                    .span = .{ .start = 25, .end = 30 },
                    .src = .{ .line = 1, .col = 26 },
                },
            },
        },
    };

    const if_then_else = try allocator.create(Node);
    defer {
        if_then_else.deinit(allocator);
        allocator.destroy(if_then_else);
    }

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
                    .span = .{ .start = 2, .end = 5 },
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
                    .span = .{ .start = 9, .end = 12 },
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
                    .span = .{ .start = 16, .end = 19 },
                    .src = .{ .line = 1, .col = 17 },
                },
            },
        },
    };

    try param_types.append(int_type1);
    try param_types.append(int_type2);
    try param_types.append(int_type3);

    var func_type = try allocator.create(Node);
    defer {
        func_type.deinit(allocator);
        allocator.destroy(func_type);
    }

    func_type.* = .{
        .function_type = .{
            .param_types = param_types,
            .token = lexer.Token{
                .kind = .DelColon,
                .lexeme = ":",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    // Test the structure
    try testing.expectEqual(lexer.TokenKind.DelColon, func_type.function_type.token.kind);
    try testing.expectEqualStrings(":", func_type.function_type.token.lexeme);
    try testing.expectEqual(@as(usize, 3), func_type.function_type.param_types.items.len);

    // Test each Int type
    for (func_type.function_type.param_types.items) |type_node| {
        try testing.expect(type_node.* == .upper_identifier);
        try testing.expectEqual(lexer.TokenKind.UpperIdent, type_node.upper_identifier.token.kind);
        try testing.expectEqualStrings("Int", type_node.upper_identifier.name);
        try testing.expectEqualStrings("Int", type_node.upper_identifier.token.lexeme);
    }

    // Test specific positions of each Int
    const first_int = func_type.function_type.param_types.items[0];
    try testing.expectEqual(@as(usize, 2), first_int.upper_identifier.token.loc.span.start);
    try testing.expectEqual(@as(usize, 3), first_int.upper_identifier.token.loc.src.col);

    const second_int = func_type.function_type.param_types.items[1];
    try testing.expectEqual(@as(usize, 9), second_int.upper_identifier.token.loc.span.start);
    try testing.expectEqual(@as(usize, 10), second_int.upper_identifier.token.loc.src.col);

    const third_int = func_type.function_type.param_types.items[2];
    try testing.expectEqual(@as(usize, 16), third_int.upper_identifier.token.loc.span.start);
    try testing.expectEqual(@as(usize, 17), third_int.upper_identifier.token.loc.src.col);
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
                    .span = .{ .start = 9, .end = 12 },
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
                    .span = .{ .start = 16, .end = 19 },
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
                    .span = .{ .start = 23, .end = 26 },
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
                    .span = .{ .start = 7, .end = 8 },
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
                    .span = .{ .start = 37, .end = 38 },
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
                    .span = .{ .start = 41, .end = 42 },
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
                    .span = .{ .start = 39, .end = 40 },
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
                    .span = .{ .start = 30, .end = 31 },
                    .src = .{ .line = 1, .col = 31 },
                },
            },
        },
    };

    var func_decl = try allocator.create(Node);
    defer {
        func_decl.deinit(allocator);
        allocator.destroy(func_decl);
    }

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
                    .span = .{ .start = 0, .end = 3 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    // Verify the structure
    try testing.expectEqual(lexer.TokenKind.KwLet, func_decl.function_decl.token.kind);
    try testing.expectEqualStrings("let", func_decl.function_decl.token.lexeme);
    try testing.expectEqualStrings("add", func_decl.function_decl.name);

    // Test type annotation
    const type_annot = func_decl.function_decl.type_annotation.?;
    try testing.expect(type_annot.* == .function_type);
    try testing.expectEqual(lexer.TokenKind.DelColon, type_annot.function_type.token.kind);
    try testing.expectEqual(@as(usize, 3), type_annot.function_type.param_types.items.len);

    // Test each Int in type signature
    for (type_annot.function_type.param_types.items) |type_node| {
        try testing.expect(type_node.* == .upper_identifier);
        try testing.expectEqual(lexer.TokenKind.UpperIdent, type_node.upper_identifier.token.kind);
        try testing.expectEqualStrings("Int", type_node.upper_identifier.name);
    }

    // Test lambda expression
    const lambda_value = func_decl.function_decl.value;
    try testing.expect(lambda_value.* == .lambda_expr);
    try testing.expectEqual(lexer.TokenKind.OpLambda, lambda_value.lambda_expr.token.kind);
    try testing.expectEqual(@as(usize, 2), lambda_value.lambda_expr.params.items.len);
    try testing.expectEqualStrings("x", lambda_value.lambda_expr.params.items[0]);
    try testing.expectEqualStrings("y", lambda_value.lambda_expr.params.items[1]);

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
}

test "ConsExprNode memory management" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // 1 :: [2, 3]

    const head = try allocator.create(Node);
    head.* = .{
        .int_literal = .{
            .value = 1,
            .token = lexer.Token{
                .kind = .LitInt,
                .lexeme = "1",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    var elements = std.ArrayList(*Node).init(allocator);

    const two = try allocator.create(Node);
    two.* = .{
        .int_literal = .{
            .value = 2,
            .token = lexer.Token{
                .kind = .LitInt,
                .lexeme = "2",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 6, .end = 7 },
                    .src = .{ .line = 1, .col = 7 },
                },
            },
        },
    };

    const three = try allocator.create(Node);
    three.* = .{
        .int_literal = .{
            .value = 3,
            .token = lexer.Token{
                .kind = .LitInt,
                .lexeme = "3",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 9, .end = 10 },
                    .src = .{ .line = 1, .col = 10 },
                },
            },
        },
    };

    try elements.append(two);
    try elements.append(three);

    const tail = try allocator.create(Node);
    tail.* = .{
        .list = .{
            .elements = elements,
            .token = lexer.Token{
                .kind = .DelLeftBracket,
                .lexeme = "[",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 5, .end = 6 },
                    .src = .{ .line = 1, .col = 6 },
                },
            },
        },
    };

    var cons = try allocator.create(Node);
    defer {
        cons.deinit(allocator);
        allocator.destroy(cons);
    }

    cons.* = .{
        .cons_expr = .{
            .head = head,
            .operator = lexer.Token{
                .kind = .OpCons,
                .lexeme = "::",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 2, .end = 3 },
                    .src = .{ .line = 1, .col = 3 },
                },
            },
            .tail = tail,
        },
    };

    // Verify the structure
    try testing.expect(cons.* == .cons_expr);
    try testing.expectEqual(lexer.TokenKind.OpCons, cons.cons_expr.operator.kind);
    try testing.expectEqualStrings("::", cons.cons_expr.operator.lexeme);

    // Test head (1)
    try testing.expect(cons.cons_expr.head.* == .int_literal);
    try testing.expectEqual(@as(i64, 1), cons.cons_expr.head.int_literal.value);

    // Test tail ([2, 3])
    try testing.expect(cons.cons_expr.tail.* == .list);
    try testing.expectEqual(@as(usize, 2), cons.cons_expr.tail.list.elements.items.len);

    // Test list elements
    const list_elements = cons.cons_expr.tail.list.elements.items;
    try testing.expect(list_elements[0].* == .int_literal);
    try testing.expectEqual(@as(i64, 2), list_elements[0].int_literal.value);
    try testing.expect(list_elements[1].* == .int_literal);
    try testing.expectEqual(@as(i64, 3), list_elements[1].int_literal.value);
}

test "StrConcatExprNode memory management" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // "Hello" <> "World"

    const left = try allocator.create(Node);
    const left_str = try allocator.dupe(u8, "Hello");
    left.* = .{
        .str_literal = .{
            .value = left_str,
            .token = lexer.Token{
                .kind = .LitString,
                .lexeme = "\"Hello\"",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 7 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    const right = try allocator.create(Node);
    const right_str = try allocator.dupe(u8, "World");
    right.* = .{
        .str_literal = .{
            .value = right_str,
            .token = lexer.Token{
                .kind = .LitString,
                .lexeme = "\"World\"",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 11, .end = 18 },
                    .src = .{ .line = 1, .col = 12 },
                },
            },
        },
    };

    var concat = try allocator.create(Node);
    defer {
        concat.deinit(allocator);
        allocator.destroy(concat);
    }

    concat.* = .{
        .str_concat_expr = .{
            .left = left,
            .operator = lexer.Token{
                .kind = .OpStrConcat,
                .lexeme = "<>",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 8, .end = 10 },
                    .src = .{ .line = 1, .col = 9 },
                },
            },
            .right = right,
        },
    };

    // Verify the structure
    try testing.expectEqual(lexer.TokenKind.OpStrConcat, concat.str_concat_expr.operator.kind);
    try testing.expectEqualStrings("<>", concat.str_concat_expr.operator.lexeme);
    try testing.expectEqualStrings("Hello", concat.str_concat_expr.left.str_literal.value);
    try testing.expectEqualStrings("World", concat.str_concat_expr.right.str_literal.value);
}

test "ListConcatExprNode memory management" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // [1, 2] ++ [3, 4]

    const left = try allocator.create(Node);
    var left_elements = std.ArrayList(*Node).init(allocator);

    const left_elem1 = try allocator.create(Node);
    left_elem1.* = .{
        .int_literal = .{
            .value = 1,
            .token = lexer.Token{
                .kind = .LitInt,
                .lexeme = "1",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 1, .end = 2 },
                    .src = .{ .line = 1, .col = 2 },
                },
            },
        },
    };

    const left_elem2 = try allocator.create(Node);
    left_elem2.* = .{
        .int_literal = .{
            .value = 2,
            .token = lexer.Token{
                .kind = .LitInt,
                .lexeme = "2",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 4, .end = 5 },
                    .src = .{ .line = 1, .col = 5 },
                },
            },
        },
    };

    try left_elements.append(left_elem1);
    try left_elements.append(left_elem2);

    left.* = .{
        .list = .{
            .elements = left_elements,
            .token = lexer.Token{
                .kind = .DelLeftBracket,
                .lexeme = "[",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    const right = try allocator.create(Node);
    var right_elements = std.ArrayList(*Node).init(allocator);

    const right_elem1 = try allocator.create(Node);
    right_elem1.* = .{
        .int_literal = .{
            .value = 3,
            .token = lexer.Token{
                .kind = .LitInt,
                .lexeme = "3",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 11, .end = 12 },
                    .src = .{ .line = 1, .col = 12 },
                },
            },
        },
    };

    const right_elem2 = try allocator.create(Node);
    right_elem2.* = .{
        .int_literal = .{
            .value = 4,
            .token = lexer.Token{
                .kind = .LitInt,
                .lexeme = "4",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 14, .end = 15 },
                    .src = .{ .line = 1, .col = 15 },
                },
            },
        },
    };

    try right_elements.append(right_elem1);
    try right_elements.append(right_elem2);

    right.* = .{
        .list = .{
            .elements = right_elements,
            .token = lexer.Token{
                .kind = .DelLeftBracket,
                .lexeme = "[",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 10, .end = 11 },
                    .src = .{ .line = 1, .col = 11 },
                },
            },
        },
    };

    var concat = try allocator.create(Node);
    defer {
        concat.deinit(allocator);
        allocator.destroy(concat);
    }

    concat.* = .{
        .list_concat_expr = .{
            .left = left,
            .operator = lexer.Token{
                .kind = .OpListConcat,
                .lexeme = "++",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 7, .end = 9 },
                    .src = .{ .line = 1, .col = 8 },
                },
            },
            .right = right,
        },
    };

    const expr = concat.list_concat_expr;

    // Verify the structure
    try testing.expectEqual(lexer.TokenKind.OpListConcat, expr.operator.kind);
    try testing.expectEqualStrings("++", expr.operator.lexeme);

    // Verify left list [1, 2]
    try testing.expect(expr.left.* == .list);
    try testing.expectEqual(@as(usize, 2), expr.left.list.elements.items.len);
    try testing.expect(expr.left.list.elements.items[0].* == .int_literal);
    try testing.expectEqual(@as(i64, 1), expr.left.list.elements.items[0].int_literal.value);
    try testing.expect(expr.left.list.elements.items[1].* == .int_literal);
    try testing.expectEqual(@as(i64, 2), expr.left.list.elements.items[1].int_literal.value);

    // Verify right list [3, 4]
    try testing.expect(expr.right.* == .list);
    try testing.expectEqual(@as(usize, 2), expr.right.list.elements.items.len);
    try testing.expect(expr.right.list.elements.items[0].* == .int_literal);
    try testing.expectEqual(@as(i64, 3), expr.right.list.elements.items[0].int_literal.value);
    try testing.expect(expr.right.list.elements.items[1].* == .int_literal);
    try testing.expectEqual(@as(i64, 4), expr.right.list.elements.items[1].int_literal.value);
}

test "CompositionExprNode memory management" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // f >> g (forward composition)

    const first = try allocator.create(Node);
    first.* = .{
        .lower_identifier = .{
            .name = "f",
            .token = lexer.Token{
                .kind = .LowerIdent,
                .lexeme = "f",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    const second = try allocator.create(Node);
    second.* = .{
        .lower_identifier = .{
            .name = "g",
            .token = lexer.Token{
                .kind = .LowerIdent,
                .lexeme = "g",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 5, .end = 6 },
                    .src = .{ .line = 1, .col = 6 },
                },
            },
        },
    };

    var compose = try allocator.create(Node);
    defer {
        compose.deinit(allocator);
        allocator.destroy(compose);
    }

    compose.* = .{
        .composition_expr = .{
            .first = first,
            .operator = lexer.Token{
                .kind = .OpComposeRight,
                .lexeme = ">>",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 2, .end = 4 },
                    .src = .{ .line = 1, .col = 3 },
                },
            },
            .second = second,
        },
    };

    const expr = compose.composition_expr;

    // Verify the structure
    try testing.expectEqual(lexer.TokenKind.OpComposeRight, expr.operator.kind);
    try testing.expectEqualStrings(">>", expr.operator.lexeme);

    // Verify first function (f)
    try testing.expect(expr.first.* == .lower_identifier);
    try testing.expectEqual(lexer.TokenKind.LowerIdent, expr.first.lower_identifier.token.kind);
    try testing.expectEqualStrings("f", expr.first.lower_identifier.name);

    // Verify second function (g)
    try testing.expect(expr.second.* == .lower_identifier);
    try testing.expectEqual(lexer.TokenKind.LowerIdent, expr.second.lower_identifier.token.kind);
    try testing.expectEqualStrings("g", expr.second.lower_identifier.name);
}

test "PipeExprNode memory management" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // x |> f (forward pipe)

    const value = try allocator.create(Node);
    value.* = .{
        .lower_identifier = .{
            .name = "x",
            .token = lexer.Token{
                .kind = .LowerIdent,
                .lexeme = "x",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    const func = try allocator.create(Node);
    func.* = .{
        .lower_identifier = .{
            .name = "f",
            .token = lexer.Token{
                .kind = .LowerIdent,
                .lexeme = "f",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 5, .end = 6 },
                    .src = .{ .line = 1, .col = 6 },
                },
            },
        },
    };

    var pipe = try allocator.create(Node);
    defer {
        pipe.deinit(allocator);
        allocator.destroy(pipe);
    }

    pipe.* = .{
        .pipe_expr = .{
            .value = value,
            .operator = lexer.Token{
                .kind = .OpPipeRight,
                .lexeme = "|>",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 2, .end = 4 },
                    .src = .{ .line = 1, .col = 3 },
                },
            },
            .func = func,
        },
    };

    const expr = pipe.pipe_expr;

    // Verify the structure
    try testing.expectEqual(lexer.TokenKind.OpPipeRight, expr.operator.kind);
    try testing.expectEqualStrings("|>", expr.operator.lexeme);

    // Verify value being piped (x)
    try testing.expect(expr.value.* == .lower_identifier);
    try testing.expectEqual(lexer.TokenKind.LowerIdent, expr.value.lower_identifier.token.kind);
    try testing.expectEqualStrings("x", expr.value.lower_identifier.name);

    // Verify function (f)
    try testing.expect(expr.func.* == .lower_identifier);
    try testing.expectEqual(lexer.TokenKind.LowerIdent, expr.func.lower_identifier.token.kind);
    try testing.expectEqualStrings("f", expr.func.lower_identifier.name);
}
