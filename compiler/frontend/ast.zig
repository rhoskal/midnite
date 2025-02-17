const std = @import("std");

const lexer = @import("lexer.zig");

//==========================================================================
// Basic Literals
//==========================================================================

/// Represents a regular, single line comment.
///
/// Examples:
/// - `# This is a comment`
pub const CommentNode = struct {
    /// The content of the comment, excluding the comment marker.
    content: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        content: []const u8,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*CommentNode {
        const node = try allocator.create(CommentNode);

        node.* = .{
            .content = try allocator.dupe(u8, params.content),
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *CommentNode, allocator: std.mem.Allocator) void {
        allocator.free(self.content);

        allocator.destroy(self);
    }
};

/// Represents a documentation comment that will be processed as markdown.
///
/// Examples:
/// - `## This is a doc comment`
pub const DocCommentNode = struct {
    /// The content of the documentation comment, excluding the comment marker.
    content: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        content: []const u8,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*DocCommentNode {
        const node = try allocator.create(DocCommentNode);

        node.* = .{
            .content = try allocator.dupe(u8, params.content),
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *DocCommentNode, allocator: std.mem.Allocator) void {
        allocator.free(self.content);

        allocator.destroy(self);
    }
};

/// Represents a literal integer value.
///
/// Examples:
/// - `42`
/// - `-17`
pub const IntLiteralNode = struct {
    /// The parsed integer value.
    value: i64,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        value: i64,
        token: lexer.Token,
    };

    pub fn init(params: InitParams) IntLiteralNode {
        return .{
            .value = params.value,
            .token = params.token,
        };
    }
};

/// Represents a literal floating-point value.
///
/// Examples:
/// - `3.14`
/// - `-0.001`
/// - `1.0e10`
pub const FloatLiteralNode = struct {
    /// The parsed floating-point value.
    value: f64,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        value: f64,
        token: lexer.Token,
    };

    pub fn init(params: InitParams) FloatLiteralNode {
        return .{
            .value = params.value,
            .token = params.token,
        };
    }
};

/// Represents a char literal value.
///
/// Examples:
/// - `'a'`
/// - `'\n'`
/// - `'\u{1f600}'`
/// - `'😀'`
pub const CharLiteralNode = struct {
    /// The Unicode codepoint value of the character.
    value: u21,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        value: u21,
        token: lexer.Token,
    };

    pub fn init(params: InitParams) CharLiteralNode {
        return .{
            .value = params.value,
            .token = params.token,
        };
    }
};

/// Represents a string literal value.
///
/// Examples:
/// - `"hello"`
/// - `"line\nbreak"`
pub const StrLiteralNode = struct {
    /// The parsed string value with escape sequences processed.
    value: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        value: []const u8,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*StrLiteralNode {
        const node = try allocator.create(StrLiteralNode);

        node.* = .{
            .value = try allocator.dupe(u8, params.value),
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *StrLiteralNode, allocator: std.mem.Allocator) void {
        allocator.free(self.value);

        allocator.destroy(self);
    }
};

/// Represents a multiline string literal value.
///
/// Examples:
/// - `"""
/// first line
/// second line
/// """`
pub const MultilineStrLiteralNode = struct {
    /// The parsed multiline string value with escape sequences processed.
    value: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        value: []const u8,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*MultilineStrLiteralNode {
        const node = try allocator.create(MultilineStrLiteralNode);

        node.* = .{
            .value = try allocator.dupe(u8, params.value),
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *MultilineStrLiteralNode, allocator: std.mem.Allocator) void {
        allocator.free(self.value);

        allocator.destroy(self);
    }
};

//==========================================================================
// Identifiers
//==========================================================================

/// Represents a lowercase identifier reference (variable names, function names, etc).
pub const LowerIdentifierNode = struct {
    /// The text of the identifier.
    name: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        name: []const u8,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*LowerIdentifierNode {
        const node = try allocator.create(LowerIdentifierNode);

        node.* = .{
            .name = try allocator.dupe(u8, params.name),
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *LowerIdentifierNode, allocator: std.mem.Allocator) void {
        allocator.free(self.name);

        allocator.destroy(self);
    }
};

/// Represents an uppercase identifier reference (type names, type constructors, etc).
pub const UpperIdentifierNode = struct {
    /// The text of the identifier.
    name: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        name: []const u8,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*UpperIdentifierNode {
        const node = try allocator.create(UpperIdentifierNode);

        node.* = .{
            .name = try allocator.dupe(u8, params.name),
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *UpperIdentifierNode, allocator: std.mem.Allocator) void {
        allocator.free(self.name);

        allocator.destroy(self);
    }
};

//==========================================================================
// Basic Data Structures
//==========================================================================

/// Represents a list of comma-separated expressions surrounded by square brackets.
///
/// Examples:
/// - `[]`
/// - `[1, 2, 3]`
/// - `[True, False, True]`
pub const ListNode = struct {
    /// Array of pointers to the AST nodes representing list elements.
    elements: std.ArrayList(*Node),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        elements: std.ArrayList(*Node),
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*ListNode {
        const node = try allocator.create(ListNode);

        node.* = .{
            .elements = params.elements,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *ListNode, allocator: std.mem.Allocator) void {
        for (self.elements.items) |element| {
            element.deinit(allocator);
            allocator.destroy(element);
        }

        self.elements.deinit();

        allocator.destroy(self);
    }
};

/// Represents a tuple of elements enclosed in parentheses.
/// Tuples are strictly pairs (two elements).
///
/// Examples:
/// - `(1, 2)`
/// - `(True, "hello")`
/// - `(x, y)`
pub const TupleNode = struct {
    /// Array of pointers to the AST nodes representing tuple elements.
    elements: std.ArrayList(*Node),

    /// The token representing the start of this tuple.
    token: lexer.Token,

    pub const InitParams = struct {
        elements: std.ArrayList(*Node),
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*TupleNode {
        const node = try allocator.create(TupleNode);

        node.* = .{
            .elements = params.elements,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *TupleNode, allocator: std.mem.Allocator) void {
        for (self.elements.items) |element| {
            element.deinit(allocator);
            allocator.destroy(element);
        }

        self.elements.deinit();

        allocator.destroy(self);
    }
};

//==========================================================================
// Basic Expressions
//==========================================================================

/// A unary operation node representing operations with only one operand.
///
/// Examples:
/// - Negation: (-)
pub const UnaryExprNode = struct {
    /// The AST node representing the operand.
    operand: *Node,

    /// The token representing the operator.
    operator: lexer.Token,

    pub const InitParams = struct {
        operand: *Node,
        operator: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*UnaryExprNode {
        const node = try allocator.create(UnaryExprNode);

        node.* = .{
            .operand = params.operand,
            .operator = params.operator,
        };

        return node;
    }

    pub fn deinit(self: *UnaryExprNode, allocator: std.mem.Allocator) void {
        self.operand.deinit(allocator);
        allocator.destroy(self.operand);

        allocator.destroy(self);
    }
};

pub const BinaryOp = struct {
    /// The AST node for the left operand.
    left: *Node,

    /// The AST node for the right operand.
    right: *Node,

    /// The token representing the operator.
    operator: lexer.Token,

    pub const InitParams = struct {
        left: *Node,
        right: *Node,
        operator: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*BinaryOp {
        const node = try allocator.create(BinaryOp);

        node.* = .{
            .left = params.left,
            .right = params.right,
            .operator = params.operator,
        };

        return node;
    }

    pub fn deinit(self: *BinaryOp, allocator: std.mem.Allocator) void {
        self.left.deinit(allocator);
        allocator.destroy(self.left);

        self.right.deinit(allocator);
        allocator.destroy(self.right);

        allocator.destroy(self);
    }
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

//==========================================================================
// Pattern Matching
//==========================================================================

/// Pattern matching constructs used in match expressions.
/// Represents different kinds of patterns like literals, variables,
/// constructors, list patterns, and wildcards.
///
/// Examples:
/// - `_` matches anything
/// - `42` matches literal integer
/// - `Some x` matches constructor with variable
/// - `head :: tail` matches list with head and tail
/// - `[]` matches empty list
pub const PatternNode = union(enum) {
    wildcard: struct {
        token: lexer.Token,
    },
    int_literal: IntLiteralNode,
    float_literal: FloatLiteralNode,
    char_literal: CharLiteralNode,
    string_literal: StrLiteralNode,
    list: struct {
        /// Array of pattern nodes for matching against list elements.
        elements: std.ArrayList(*PatternNode),

        /// The token representing the start of the list pattern
        token: lexer.Token,

        pub fn init(
            allocator: std.mem.Allocator,
            elements: std.ArrayList(*PatternNode),
            token: lexer.Token,
        ) !*PatternNode.list {
            const pattern = try allocator.create(PatternNode.list);

            pattern.* = .{
                .elements = elements,
                .token = token,
            };

            return pattern;
        }
    },
    variable: struct {
        /// The name of the variable to bind.
        name: []const u8,

        /// The token representing the variable.
        token: lexer.Token,

        pub fn init(
            allocator: std.mem.Allocator,
            name: []const u8,
            token: lexer.Token,
        ) !*PatternNode.variable {
            const pattern = try allocator.create(PatternNode.variable);

            pattern.* = .{
                .name = try allocator.dupe(u8, name),
                .token = token,
            };

            return pattern;
        }
    },
    constructor: struct {
        /// The name of the constructor.
        name: []const u8,

        /// Array of patterns for matching constructor arguments.
        args: std.ArrayList(*PatternNode),

        /// The token representing the constructor.
        token: lexer.Token,

        pub fn init(
            allocator: std.mem.Allocator,
            name: []const u8,
            args: std.ArrayList(*PatternNode),
            token: lexer.Token,
        ) !*PatternNode.constructor {
            const pattern = try allocator.create(PatternNode.constructor);

            pattern.* = .{
                .name = try allocator.dupe(u8, name),
                .args = args,
                .token = token,
            };

            return pattern;
        }
    },
    empty_list: struct {
        /// The token representing the empty list pattern.
        token: lexer.Token,
    },
    cons: struct {
        /// Pattern for matching the head element.
        head: *PatternNode,

        /// Pattern for matching the tail list.
        tail: *PatternNode,

        /// The token representing the cons pattern.
        token: lexer.Token,

        pub fn init(
            allocator: std.mem.Allocator,
            head: *PatternNode,
            tail: *PatternNode,
            token: lexer.Token,
        ) !*PatternNode.cons {
            const pattern = try allocator.create(PatternNode.cons);

            pattern.* = .{
                .head = head,
                .tail = tail,
                .token = token,
            };

            return pattern;
        }
    },

    pub fn deinit(self: *PatternNode, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .wildcard,
            .int_literal,
            .float_literal,
            .char_literal,
            .empty_list,
            => {},
            .string_literal => |*lit| {
                lit.deinit(allocator);
                allocator.destroy(lit);
            },
            .list => |list| {
                for (list.elements.items) |element| {
                    element.deinit(allocator);
                    allocator.destroy(element);
                }

                list.elements.deinit();
            },
            .variable => |variable| {
                allocator.free(variable.name);
            },
            .constructor => |constructor| {
                allocator.free(constructor.name);

                for (constructor.args.items) |arg| {
                    arg.deinit(allocator);
                    allocator.destroy(arg);
                }

                constructor.args.deinit();
            },
            .cons => |cons| {
                cons.head.deinit(allocator);
                allocator.destroy(cons.head);

                cons.tail.deinit(allocator);
                allocator.destroy(cons.tail);
            },
        }

        allocator.destroy(self);
    }
};

/// Represents a guard condition in a match case that must evaluate
/// to true for the pattern to match.
///
/// Examples:
/// - `when x > 0`
/// - `when is_valid? name`
pub const GuardNode = struct {
    /// The AST node representing the boolean condition.
    condition: *Node,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        condition: *Node,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*GuardNode {
        const node = try allocator.create(GuardNode);

        node.* = .{
            .condition = params.condition,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *GuardNode, allocator: std.mem.Allocator) void {
        self.condition.deinit(allocator);
        allocator.destroy(self.condition);

        allocator.destroy(self);
    }
};

/// A single pattern matching case with an optional guard condition.
/// When pattern matches and guard evaluates true, the expression is evaluated.
///
/// Examples:
/// - `0 => "zero"`
/// - `x :: xs when length xs > 0 => count + 1`
pub const MatchCase = struct {
    /// The pattern to match against.
    pattern: *PatternNode,

    /// The expression to evaluate if pattern matches.
    expression: *Node,

    /// Optional guard condition that must be true for match to succeed.
    guard: ?*GuardNode,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        pattern: *PatternNode,
        expression: *Node,
        guard: ?*GuardNode,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*MatchCase {
        const case = try allocator.create(MatchCase);

        case.* = .{
            .pattern = params.pattern,
            .expression = params.expression,
            .guard = params.guard,
            .token = params.token,
        };

        return case;
    }

    pub fn deinit(self: *MatchCase, allocator: std.mem.Allocator) void {
        self.pattern.deinit(allocator);
        allocator.destroy(self.pattern);

        self.expression.deinit(allocator);
        allocator.destroy(self.expression);

        if (self.guard) |guard| {
            guard.deinit(allocator);
            allocator.destroy(guard);
        }

        allocator.destroy(self);
    }
};

/// Represents a complete match expression that tests a value against multiple patterns.
/// Each pattern is paired with an expression to evaluate when that pattern matches.
///
/// Examples:
/// - `match x on | 0 => "zero" | _ => "other"`
/// - `match list on | [] => 0 | x :: xs => 1 + length xs`
/// - `match result on | Ok v => v | Err e => default`
pub const MatchExprNode = struct {
    /// The AST node representing the value being matched.
    value: *Node,

    /// Array of match cases to test against.
    cases: std.ArrayList(MatchCase),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        value: *Node,
        cases: std.ArrayList(MatchCase),
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*MatchExprNode {
        const node = try allocator.create(MatchExprNode);

        node.* = .{
            .value = params.value,
            .cases = params.cases,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *MatchExprNode, allocator: std.mem.Allocator) void {
        self.value.deinit(allocator);
        allocator.destroy(self.value);

        for (self.cases.items) |*case| {
            case.deinit(allocator);
            allocator.destroy(case);
        }

        self.cases.deinit();

        allocator.destroy(self);
    }
};

//==========================================================================
// Functions and Applications
//==========================================================================

/// Represents a function type annotation.
///
/// Examples:
/// - `Int -> Int -> Int`
pub const FunctionTypeNode = struct {
    /// Array of AST nodes representing parameter types, with last being return type.
    param_types: std.ArrayList(*Node),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        param_types: std.ArrayList(*Node),
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*FunctionTypeNode {
        const node = try allocator.create(FunctionTypeNode);

        node.* = .{
            .param_types = params.param_types,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *FunctionTypeNode, allocator: std.mem.Allocator) void {
        for (self.param_types.items) |t| {
            t.deinit(allocator);
            allocator.destroy(t);
        }

        self.param_types.deinit();

        allocator.destroy(self);
    }
};

/// Represents a lambda expression.
///
/// Examples:
/// - `\x y => x + y`
pub const LambdaExprNode = struct {
    /// Array of parameter names.
    parameters: std.ArrayList([]const u8),

    /// The AST node representing the function body expression.
    body: *Node,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        parameters: std.ArrayList([]const u8),
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*LambdaExprNode {
        const node = try allocator.create(LambdaExprNode);

        node.* = .{
            .parameters = params.parameters,
            .body = params.body,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *LambdaExprNode, allocator: std.mem.Allocator) void {
        for (self.params.items) |param| {
            allocator.free(param);
        }

        self.params.deinit();

        self.body.deinit(allocator);
        allocator.destroy(self.body);

        allocator.destroy(self);
    }
};

/// Represents a function application where a function is applied to an argument.
///
/// Examples:
/// - `f 42`
/// - `not (and x y)`
pub const FuncApplicationNode = struct {
    /// The AST node representing the function being applied.
    function: *Node,

    /// The AST node representing the argument the function is being applied to.
    argument: *Node,

    /// The token representing the start of this application (usually the function's token).
    token: lexer.Token,

    pub const InitParams = struct {
        function: *Node,
        argument: *Node,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*FuncApplicationNode {
        const node = try allocator.create(FuncApplicationNode);

        node.* = .{
            .function = params.function,
            .argument = params.argument,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *FuncApplicationNode, allocator: std.mem.Allocator) void {
        self.function.deinit(allocator);
        allocator.destroy(self.function);

        self.argument.deinit(allocator);
        allocator.destroy(self.argument);

        allocator.destroy(self);
    }
};

//==========================================================================
// Advanced Expressions
//==========================================================================

/// Represents a binary operation that constructs a list by prepending
/// an element to the front of a list.
///
/// Examples:
/// - `1 :: [2, 3]`
/// - `x :: xs`
pub const ConsExprNode = struct {
    /// The AST node representing the element to prepend.
    head: *Node,

    /// The AST node representing the target list.
    tail: *Node,

    /// The token representing the cons operator.
    operator: lexer.Token,

    pub const InitParams = struct {
        head: *Node,
        tail: *Node,
        operator: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*ConsExprNode {
        const node = try allocator.create(ConsExprNode);

        node.* = .{
            .head = params.head,
            .tail = params.tail,
            .operator = params.operator,
        };

        return node;
    }

    pub fn deinit(self: *ConsExprNode, allocator: std.mem.Allocator) void {
        self.head.deinit(allocator);
        allocator.destroy(self.head);

        self.tail.deinit(allocator);
        allocator.destroy(self.tail);

        allocator.destroy(self);
    }
};

/// Represents a binary operation that concatenates two strings.
///
/// Examples:
/// - `"Hello" <> "World"`
/// - `name <> "!"`
pub const StrConcatExprNode = BinaryOp;

/// Represents a binary operation that concatenates two lists.
///
/// Examples:
/// - `[1, 2] ++ [3, 4]`
/// - `xs ++ ys`
/// - `[] ++ xs`
pub const ListConcatExprNode = BinaryOp;

/// Represents a binary operation that combines functions through composition.
///
/// Examples:
/// - `f >> g` (forward composition)
/// - `f << g` (backward composition)
pub const CompositionExprNode = struct {
    /// The AST node representing the first function in the composition.
    first: *Node,

    /// The AST node representing the second function in the composition.
    second: *Node,

    /// The token representing the composition operator.
    operator: lexer.Token,

    pub const InitParams = struct {
        first: *Node,
        second: *Node,
        operator: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*CompositionExprNode {
        const node = try allocator.create(CompositionExprNode);

        node.* = .{
            .first = params.first,
            .second = params.second,
            .operator = params.operator,
        };

        return node;
    }

    pub fn deinit(self: *CompositionExprNode, allocator: std.mem.Allocator) void {
        self.first.deinit(allocator);
        allocator.destroy(self.first);

        self.second.deinit(allocator);
        allocator.destroy(self.second);

        allocator.destroy(self);
    }
};

/// Represents a binary operation that passes a value through a pipeline
/// of function applications.
///
/// Examples:
/// - `x |> f` (forward pipe)
/// - `f <| x` (backward pipe)
pub const PipeExprNode = struct {
    /// The AST node representing the value being piped.
    value: *Node,

    /// The AST node representing the function receiving the piped value.
    func: *Node,

    /// The token representing the pipe operator.
    operator: lexer.Token,

    pub const InitParams = struct {
        value: *Node,
        func: *Node,
        operator: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*PipeExprNode {
        const node = try allocator.create(PipeExprNode);

        node.* = .{
            .value = params.value,
            .func = params.func,
            .operator = params.operator,
        };

        return node;
    }

    pub fn deinit(self: *PipeExprNode, allocator: std.mem.Allocator) void {
        self.value.deinit(allocator);
        allocator.destroy(self.value);

        self.func.deinit(allocator);
        allocator.destroy(self.func);

        allocator.destroy(self);
    }
};

//==========================================================================
// Control Flow
//==========================================================================

/// Represents a conditional expression with a condition, then branch, and else branch.
/// The condition must evaluate to a boolean value. If the condition is true,
/// the then branch is evaluated, otherwise the else branch is evaluated.
///
/// Examples:
/// - `if x > 0 then 1 else 0`
/// - `if empty? list then None else Some (head list)`
/// - `if even? n then n / 2 else 3 * n + 1`
pub const IfThenElseStmtNode = struct {
    /// The AST node representing the boolean condition.
    condition: *Node,

    /// The AST node representing the expression to evaluate if condition is true.
    then_branch: *Node,

    /// The AST node representing the expression to evaluate if condition is false.
    else_branch: *Node,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        condition: *Node,
        then_branch: *Node,
        else_branch: *Node,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*IfThenElseStmtNode {
        const node = try allocator.create(IfThenElseStmtNode);

        node.* = .{
            .condition = params.condition,
            .then_branch = params.then_branch,
            .else_branch = params.else_branch,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *IfThenElseStmtNode, allocator: std.mem.Allocator) void {
        self.condition.deinit(allocator);
        allocator.destroy(self.condition);

        self.then_branch.deinit(allocator);
        allocator.destroy(self.then_branch);

        self.else_branch.deinit(allocator);
        allocator.destroy(self.else_branch);

        allocator.destroy(self);
    }
};

//==========================================================================
// Type System
//==========================================================================

/// Represents a typed hole - a placeholder in code where type inference
/// should determine the appropriate type. Useful during development
/// and for type-driven development.
///
/// Examples:
/// - `?`
/// - `let x : ? = expr`
pub const TypedHoleNode = struct {
    /// The token representing the start of this declaration.
    token: lexer.Token,
};

/// Represents an application of type arguments to a type constructor.
/// For example: in `Map k v`, Map is the base type being applied to args k and v.
///
/// Examples:
/// - `List a`
/// - `Map k v`
/// - `Result e a`
/// - `Tree (Maybe a)`
pub const TypeApplicationNode = struct {
    /// The type constructor being applied (e.g., Map, List, Maybe).
    base: *Node,

    /// The type arguments being applied.
    args: std.ArrayList(*Node),

    /// The token representing the type application.
    token: lexer.Token,

    pub const InitParams = struct {
        base: *Node,
        args: std.ArrayList(*Node),
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*TypeApplicationNode {
        const node = try allocator.create(TypeApplicationNode);

        node.* = .{
            .base = params.base,
            .args = params.args,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *TypeApplicationNode, allocator: std.mem.Allocator) void {
        self.base.deinit(allocator);
        allocator.destroy(self.base);

        for (self.args.items) |arg| {
            arg.deinit(allocator);
            allocator.destroy(arg);
        }

        self.args.deinit();

        allocator.destroy(self);
    }
};

/// Represents a type alias declaration that creates a new name for an existing type.
/// Type aliases can be used to create more descriptive type names or to
/// simplify complex type signatures.
///
/// Examples:
/// - `type alias UserId = String`
/// - `type alias Dict k v = Map k v`
/// - `type alias Reducer a b = a -> b -> b`
/// - `type alias TreeMap k v = Tree (Pair k v) (Compare k)`
pub const TypeAliasNode = struct {
    /// The name of the type alias.
    name: []const u8,

    /// Array of type parameter names.
    type_params: std.ArrayList([]const u8),

    /// The AST node representing the aliased type.
    value: *Node,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        name: []const u8,
        type_params: std.ArrayList([]const u8),
        value: *Node,
        token: lexer.Token,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        name: []const u8,
        type_params: std.ArrayList([]const u8),
        value: *Node,
        token: lexer.Token,
    ) !*TypeAliasNode {
        const node = try allocator.create(TypeAliasNode);

        node.* = .{
            .name = try allocator.dupe(u8, name),
            .type_params = type_params,
            .value = value,
            .token = token,
        };

        return node;
    }

    pub fn deinit(self: *TypeAliasNode, allocator: std.mem.Allocator) void {
        allocator.free(self.name);

        for (self.type_params.items) |param| {
            allocator.free(param);
        }

        self.type_params.deinit();

        self.value.deinit(allocator);
        allocator.destroy(self.value);

        allocator.destroy(self);
    }
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
    /// The name of the constructor.
    name: []const u8,

    /// Array of AST nodes representing the constructor's parameter types.
    params: std.ArrayList(*Node),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        name: []const u8,
        parameters: std.ArrayList(*Node),
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*VariantConstructorNode {
        const node = try allocator.create(VariantConstructorNode);

        node.* = .{
            .name = try allocator.dupe(u8, params.name),
            .params = params.parameters,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *VariantConstructorNode, allocator: std.mem.Allocator) void {
        allocator.free(self.name);

        for (self.params.items) |param| {
            param.deinit(allocator);
            allocator.destroy(param);
        }

        self.params.deinit();

        allocator.destroy(self);
    }
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
    /// The name of the variant type.
    name: []const u8,

    /// Array of type parameter names.
    type_params: std.ArrayList([]const u8),

    /// Array of constructors defined for this type.
    constructors: std.ArrayList(VariantConstructorNode),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        name: []const u8,
        type_params: std.ArrayList([]const u8),
        constructors: std.ArrayList(VariantConstructorNode),
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*VariantTypeNode {
        const node = try allocator.create(VariantTypeNode);

        node.* = .{
            .name = try allocator.dupe(u8, params.name),
            .type_params = params.type_params,
            .constructors = params.constructors,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *VariantTypeNode, allocator: std.mem.Allocator) void {
        allocator.free(self.name);

        for (self.type_params.items) |param| {
            allocator.free(param);
        }

        self.type_params.deinit();

        for (self.constructors.items) |*constructor| {
            constructor.deinit(allocator);
        }

        self.constructors.deinit();

        allocator.destroy(self);
    }
};

/// Represents a field in a record type with a name and type.
pub const RecordFieldNode = struct {
    /// The name of the field.
    name: []const u8,

    /// The AST node representing the field's type.
    field_type: *Node,

    /// The token representing this field declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        name: []const u8,
        field_type: *Node,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*RecordFieldNode {
        const node = try allocator.create(RecordFieldNode);

        node.* = .{
            .name = try allocator.dupe(u8, params.name),
            .field_type = params.field_type,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *RecordFieldNode, allocator: std.mem.Allocator) void {
        allocator.free(self.name);

        self.type.deinit(allocator);
        allocator.destroy(self.type);

        allocator.destroy(self);
    }
};

/// Represents a record type declaration with named fields and types.
///
/// Examples:
/// - `type Point = { x: Int, y: Int }`
/// - `type User = { name: String, email: String }`
pub const RecordTypeNode = struct {
    /// The name of the record type.
    name: []const u8,

    /// Array of type parameter names.
    type_params: std.ArrayList([]const u8),

    /// Array of fields in the record.
    fields: std.ArrayList(RecordFieldNode),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        name: []const u8,
        type_params: std.ArrayList([]const u8),
        fields: std.ArrayList(RecordFieldNode),
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*RecordTypeNode {
        const node = try allocator.create(RecordTypeNode);

        node.* = .{
            .name = try allocator.dupe(u8, params.name),
            .type_params = params.type_params,
            .fields = params.fields,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *RecordTypeNode, allocator: std.mem.Allocator) void {
        allocator.free(self.name);

        for (self.type_params.items) |param| {
            allocator.free(param);
        }

        self.type_params.deinit();

        for (self.fields.items) |*field| {
            field.deinit(allocator);
        }

        self.fields.deinit();

        allocator.destroy(self);
    }
};

//==========================================================================
// Module System
//==========================================================================

/// Represents a module path reference, which can be a single identifier
/// or a qualified path of dot-separated identifiers.
///
/// Examples:
/// - `MyModule`
/// - `MyModule.SubModule`
/// - `Std.List`
pub const ModulePathNode = struct {
    /// Array of identifiers forming the module path.
    segments: std.ArrayList([]const u8),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        segments: std.ArrayList([]const u8),
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*ModulePathNode {
        const node = try allocator.create(ModulePathNode);

        node.* = .{
            .segments = params.segments,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *ModulePathNode, allocator: std.mem.Allocator) void {
        for (self.segments.items) |segment| {
            allocator.free(segment);
        }

        self.segments.deinit();

        allocator.destroy(self);
    }
};

pub const ExportItem = struct {
    /// The name of the item being exported.
    name: []const u8,

    /// Whether to expose constructors for exported types.
    expose_constructors: bool,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        name: []const u8,
        expose_constructors: bool,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*ExportItem {
        const item = try allocator.create(ExportItem);

        item.* = .{
            .name = try allocator.dupe(u8, params.name),
            .expose_constructors = params.expose_constructors,
            .token = params.token,
        };

        return item;
    }

    pub fn deinit(self: *ExportItem, allocator: std.mem.Allocator) void {
        allocator.free(self.name);

        allocator.destroy(self);
    }
};

/// Represents a method of exposing items from a module.
/// This can be everything (..), specific items, or nothing.
///
/// Examples:
/// - `exposing (..)`
/// - `exposing (func1, Type1, Type2(..))`
pub const ExportSpecNode = struct {
    /// Whether all items are being exposed (..).
    exposing_all: bool,

    /// Optional array of specific items being exposed.
    items: ?std.ArrayList(ExportItem),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        exposing_all: bool,
        items: ?std.ArrayList(ExportItem) = null,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*ExportSpecNode {
        const node = try allocator.create(ExportSpecNode);

        node.* = .{
            .exposing_all = params.exposing_all,
            .items = if (params.items) params.items else null,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *ExportSpecNode, allocator: std.mem.Allocator) void {
        if (self.items) |*items| {
            for (items.items) |*item| {
                item.deinit(allocator);
            }

            items.deinit();
        }

        allocator.destroy(self);
    }
};

/// Items that can be imported from a module, potentially with aliases.
/// These represent what appears in the parentheses after `using` or `hiding`.
pub const ImportItem = union(enum) {
    /// Import a function, optionally with an alias.
    ///
    /// Examples:
    /// - `map as list_map` or just `map`
    function: struct {
        /// The original name of the function in the module.
        name: []const u8,

        /// Optional alias to use in the importing module.
        alias: ?[]const u8,
    },

    /// Import an operator (like >>=).
    ///
    /// Examples:
    /// - `(>>=)` or `(>>=) as bind`
    operator: struct {
        /// The operator symbol.
        symbol: []const u8,

        /// Optional alias to use in the importing module.
        alias: ?[]const u8,
    },

    /// Import a type, optionally exposing its constructors and/or with an alias.
    ///
    /// Examples:
    /// - `Maybe(..)` or `Maybe(..) as Optional`
    type: struct {
        /// The name of the type.
        name: []const u8,

        /// Whether to expose type constructors (indicated by (..)).
        expose_constructors: bool,

        /// Optional alias to use in the importing module.
        alias: ?[]const u8,
    },

    pub const InitParams = struct {
        kind: enum {
            Function,
            Operator,
            Type,
        },
        args: struct {
            name: []const u8,
            alias: ?[]const u8 = null,
            expose_constructors: bool = false,
        },
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*ImportItem {
        const node = try allocator.create(ImportItem);

        node.* = switch (params.kind) {
            .Function => .{
                .function = .{
                    .name = try allocator.dupe(u8, params.args.name),
                    .alias = if (params.args.alias) |a| try allocator.dupe(u8, a) else null,
                },
            },
            .Operator => .{
                .operator = .{
                    .symbol = try allocator.dupe(u8, params.args.name),
                    .alias = if (params.args.alias) |a| try allocator.dupe(u8, a) else null,
                },
            },
            .Type => .{
                .type = .{
                    .name = try allocator.dupe(u8, params.args.name),
                    .expose_constructors = params.args.expose_constructors,
                    .alias = if (params.args.alias) |a| try allocator.dupe(u8, a) else null,
                },
            },
        };

        return node;
    }

    pub fn deinit(self: *ImportItem, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .function => |*f| {
                allocator.free(f.name);

                if (f.alias) |alias| {
                    allocator.free(alias);
                }
            },
            .operator => |*op| {
                allocator.free(op.symbol);

                if (op.alias) |alias| {
                    allocator.free(alias);
                }
            },
            .type => |*t| {
                allocator.free(t.name);

                if (t.alias) |alias| {
                    allocator.free(alias);
                }
            },
        }

        allocator.destroy(self);
    }
};

/// The kind of import being performed.
pub const ImportKind = enum {
    /// Simple import of an entire module.
    ///
    /// Examples:
    /// - `open MyModule`
    Simple,

    /// Import an entire module with an alias.
    ///
    /// Examples:
    /// - `open MyModule as M`
    Alias,

    /// Selective import of specific items, optionally with aliases.
    ///
    /// Examples:
    /// - `open Std.List using (map as list_map, filter)`
    Using,

    /// Import a module while excluding specific items.
    ///
    /// Examples:
    /// - `open MyModule hiding (internal_func)`
    Hiding,
};

/// Represents an import specification that controls how a module is imported.
///
/// Examples:
/// - `open MyModule`
/// - `open MyModule as M`
/// - `open Std.List using (map, filter)`
/// - `open Std.List using (map as list_map)`
/// - `open MyModule hiding (internal_func)`
pub const ImportSpecNode = struct {
    /// The module path being imported.
    path: ModulePathNode,

    /// The kind of import being performed.
    kind: ImportKind,

    /// Optional alias name for the imported module.
    /// Only used when kind is .Alias
    alias: ?[]const u8,

    /// Optional list of items being imported or renamed.
    /// Used when kind is .Using or .Hiding
    items: ?std.ArrayList(ImportItem),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        path: ModulePathNode,
        kind: ImportKind,
        alias: ?[]const u8 = null,
        items: ?std.ArrayList(ImportItem) = null,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*ImportSpecNode {
        const node = try allocator.create(ImportSpecNode);

        node.* = .{
            .path = params.path,
            .kind = params.kind,
            .alias = if (params.alias) |a| try allocator.dupe(u8, a) else null,
            .items = if (params.items) params.items else null,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *ImportSpecNode, allocator: std.mem.Allocator) void {
        for (self.path.segments.items) |segment| {
            allocator.free(segment);
        }

        self.path.segments.deinit();

        if (self.alias) |alias| {
            allocator.free(alias);
        }

        if (self.items) |*items| {
            for (items.items) |*item| {
                item.deinit(allocator);
            }

            items.deinit();
        }

        allocator.destroy(self);
    }
};

/// Represents a module include declaration that imports and re-exports
/// all contents from another module.
///
/// Examples:
/// - `include MyModule`
/// - `include Data.List`
/// - `include Parser.Internal`
pub const IncludeNode = struct {
    /// The module path being included.
    path: ModulePathNode,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        path: ModulePathNode,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*IncludeNode {
        const node = try allocator.create(IncludeNode);

        node.* = .{
            .path = params.path,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *IncludeNode, allocator: std.mem.Allocator) void {
        for (self.path.segments.items) |segment| {
            allocator.free(segment);
        }

        self.path.segments.deinit();

        allocator.destroy(self);
    }
};

//==========================================================================
// Top-Level Declarations
//==========================================================================

/// Represents a top-level function declaration with name, optional type annotation, and value.
/// The value is typically a lambda expression but can be any valid expression.
///
/// Examples:
/// - `let add : Int -> Int -> Int = \x y => x + y`
/// - `let compose : (b -> c) -> (a -> b) -> a -> c = \f g x => f (g x)`
pub const FunctionDeclNode = struct {
    /// The name of the function being declared.
    name: []const u8,

    /// Optional AST node representing the type annotation.
    type_annotation: ?*Node,

    // doc_comments: []*DocCommentNode,

    /// The AST node representing the function's implementation.
    value: *Node,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        name: []const u8,
        type_annotation: ?*Node = null,
        value: *Node,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*FunctionDeclNode {
        const node = try allocator.create(FunctionDeclNode);

        node.* = .{
            .name = try allocator.dupe(u8, params.name),
            .type_annotation = if (params.type_annotation) params.type_annotation else null,
            .value = params.value,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *FunctionDeclNode, allocator: std.mem.Allocator) void {
        allocator.free(self.name);

        if (self.type_annotation) |type_annotation| {
            type_annotation.deinit(allocator);
            allocator.destroy(type_annotation);
        }

        self.value.deinit(allocator);
        allocator.destroy(self.value);

        allocator.destroy(self);
    }
};

/// A declaration that links a function to external code, specifying both
/// the internal name/type and external symbol to link against.
///
/// Example:
/// - `foreign sqrt : Float -> Float = "c_sqrt"`
/// - `foreign print : String -> Unit = "c_print"`
pub const ForeignFunctionDeclNode = struct {
    /// The internal name used to refer to this function.
    name: []const u8,

    /// The function's type signature.
    type_annotation: *Node,

    /// The external symbol name to link against.
    external_name: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        name: []const u8,
        type_annotation: *Node,
        external_name: []const u8,
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*ForeignFunctionDeclNode {
        const node = try allocator.create(ForeignFunctionDeclNode);

        node.* = .{
            .name = try allocator.dupe(u8, params.name),
            .type_annotation = params.type_annotation,
            .external_name = try allocator.dupe(u8, params.external_name),
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *ForeignFunctionDeclNode, allocator: std.mem.Allocator) void {
        allocator.free(self.name);

        self.type_annotation.deinit(allocator);
        allocator.destroy(self.type_annotation);

        allocator.free(self.external_name);

        allocator.destroy(self);
    }
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
    /// The fully qualified path of the module.
    path: ModulePathNode,

    /// Specification of which items to expose from the module.
    exports: ExportSpecNode,

    /// Array of AST nodes representing the module's contents.
    declarations: std.ArrayList(*Node),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        path: ModulePathNode,
        exports: ExportSpecNode,
        declarations: std.ArrayList(*Node),
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*ModuleDeclNode {
        const node = try allocator.create(ModuleDeclNode);

        node.* = .{
            .path = params.path,
            .exports = params.exports,
            .declarations = params.declarations,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *ModuleDeclNode, allocator: std.mem.Allocator) void {
        for (self.path.segments.items) |segment| {
            allocator.free(segment);
        }

        self.path.segments.deinit();

        if (self.exports.items) |*items| {
            for (items.items) |*item| {
                item.deinit(allocator);
            }

            items.deinit();
        }

        for (self.declarations.items) |declaration| {
            declaration.deinit(allocator);
            allocator.destroy(declaration);
        }

        self.declarations.deinit();

        allocator.destroy(self);
    }
};

/// The root AST node containing a sequence of top-level declarations like
/// function definitions, type declarations, imports, and module definitions.
pub const ProgramNode = struct {
    /// Array of AST nodes representing top-level declarations.
    statements: std.ArrayList(*Node),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub const InitParams = struct {
        statements: std.ArrayList(*Node),
        token: lexer.Token,
    };

    pub fn init(allocator: std.mem.Allocator, params: InitParams) !*ProgramNode {
        const node = try allocator.create(ProgramNode);

        node.* = .{
            .statements = params.statements,
            .token = params.token,
        };

        return node;
    }

    pub fn deinit(self: *ProgramNode, allocator: std.mem.Allocator) void {
        for (self.statements.items) |statement| {
            statement.deinit(allocator);
            allocator.destroy(statement);
        }

        self.statements.deinit();

        allocator.destroy(self);
    }
};

//==========================================================================
// Main Node Union Type - Brings Together All Node Types
//==========================================================================

pub const Node = union(enum) {
    // Basic Literals
    comment: *CommentNode,
    doc_comment: *DocCommentNode,
    int_literal: IntLiteralNode,
    float_literal: FloatLiteralNode,
    char_literal: CharLiteralNode,
    str_literal: *StrLiteralNode,
    multiline_str_literal: *MultilineStrLiteralNode,

    // Identifiers
    lower_identifier: *LowerIdentifierNode,
    upper_identifier: *UpperIdentifierNode,

    // Basic Data Structures
    list: *ListNode,
    tuple: *TupleNode,

    // Basic Expressions
    unary_expr: *UnaryExprNode,
    arithmetic_expr: *ArithmeticExprNode,
    logical_expr: *LogicalExprNode,
    comparison_expr: *ComparisonExprNode,

    // Pattern Matching
    pattern: *PatternNode,
    match_expr: *MatchExprNode,

    // Functions and Applications
    function_type: *FunctionTypeNode,
    lambda_expr: *LambdaExprNode,
    function_application: *FuncApplicationNode,

    // Advanced Expressions
    cons_expr: *ConsExprNode,
    str_concat_expr: *StrConcatExprNode,
    list_concat_expr: *ListConcatExprNode,
    composition_expr: *CompositionExprNode,
    pipe_expr: *PipeExprNode,

    // Control Flow
    if_then_else_stmt: *IfThenElseStmtNode,

    // Type System
    typed_hole: TypedHoleNode,
    type_application: *TypeApplicationNode,
    type_alias: *TypeAliasNode,
    variant_type: *VariantTypeNode,
    record_type: *RecordTypeNode,

    // Module System
    module_path: *ModulePathNode,
    export_spec: *ExportSpecNode,
    import_spec: *ImportSpecNode,
    include: *IncludeNode,

    // Top-Level Declarations
    function_decl: *FunctionDeclNode,
    foreign_function_decl: *ForeignFunctionDeclNode,
    module_decl: *ModuleDeclNode,
    program: *ProgramNode,

    /// Cleans up resources associated with this node.
    ///
    /// Recursively deinitializes any child nodes that this node owns,
    /// ensuring no memory leaks occur.
    pub fn deinit(self: *Node, allocator: std.mem.Allocator) void {
        switch (self.*) {
            // Basic Literals
            .comment => |comment| comment.deinit(allocator),
            .doc_comment => |comment| comment.deinit(allocator),
            .int_literal => {}, // no allocation
            .float_literal => {}, // no allocation
            .char_literal => {}, // no allocation
            .str_literal => |lit| lit.deinit(allocator),
            .multiline_str_literal => |lit| lit.deinit(allocator),

            // Identifiers
            .lower_identifier => |ident| ident.deinit(allocator),
            .upper_identifier => |ident| ident.deinit(allocator),

            // Basic Data Structures
            .list => |list| list.deinit(allocator),
            .tuple => |tuple| tuple.deinit(allocator),

            // Basic Expressions
            .unary_expr => |expr| expr.deinit(allocator),
            .arithmetic_expr => |expr| expr.deinit(allocator),
            .logical_expr => |expr| expr.deinit(allocator),
            .comparison_expr => |expr| expr.deinit(allocator),

            // Pattern Matching
            .pattern => |pat| pat.deinit(allocator),
            .match_expr => |expr| expr.deinit(allocator),

            // Functions and Applications
            .function_type => |ftype| ftype.deinit(allocator),
            .lambda_expr => |expr| expr.deinit(allocator),
            .function_application => |expr| expr.deinit(allocator),

            // Advanced Expressions
            .cons_expr => |expr| expr.deinit(allocator),
            .str_concat_expr => |expr| expr.deinit(allocator),
            .list_concat_expr => |expr| expr.deinit(allocator),
            .composition_expr => |expr| expr.deinit(allocator),
            .pipe_expr => |expr| expr.deinit(allocator),

            // Control Flow
            .if_then_else_stmt => |stmt| stmt.deinit(allocator),

            // Type System
            .typed_hole => {}, // no allocation
            .type_application => |app| app.deinit(allocator),
            .type_alias => |alias| alias.deinit(allocator),
            .variant_type => |vtype| vtype.deinit(allocator),
            .record_type => |rtype| rtype.deinit(allocator),

            // Module System
            .module_path => |path| path.deinit(allocator),
            .export_spec => |spec| spec.deinit(allocator),
            .import_spec => |spec| spec.deinit(allocator),
            .include => |inc| inc.deinit(allocator),

            // Top-Level Declarations
            .function_decl => |decl| decl.deinit(allocator),
            .foreign_function_decl => |decl| decl.deinit(allocator),
            .module_decl => |decl| decl.deinit(allocator),
            .program => |prog| prog.deinit(allocator),
        }
    }
};

const testing = std.testing;

const TEST_FILE = "test.mox";

// AST node tests verify:
// 1. Construction - Node fields are properly initialized with correct types/values
// 2. Memory - Allocation and cleanup happen without leaks
// 3. Structure - Node relationships and hierarchy match the source input

test "[CommentNode]" {
    // Test input: # This is a comment

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const content = "This is a comment";

    // Action
    const params = CommentNode.InitParams{
        .content = content,
        .token = lexer.Token{
            .kind = lexer.TokenKind{ .comment = .Regular },
            .lexeme = "#",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 18 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const comment_node = try CommentNode.init(allocator, params);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .comment = comment_node };

    // Assertions
    // Verify the node is a regular comment
    try testing.expect(node.* == .comment);

    const comment = node.comment;

    // Validate the comment token and its properties
    try testing.expectEqual(lexer.TokenKind{ .comment = .Regular }, comment.token.kind);

    // Verify the content
    try testing.expectEqualStrings(content, comment.content);
}

test "[DocCommentNode]" {
    // Test input: ## This is a doc comment

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const content = "This is a doc comment";

    // Action
    const params = DocCommentNode.InitParams{
        .content = content,
        .token = lexer.Token{
            .kind = lexer.TokenKind{ .comment = .Doc },
            .lexeme = "##",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 23 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const comment_node = try DocCommentNode.init(allocator, params);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .doc_comment = comment_node };

    // Assertions
    // Verify the node is a doc comment
    try testing.expect(node.* == .doc_comment);

    const comment = node.doc_comment;

    // Validate the comment token and its properties
    try testing.expectEqual(lexer.TokenKind{ .comment = .Doc }, comment.token.kind);

    // Verify the content
    try testing.expectEqualStrings(content, comment.content);
}

test "[IntLiteralNode]" {
    // Test input: 42

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const value = 42;

    // Action
    const params = IntLiteralNode.InitParams{
        .value = value,
        .token = lexer.Token{
            .kind = lexer.TokenKind{ .literal = .Int },
            .lexeme = "42",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const int_node = IntLiteralNode.init(params);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .int_literal = int_node };

    // Assertions
    // Verify the node is an integer literal
    try testing.expect(node.* == .int_literal);

    const literal = node.int_literal;

    // Validate the literal token and its properties
    try testing.expectEqual(lexer.TokenKind{ .literal = .Int }, literal.token.kind);

    // Verify the content
    try testing.expectEqual(value, literal.value);
}

test "[FloatLiteralNode]" {
    // Test input: 42.0

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const value = 42.0;

    // Action
    const params = FloatLiteralNode.InitParams{
        .value = value,
        .token = lexer.Token{
            .kind = lexer.TokenKind{ .literal = .Float },
            .lexeme = "42.0",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const float_node = FloatLiteralNode.init(params);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .float_literal = float_node };

    // Assertions
    // Verify the node is a float literal
    try testing.expect(node.* == .float_literal);

    const literal = node.float_literal;

    // Validate the literal token and its properties
    try testing.expectEqual(lexer.TokenKind{ .literal = .Float }, literal.token.kind);

    // Verify the content
    try testing.expectEqual(value, literal.value);
}

test "[CharLiteralNode]" {
    // Test input: 'a'

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const value = 'a';

    // Action
    const params = CharLiteralNode.InitParams{
        .value = value,
        .token = lexer.Token{
            .kind = lexer.TokenKind{ .literal = .Char },
            .lexeme = "\'",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const char_node = CharLiteralNode.init(params);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .char_literal = char_node };

    // Assertions
    // Verify the node is a character literal
    try testing.expect(node.* == .char_literal);

    const literal = node.char_literal;

    // Validate the literal token and its properties
    try testing.expectEqual(lexer.TokenKind{ .literal = .Char }, literal.token.kind);

    // Verify the content
    try testing.expectEqual(value, literal.value);
}

test "[StrLiteralNode]" {
    // Test input: "foo"

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const value = "foo";

    // Action
    const params = StrLiteralNode.InitParams{
        .value = value,
        .token = lexer.Token{
            .kind = lexer.TokenKind{ .literal = .String },
            .lexeme = "\"",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const literal_node = try StrLiteralNode.init(allocator, params);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .str_literal = literal_node };

    // Assertions
    // Verify the node is a string literal
    try testing.expect(node.* == .str_literal);

    const literal = node.str_literal;

    // Validate the comment token and its properties
    try testing.expectEqual(lexer.TokenKind{ .literal = .String }, literal.token.kind);

    // Verify the content
    try testing.expectEqualStrings(value, literal.value);
}

test "[MultilineStrLiteralNode]" {
    // Test input: """
    //             first line
    //             second line
    //             """

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const value =
        \\first line
        \\second line
        \\third line
    ;

    // Action
    const params = MultilineStrLiteralNode.InitParams{
        .value = value,
        .token = lexer.Token{
            .kind = lexer.TokenKind{ .literal = .MultilineString },
            .lexeme = "\"\"\"",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 38 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const literal_node = try MultilineStrLiteralNode.init(allocator, params);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .multiline_str_literal = literal_node };

    // Assertions
    // Verify the node is a string literal
    try testing.expect(node.* == .multiline_str_literal);

    const literal = node.multiline_str_literal;

    // Validate the comment token and its properties
    try testing.expectEqual(lexer.TokenKind{ .literal = .MultilineString }, literal.token.kind);

    // Verify the content
    try testing.expectEqualStrings(value, literal.value);
}

test "[LowerIdentifierNode]" {
    // Test input: my_variable

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const name = "my_variable";

    // Action
    const params = LowerIdentifierNode.InitParams{
        .name = name,
        .token = lexer.Token{
            .kind = lexer.TokenKind{ .identifier = .Lower },
            .lexeme = name,
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 10 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const ident_node = try LowerIdentifierNode.init(allocator, params);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .lower_identifier = ident_node };

    // Assertions
    // Verify the node is a lower identifier
    try testing.expect(node.* == .lower_identifier);

    const identifier = node.lower_identifier;

    // Verify the name of the identifier
    try testing.expectEqualStrings(name, identifier.name);

    // Verify the token kind is a lower identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, identifier.token.kind);

    // Verify the lexeme matches the name
    try testing.expectEqualStrings(name, identifier.token.lexeme);
}

test "[UpperIdentifierNode]" {
    // Test input: TypeName

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const name = "TypeName";

    // Action
    const params = UpperIdentifierNode.InitParams{
        .name = name,
        .token = lexer.Token{
            .kind = lexer.TokenKind{ .identifier = .Upper },
            .lexeme = name,
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 8 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const ident_node = try UpperIdentifierNode.init(allocator, params);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .upper_identifier = ident_node };

    // Assertions
    // Verify the node is an upper identifier
    try testing.expect(node.* == .upper_identifier);

    const identifier = node.upper_identifier;

    // Verify the name of the identifier
    try testing.expectEqualStrings(name, identifier.name);

    // Verify the token kind is an upper identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, identifier.token.kind);

    // Verify the lexeme matches the name
    try testing.expectEqualStrings(name, identifier.token.lexeme);
}

test "[ListNode]" {
    // Test input: [1, 2]

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    var elements = std.ArrayList(*Node).init(allocator);

    const elem1 = try allocator.create(Node);
    elem1.* = .{
        .int_literal = IntLiteralNode.init(.{
            .value = 1,
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .literal = .Int },
                .lexeme = "1",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 1, .end = 2 },
                    .src = .{ .line = 1, .col = 2 },
                },
            },
        }),
    };

    const elem2 = try allocator.create(Node);
    elem2.* = .{
        .int_literal = IntLiteralNode.init(.{
            .value = 2,
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .literal = .Int },
                .lexeme = "2",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 4, .end = 5 },
                    .src = .{ .line = 1, .col = 5 },
                },
            },
        }),
    };

    try elements.append(elem1);
    try elements.append(elem2);

    const params = ListNode.InitParams{
        .elements = elements,
        .token = lexer.Token{
            .kind = lexer.TokenKind{ .delimiter = .LeftBracket },
            .lexeme = "[",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 6 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const list_node = try ListNode.init(allocator, params);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .list = list_node };

    // Assertions
    // Verify the node is a list
    try testing.expect(node.* == .list);

    const list = node.list;

    // Verify the list contains exactly 2 elements
    try testing.expectEqual(@as(usize, 2), list.elements.items.len);

    // Test first element (1)
    try testing.expect(node.list.elements.items[0].* == .int_literal);
    try testing.expectEqual(@as(i64, 1), list.elements.items[0].int_literal.value);

    // Test second element (2)
    try testing.expect(node.list.elements.items[1].* == .int_literal);
    try testing.expectEqual(@as(i64, 2), list.elements.items[1].int_literal.value);
}

test "[TupleNode]" {
    // Test input: (1, "hello")

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var elements = std.ArrayList(*Node).init(allocator);

    const first = try allocator.create(Node);
    first.* = .{
        .int_literal = IntLiteralNode.init(.{
            .value = 1,
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .literal = .Int },
                .lexeme = "1",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 1, .end = 2 },
                    .src = .{ .line = 1, .col = 2 },
                },
            },
        }),
    };

    const second = try allocator.create(Node);
    second.* = .{
        .str_literal = try StrLiteralNode.init(allocator, .{
            .value = "hello",
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .literal = .String },
                .lexeme = "\"hello\"",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 4, .end = 11 },
                    .src = .{ .line = 1, .col = 5 },
                },
            },
        }),
    };

    try elements.append(first);
    try elements.append(second);

    const params = TupleNode.InitParams{
        .elements = elements,
        .token = lexer.Token{
            .kind = lexer.TokenKind{ .delimiter = .LeftParen },
            .lexeme = "(",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const tuple_node = try TupleNode.init(allocator, params);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .tuple = tuple_node };

    // Assertions
    // Verify the node is a tuple.
    try testing.expect(node.* == .tuple);

    const tuple = node.tuple;

    // Verify the tuple contains exactly 2 elements.
    try testing.expectEqual(@as(usize, 2), tuple.elements.items.len);

    // Test first element (1)
    try testing.expect(tuple.elements.items[0].* == .int_literal);
    try testing.expectEqual(@as(i64, 1), tuple.elements.items[0].int_literal.value);

    // Test second element ("hello")
    try testing.expect(tuple.elements.items[1].* == .str_literal);
    try testing.expectEqualStrings("hello", tuple.elements.items[1].str_literal.value);

    // Ensure the token kind is a left parenthesis '(' indicating its start.
    try testing.expectEqual(lexer.TokenKind{ .delimiter = .LeftParen }, tuple.token.kind);
}

test "[UnaryExprNode]" {
    // Test input: -42

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const operand = try allocator.create(Node);
    operand.* = .{
        .int_literal = IntLiteralNode.init(.{
            .value = 42,
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .literal = .Int },
                .lexeme = "42",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 1, .end = 3 },
                    .src = .{ .line = 1, .col = 2 },
                },
            },
        }),
    };

    const params = UnaryExprNode.InitParams{
        .operand = operand,
        .operator = lexer.Token{
            .kind = lexer.TokenKind{ .operator = .IntSub },
            .lexeme = "-",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const unary_node = try UnaryExprNode.init(allocator, params);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .unary_expr = unary_node };

    // Assertions
    // Verify the node is a unary expression
    try testing.expect(node.* == .unary_expr);

    const expr = node.unary_expr;

    // Verify the operator in the unary expression is an integer subtraction operator (-)
    try testing.expectEqual(lexer.TokenKind{ .operator = .IntSub }, expr.operator.kind);

    // Ensure the lexeme for the operator is the string "-"
    try testing.expectEqualStrings("-", expr.operator.lexeme);

    // Verify the operand is an integer literal with the correct token kind
    try testing.expectEqual(lexer.TokenKind{ .literal = .Int }, expr.operand.int_literal.token.kind);

    // Check the value of the integer literal operand
    try testing.expect(expr.operand.int_literal.value == 42);
}

test "[ArithmeticExprNode]" {
    // Test input: 2 * 3

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const left = try allocator.create(Node);
    left.* = .{
        .int_literal = IntLiteralNode.init(.{
            .value = 2,
            .token = .{
                .kind = .{ .literal = .Int },
                .lexeme = "2",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        }),
    };

    const right = try allocator.create(Node);
    right.* = .{
        .int_literal = IntLiteralNode.init(.{
            .value = 3,
            .token = .{
                .kind = .{ .literal = .Int },
                .lexeme = "3",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 4, .end = 5 },
                    .src = .{ .line = 1, .col = 5 },
                },
            },
        }),
    };

    const params = ArithmeticExprNode.InitParams{
        .left = left,
        .right = right,
        .operator = lexer.Token{
            .kind = .{ .operator = .IntMul },
            .lexeme = "*",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 2, .end = 3 },
                .src = .{ .line = 1, .col = 3 },
            },
        },
    };

    const arithmetic_node = try ArithmeticExprNode.init(allocator, params);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .arithmetic_expr = arithmetic_node };

    // Assertions
    // Verify the node is an arithmetic expression
    try testing.expect(node.* == .arithmetic_expr);

    const expr = node.arithmetic_expr;

    // Verify the operator is multiplication
    try testing.expectEqual(lexer.TokenKind{ .operator = .IntMul }, expr.operator.kind);
    try testing.expectEqualStrings("*", expr.operator.lexeme);

    // Test left operand
    try testing.expect(expr.left.* == .int_literal);
    try testing.expectEqual(@as(i64, 2), expr.left.int_literal.value);

    // Test right operand
    try testing.expect(expr.right.* == .int_literal);
    try testing.expectEqual(@as(i64, 3), expr.right.int_literal.value);
}

test "[LogicalExprNode]" {
    // Test input: a && b

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const left = try allocator.create(Node);
    left.* = .{
        .lower_identifier = try LowerIdentifierNode.init(allocator, .{
            .name = "a",
            .token = lexer.Token{
                .kind = .{ .identifier = .Lower },
                .lexeme = "a",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        }),
    };

    const right = try allocator.create(Node);
    right.* = .{
        .lower_identifier = try LowerIdentifierNode.init(allocator, .{
            .name = "b",
            .token = lexer.Token{
                .kind = .{ .identifier = .Lower },
                .lexeme = "b",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 5, .end = 6 },
                    .src = .{ .line = 1, .col = 6 },
                },
            },
        }),
    };

    const params = LogicalExprNode.InitParams{
        .left = left,
        .right = right,
        .operator = lexer.Token{
            .kind = .{ .operator = .LogicalAnd },
            .lexeme = "&&",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 2, .end = 4 },
                .src = .{ .line = 1, .col = 3 },
            },
        },
    };

    const logical_node = try LogicalExprNode.init(allocator, params);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .logical_expr = logical_node };

    // Assertions
    // Verify the node is a logical expression
    try testing.expect(node.* == .logical_expr);

    const expr = node.logical_expr;

    // Verify the operator is logical AND
    try testing.expectEqual(lexer.TokenKind{ .operator = .LogicalAnd }, expr.operator.kind);
    try testing.expectEqualStrings("&&", expr.operator.lexeme);

    // Test left operand
    try testing.expect(expr.left.* == .lower_identifier);
    try testing.expectEqualStrings("a", expr.left.lower_identifier.name);

    // Test right operand
    try testing.expect(expr.right.* == .lower_identifier);
    try testing.expectEqualStrings("b", expr.right.lower_identifier.name);
}

test "[ComparisonExprNode]" {
    // Test input: x <= y

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const left = try allocator.create(Node);
    left.* = .{
        .lower_identifier = try LowerIdentifierNode.init(allocator, .{
            .name = "x",
            .token = lexer.Token{
                .kind = .{ .identifier = .Lower },
                .lexeme = "x",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        }),
    };

    const right = try allocator.create(Node);
    right.* = .{
        .lower_identifier = try LowerIdentifierNode.init(allocator, .{
            .name = "y",
            .token = lexer.Token{
                .kind = .{ .identifier = .Lower },
                .lexeme = "y",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 5, .end = 6 },
                    .src = .{ .line = 1, .col = 6 },
                },
            },
        }),
    };

    const params = ComparisonExprNode.InitParams{
        .left = left,
        .right = right,
        .operator = lexer.Token{
            .kind = .{ .operator = .LessThanEqual },
            .lexeme = "<=",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 2, .end = 4 },
                .src = .{ .line = 1, .col = 3 },
            },
        },
    };

    const comparison_node = try ComparisonExprNode.init(allocator, params);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .comparison_expr = comparison_node };

    // Assertions
    // Verify the node is a comparison expression
    try testing.expect(node.* == .comparison_expr);

    const expr = node.comparison_expr;

    // Verify the operator is less than or equal
    try testing.expectEqual(lexer.TokenKind{ .operator = .LessThanEqual }, expr.operator.kind);
    try testing.expectEqualStrings("<=", expr.operator.lexeme);

    // Test left operand
    try testing.expect(expr.left.* == .lower_identifier);
    try testing.expectEqualStrings("x", expr.left.lower_identifier.name);

    // Test right operand
    try testing.expect(expr.right.* == .lower_identifier);
    try testing.expectEqualStrings("y", expr.right.lower_identifier.name);
}
