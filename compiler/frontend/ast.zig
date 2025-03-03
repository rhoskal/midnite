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
    /// The text of the comment, excluding the comment marker.
    text: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *CommentNode, allocator: std.mem.Allocator) void {
        allocator.free(self.text);

        allocator.destroy(self);
    }
};

/// Represents a documentation comment that will be processed as markdown.
///
/// Examples:
/// - `## This is a doc comment`
pub const DocCommentNode = struct {
    /// The text of the documentation comment, excluding the comment marker.
    text: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *DocCommentNode, allocator: std.mem.Allocator) void {
        allocator.free(self.text);

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

    pub fn deinit(self: *StrLiteralNode, allocator: std.mem.Allocator) void {
        allocator.free(self.value);

        allocator.destroy(self);
    }
};

/// Represents a multiline string literal value.
///
/// Examples:
/// - `"""
///first line
///second line
///"""`
pub const MultilineStrLiteralNode = struct {
    /// The parsed multiline string value with escape sequences processed.
    value: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,

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
    identifier: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *LowerIdentifierNode, allocator: std.mem.Allocator) void {
        allocator.free(self.identifier);
        allocator.destroy(self);
    }
};

/// Represents an uppercase identifier reference (type names, type constructors, etc).
pub const UpperIdentifierNode = struct {
    /// The text of the identifier.
    identifier: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *UpperIdentifierNode, allocator: std.mem.Allocator) void {
        allocator.free(self.identifier);
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
/// - `_`
/// - `42`
/// - `Some(x)`
/// - `head :: tail`
/// - `[]`
pub const PatternNode = union(enum) {
    wildcard: struct {
        token: lexer.Token,
    },
    int_literal: IntLiteralNode,
    float_literal: FloatLiteralNode,
    char_literal: CharLiteralNode,
    string_literal: *StrLiteralNode,
    list: struct {
        /// Array of pattern nodes for matching against list elements.
        patterns: std.ArrayList(*PatternNode),

        /// The token representing the start of the list pattern
        token: lexer.Token,
    },
    variable: struct {
        /// The name of the variable to bind.
        name: *LowerIdentifierNode,

        /// The token representing the variable.
        token: lexer.Token,
    },
    constructor: struct {
        /// The name of the constructor.
        name: []const u8,

        /// Array of patterns for matching constructor arguments.
        parameters: std.ArrayList(*PatternNode),

        /// The token representing the constructor.
        token: lexer.Token,
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
    },

    pub fn deinit(self: *PatternNode, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .wildcard,
            .int_literal,
            .float_literal,
            .char_literal,
            .empty_list,
            => {},
            .string_literal => |lit| {
                lit.deinit(allocator);
            },
            .list => |list| {
                for (list.patterns.items) |pattern| {
                    pattern.deinit(allocator);
                }

                list.patterns.deinit();
            },
            .variable => |variable| {
                variable.name.deinit(allocator);
            },
            .constructor => |constructor| {
                allocator.free(constructor.name);

                for (constructor.parameters.items) |param| {
                    param.deinit(allocator);
                    allocator.destroy(param);
                }

                constructor.parameters.deinit();
            },
            .cons => |cons| {
                cons.head.deinit(allocator);
                allocator.destroy(cons.head);

                cons.tail.deinit(allocator);
                allocator.destroy(cons.tail);
            },
        }
    }
};

/// Represents a guard condition in a match case that must evaluate
/// to true for the pattern to match.
///
/// Examples:
/// - `when x > 0`
/// - `when is_valid?(name)`
pub const GuardNode = struct {
    /// The AST node representing the boolean condition.
    condition: *Node,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *GuardNode, allocator: std.mem.Allocator) void {
        self.condition.deinit(allocator);
        allocator.destroy(self.condition);
    }
};

/// A single pattern matching case with an optional guard condition.
/// When pattern matches and guard evaluates true, the expression is evaluated.
///
/// Examples:
/// - `0 => "zero"`
/// - `x :: xs when length(xs) > 0 => count + 1`
pub const MatchCase = struct {
    /// The pattern to match against.
    pattern: *PatternNode,

    /// The expression to evaluate if pattern matches.
    expression: *Node,

    /// Optional guard condition that must be true for match to succeed.
    guard: ?*GuardNode,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *MatchCase, allocator: std.mem.Allocator) void {
        self.pattern.deinit(allocator);
        allocator.destroy(self.pattern);

        self.expression.deinit(allocator);
        allocator.destroy(self.expression);

        if (self.guard) |guard| {
            guard.deinit(allocator);
            allocator.destroy(guard);
        }
    }
};

/// Represents a complete match expression that tests a value against multiple patterns.
/// Each pattern is paired with an expression to evaluate when that pattern matches.
///
/// Examples:
/// - `match x on | 0 => "zero" | _ => "other"`
/// - `match list on | [] => 0 | x :: xs => 1 + length(xs)`
/// - `match result on | Ok(v) => v | Err(e) => default`
pub const MatchExprNode = struct {
    /// The AST node representing the value being matched.
    subject: *Node,

    /// Array of match cases to test against.
    cases: std.ArrayList(*MatchCase),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *MatchExprNode, allocator: std.mem.Allocator) void {
        self.subject.deinit(allocator);
        allocator.destroy(self.subject);

        for (self.cases.items) |case| {
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

/// Represents a function type annotation with parameter types and a return type.
///
/// Examples:
/// - As parameter types: `map(f : (a) -> b, xs : List(a))`
/// - As return types: `curry() -> (a, b) -> c`
/// - In type aliases: `type Predicate(a) = (a) -> Bool`
pub const FunctionSignatureNode = struct {
    /// Array of AST nodes representing parameter types.
    parameter_types: std.ArrayList(*Node),

    /// The AST node representing the return type.
    return_type: *Node,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *FunctionSignatureNode, allocator: std.mem.Allocator) void {
        for (self.parameter_types.items) |param_type| {
            param_type.deinit(allocator);
            allocator.destroy(param_type);
        }

        self.parameter_types.deinit();

        self.return_type.deinit(allocator);
        allocator.destroy(self.return_type);
    }
};

/// Represents a lambda expression with a parameter list.
///
/// Examples:
/// - `fn(x, y) => x + y`
/// - `fn(x : Int, y : Int) -> Int => x + y`
/// - `fn(f, xs) => xs |> map(f)`
pub const LambdaExprNode = struct {
    /// Array of parameter declarations.
    parameters: std.ArrayList(*ParamDeclNode),

    /// Optional AST node representing the return type annotation.
    return_type: ?*Node,

    /// The AST node representing the function body expression.
    body: *Node,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *LambdaExprNode, allocator: std.mem.Allocator) void {
        for (self.parameters.items) |param| {
            param.deinit(allocator);
            allocator.destroy(param);
        }

        if (self.return_type) |rt| {
            rt.deinit(allocator);
            allocator.destroy(rt);
        }

        self.parameters.deinit();

        self.body.deinit(allocator);
        allocator.destroy(self.body);

        allocator.destroy(self);
    }
};

/// Represents a function call where a function is invoked with arguments.
///
/// Examples:
/// - `add(42, 17)`
/// - `not(and(x, y))`
/// - `map(double, [1, 2, 3])`
pub const FunctionCallNode = struct {
    /// The AST node representing the function being applied.
    function: *Node,

    /// Array of AST nodes representing the arguments passed to the function.
    arguments: std.ArrayList(*Node),

    /// The token representing the start of this application (usually the function's token).
    token: lexer.Token,

    pub fn deinit(self: *FunctionCallNode, allocator: std.mem.Allocator) void {
        self.function.deinit(allocator);
        allocator.destroy(self.function);

        for (self.arguments.items) |argument| {
            argument.deinit(allocator);
            allocator.destroy(argument);
        }

        self.arguments.deinit();

        allocator.destroy(self);
    }
};

/// Represents a parameter declaration with a name and type annotation.
pub const ParamDeclNode = struct {
    /// The name of the parameter.
    name: *LowerIdentifierNode,

    /// The AST node representing the parameter's type annotation.
    /// This is optional for regular functions but required for foreign functions.
    type_annotation: ?*Node,

    /// The token representing this parameter declaration.
    token: lexer.Token,

    pub fn deinit(self: *ParamDeclNode, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);

        if (self.type_annotation) |type_ann| {
            type_ann.deinit(allocator);
            allocator.destroy(type_ann);
        }
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

/// Represents a binary operation that passes a value through a pipeline
/// of function applications.
///
/// Examples:
/// - `x |> f()`
pub const PipeExprNode = struct {
    /// The AST node representing the value being piped.
    value: *Node,

    /// The AST node representing the function receiving the piped value.
    func: *Node,

    /// The token representing the pipe operator.
    operator: lexer.Token,

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
/// - `if empty?(list) then None else Some(head(list))`
/// - `if even?(n) then n / 2 else 3 * n + 1`
pub const IfThenElseStmtNode = struct {
    /// The AST node representing the boolean condition.
    condition: *Node,

    /// The AST node representing the expression to evaluate if condition is true.
    then_branch: *Node,

    /// The AST node representing the expression to evaluate if condition is false.
    else_branch: *Node,

    /// The token representing the start of this declaration.
    token: lexer.Token,

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
/// For example: in `Map(k, v)`, Map is the base type being applied to args k and v.
///
/// Examples:
/// - `List(a)`
/// - `Map(k, v)`
/// - `Result(e, a)`
/// - `Tree(Maybe(a))`
pub const TypeApplicationNode = struct {
    /// The type constructor being applied (e.g., Map, List, Maybe).
    constructor: *UpperIdentifierNode,

    /// The type arguments being applied.
    args: std.ArrayList(*Node),

    /// The token representing the type application.
    token: lexer.Token,

    pub fn deinit(self: *TypeApplicationNode, allocator: std.mem.Allocator) void {
        self.constructor.deinit(allocator);

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
/// - `type alias Dict(k, v) = Map(k, v)`
/// - `type alias Reducer(a, b) = (a, b) -> b`
/// - `type alias TreeMap(k, v) = Tree(Pair(k, v), Compare(k))`
pub const TypeAliasNode = struct {
    /// The name of the type alias.
    name: *UpperIdentifierNode,

    /// Array of type parameter names.
    type_params: std.ArrayList([]const u8),

    /// The AST node representing the aliased type.
    value: *Node,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *TypeAliasNode, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);

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
/// - `None`
/// - `Some(a)`
/// - `Entry(k, v)`
/// - `Node(a, Tree(a))`
pub const VariantConstructorNode = struct {
    /// The name of the constructor.
    name: *UpperIdentifierNode,

    /// Array of AST nodes representing the constructor's parameter types.
    parameters: std.ArrayList(*Node),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *VariantConstructorNode, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);

        for (self.parameters.items) |param| {
            param.deinit(allocator);
            allocator.destroy(param);
        }

        self.parameters.deinit();

        allocator.destroy(self);
    }
};

/// Represents a variant type declaration with a list of constructors.
/// Each variant type can have zero or more type parameters that can be used
/// in its constructors.
///
/// Examples:
/// - `type Maybe(a) = None | Some(a)`
/// - `type List(a) = Nil | Cons(a, List(a))`
/// - `type Tree(a) = Leaf | Branch(Tree(a), a, Tree(a))`
/// - `type Result(e, a) = Err(e) | Ok(a)`
pub const VariantTypeNode = struct {
    /// The name of the variant type.
    name: *UpperIdentifierNode,

    /// Array of type parameter names.
    type_params: std.ArrayList([]const u8),

    /// Array of constructors defined for this type.
    constructors: std.ArrayList(*VariantConstructorNode),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *VariantTypeNode, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);

        for (self.type_params.items) |param| {
            allocator.free(param);
        }

        self.type_params.deinit();

        for (self.constructors.items) |constructor| {
            constructor.deinit(allocator);
        }

        self.constructors.deinit();

        allocator.destroy(self);
    }
};

/// Represents a field in a record type with a name and type.
pub const RecordFieldNode = struct {
    /// The name of the field.
    name: *LowerIdentifierNode,

    /// The AST node representing the field's type.
    type: *Node,

    /// The token representing this field declaration.
    token: lexer.Token,

    pub fn deinit(self: *RecordFieldNode, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);

        self.type.deinit(allocator);
        allocator.destroy(self.type);
    }
};

/// Represents a record type declaration with named fields and types.
///
/// Examples:
/// - `type Point = { x : Int, y : Int }`
/// - `type User = { name : String, email : String }`
pub const RecordTypeNode = struct {
    /// The name of the record type.
    name: *UpperIdentifierNode,

    /// Array of type parameter names.
    type_params: std.ArrayList([]const u8),

    /// Array of fields in the record.
    fields: std.ArrayList(*RecordFieldNode),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *RecordTypeNode, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);

        for (self.type_params.items) |param| {
            allocator.free(param);
        }

        self.type_params.deinit();

        for (self.fields.items) |field| {
            field.deinit(allocator);
            allocator.destroy(field);
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
pub const ModulePathNode = struct {
    /// Array of identifiers forming the module path.
    segments: std.ArrayList(*UpperIdentifierNode),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *ModulePathNode, allocator: std.mem.Allocator) void {
        for (self.segments.items) |segment| {
            segment.deinit(allocator);
        }

        self.segments.deinit();
    }
};

pub const ExportItem = struct {
    /// The name of the item being exported.
    name: []const u8,

    /// Whether to expose constructors for exported types.
    expose_constructors: bool,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: ExportItem, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
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

    pub fn deinit(self: *ExportSpecNode, allocator: std.mem.Allocator) void {
        if (self.items) |*items| {
            for (items.items) |item| {
                item.deinit(allocator);
            }

            items.deinit();
        }
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
    path: *ModulePathNode,

    /// The kind of import being performed.
    kind: ImportKind,

    /// Optional alias name for the imported module.
    /// Only used when kind is .Alias
    alias: ?*UpperIdentifierNode,

    /// Optional list of items being imported or renamed.
    /// Used when kind is .Using or .Hiding
    items: ?std.ArrayList(*ImportItem),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *ImportSpecNode, allocator: std.mem.Allocator) void {
        self.path.deinit(allocator);
        allocator.destroy(self.path);

        if (self.alias) |alias| {
            alias.deinit(allocator);
        }

        if (self.items) |*items| {
            for (items.items) |item| {
                item.deinit(allocator);
                allocator.destroy(item);
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
    path: *ModulePathNode,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *IncludeNode, allocator: std.mem.Allocator) void {
        self.path.deinit(allocator);
        allocator.destroy(self.path);

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
/// - `let add(x : Int, y : Int) -> Int = x + y`
/// - `let compose(f : (b) -> c, g : (a) -> b, x : a) -> c = f(g(x))`
pub const FunctionDeclNode = struct {
    /// The name of the function being declared.
    name: *LowerIdentifierNode,

    /// Array of parameter declarations.
    parameters: std.ArrayList(*ParamDeclNode),

    /// Optional AST node representing the return type annotation.
    return_type: ?*Node,

    // doc_comments: []*DocCommentNode,

    /// The AST node representing the function's implementation.
    value: *Node,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *FunctionDeclNode, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);

        for (self.parameters.items) |param| {
            param.deinit(allocator);
            allocator.destroy(param);
        }

        self.parameters.deinit();

        if (self.return_type) |ret_type| {
            ret_type.deinit(allocator);
            allocator.destroy(ret_type);
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
/// - `foreign sqrt(x : Float) -> Float = "c_sqrt"`
/// - `foreign print(str : String) -> Unit = "c_print"`
pub const ForeignFunctionDeclNode = struct {
    /// The internal name used to refer to this function.
    name: *LowerIdentifierNode,

    /// Array of parameter declarations.
    parameters: std.ArrayList(*ParamDeclNode),

    /// The AST node representing the return type.
    return_type: *Node,

    /// The external symbol name to link against.
    external_name: *StrLiteralNode,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *ForeignFunctionDeclNode, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);

        for (self.parameters.items) |param| {
            param.deinit(allocator);
            allocator.destroy(param);
        }

        self.parameters.deinit();

        self.return_type.deinit(allocator);
        allocator.destroy(self.return_type);

        self.external_name.deinit(allocator);

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
    path: *ModulePathNode,

    /// Specification of which items to expose from the module.
    exports: *ExportSpecNode,

    /// Array of AST nodes representing the module's contents.
    declarations: std.ArrayList(*Node),

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn deinit(self: *ModuleDeclNode, allocator: std.mem.Allocator) void {
        self.path.deinit(allocator);
        allocator.destroy(self.path);

        self.exports.deinit(allocator);
        allocator.destroy(self.exports);

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
    function_signature: *FunctionSignatureNode,
    lambda_expr: *LambdaExprNode,
    function_call: *FunctionCallNode,

    // Advanced Expressions
    cons_expr: *ConsExprNode,
    str_concat_expr: *StrConcatExprNode,
    list_concat_expr: *ListConcatExprNode,
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
            .function_signature => |sig| sig.deinit(allocator),
            .lambda_expr => |expr| expr.deinit(allocator),
            .function_call => |expr| expr.deinit(allocator),

            // Advanced Expressions
            .cons_expr => |expr| expr.deinit(allocator),
            .str_concat_expr => |expr| expr.deinit(allocator),
            .list_concat_expr => |expr| expr.deinit(allocator),
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

const TEST_FILE = "test.mn";

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

    const text = "This is a comment";

    // Action
    const comment_node = try allocator.create(CommentNode);
    comment_node.* = .{
        .text = try allocator.dupe(u8, text),
        .token = .{
            .kind = .{ .comment = .Regular },
            .lexeme = "#",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 18 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

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
    try testing.expectEqualStrings(text, comment.text);
}

test "[DocCommentNode]" {
    // Test input: ## This is a doc comment

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const text = "This is a doc comment";

    // Action
    const comment_node = try allocator.create(DocCommentNode);
    comment_node.* = .{
        .text = try allocator.dupe(u8, text),
        .token = .{
            .kind = .{ .comment = .Doc },
            .lexeme = "##",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 23 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

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
    try testing.expectEqualStrings(text, comment.text);
}

test "[IntLiteralNode]" {
    // Test input: 42

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const value = 42;

    // Action
    const int_node = .{
        .value = value,
        .token = .{
            .kind = .{ .literal = .Int },
            .lexeme = "42",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

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
    const float_node = .{
        .value = value,
        .token = .{
            .kind = .{ .literal = .Float },
            .lexeme = "42.0",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

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
    const char_node = .{
        .value = value,
        .token = .{
            .kind = .{ .literal = .Char },
            .lexeme = "\'",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

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
    const literal_node = try allocator.create(StrLiteralNode);
    literal_node.* = .{
        .value = try allocator.dupe(u8, value),
        .token = .{
            .kind = .{ .literal = .String },
            .lexeme = "\"",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

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
    const literal_node = try allocator.create(MultilineStrLiteralNode);
    literal_node.* = .{
        .value = try allocator.dupe(u8, value),
        .token = .{
            .kind = .{ .literal = .MultilineString },
            .lexeme = "\"\"\"",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 38 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

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

    const identifier = "my_variable";

    // Action
    const ident_node = try allocator.create(LowerIdentifierNode);
    ident_node.* = .{
        .identifier = try allocator.dupe(u8, identifier),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = identifier,
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 10 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .lower_identifier = ident_node };

    // Assertions
    // Verify the node is a lower identifier
    try testing.expect(node.* == .lower_identifier);

    const ident = node.lower_identifier;

    // Verify the name of the identifier
    try testing.expectEqualStrings(identifier, ident.identifier);

    // Verify the token kind is a lower identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, ident.token.kind);

    // Verify the lexeme matches the name
    try testing.expectEqualStrings(identifier, ident.token.lexeme);
}

test "[UpperIdentifierNode]" {
    // Test input: TypeName

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const identifier = "TypeName";

    // Action
    const ident_node = try allocator.create(UpperIdentifierNode);
    ident_node.* = .{
        .identifier = try allocator.dupe(u8, identifier),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = identifier,
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 8 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .upper_identifier = ident_node };

    // Assertions
    // Verify the node is a upper identifier
    try testing.expect(node.* == .upper_identifier);

    const ident = node.upper_identifier;

    // Verify the name of the identifier
    try testing.expectEqualStrings(identifier, ident.identifier);

    // Verify the token kind is an upper identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, ident.token.kind);

    // Verify the lexeme matches the name
    try testing.expectEqualStrings(identifier, ident.token.lexeme);
}

test "[ListNode]" {
    // Test input: [1, 2]

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const elem1 = try allocator.create(Node);
    elem1.* = .{
        .int_literal = .{
            .value = 1,
            .token = .{
                .kind = .{ .literal = .Int },
                .lexeme = "1",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 1, .end = 2 },
                    .src = .{ .line = 1, .col = 2 },
                },
            },
        },
    };

    const elem2 = try allocator.create(Node);
    elem2.* = .{
        .int_literal = .{
            .value = 2,
            .token = .{
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

    var elements = std.ArrayList(*Node).init(allocator);
    try elements.append(elem1);
    try elements.append(elem2);

    const list_node = try allocator.create(ListNode);
    list_node.* = .{
        .elements = elements,
        .token = .{
            .kind = .{ .delimiter = .LeftBracket },
            .lexeme = "[",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 6 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

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

    const first = try allocator.create(Node);
    first.* = .{
        .int_literal = .{
            .value = 1,
            .token = .{
                .kind = .{ .literal = .Int },
                .lexeme = "1",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 1, .end = 2 },
                    .src = .{ .line = 1, .col = 2 },
                },
            },
        },
    };

    const second_str = try allocator.create(StrLiteralNode);
    second_str.* = .{
        .value = try allocator.dupe(u8, "hello"),
        .token = .{
            .kind = .{ .literal = .String },
            .lexeme = "\"hello\"",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 4, .end = 11 },
                .src = .{ .line = 1, .col = 5 },
            },
        },
    };

    const second = try allocator.create(Node);
    second.* = .{ .str_literal = second_str };

    var elements = std.ArrayList(*Node).init(allocator);
    try elements.append(first);
    try elements.append(second);

    const tuple_node = try allocator.create(TupleNode);
    tuple_node.* = .{
        .elements = elements,
        .token = .{
            .kind = .{ .delimiter = .LeftParen },
            .lexeme = "(",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

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
        .int_literal = .{
            .value = 42,
            .token = .{
                .kind = .{ .literal = .Int },
                .lexeme = "42",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 1, .end = 3 },
                    .src = .{ .line = 1, .col = 2 },
                },
            },
        },
    };

    const unary_node = try allocator.create(UnaryExprNode);
    unary_node.* = .{
        .operand = operand,
        .operator = .{
            .kind = .{ .operator = .IntSub },
            .lexeme = "-",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

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
        .int_literal = .{
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
        },
    };

    const right = try allocator.create(Node);
    right.* = .{
        .int_literal = .{
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
        },
    };

    const arithmetic_node = try allocator.create(ArithmeticExprNode);
    arithmetic_node.* = .{
        .left = left,
        .right = right,
        .operator = .{
            .kind = .{ .operator = .IntMul },
            .lexeme = "*",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 2, .end = 3 },
                .src = .{ .line = 1, .col = 3 },
            },
        },
    };

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
    const left_ident = try allocator.create(LowerIdentifierNode);
    left_ident.* = .{
        .identifier = try allocator.dupe(u8, "a"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "a",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const left = try allocator.create(Node);
    left.* = .{ .lower_identifier = left_ident };

    const right_ident = try allocator.create(LowerIdentifierNode);
    right_ident.* = .{
        .identifier = try allocator.dupe(u8, "b"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "b",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 5, .end = 6 },
                .src = .{ .line = 1, .col = 6 },
            },
        },
    };

    const right = try allocator.create(Node);
    right.* = .{ .lower_identifier = right_ident };

    const logical_node = try allocator.create(LogicalExprNode);
    logical_node.* = .{
        .left = left,
        .right = right,
        .operator = .{
            .kind = .{ .operator = .LogicalAnd },
            .lexeme = "&&",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 2, .end = 4 },
                .src = .{ .line = 1, .col = 3 },
            },
        },
    };

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
    try testing.expectEqualStrings("a", expr.left.lower_identifier.identifier);

    // Test right operand
    try testing.expect(expr.right.* == .lower_identifier);
    try testing.expectEqualStrings("b", expr.right.lower_identifier.identifier);
}

test "[ComparisonExprNode]" {
    // Test input: x <= y

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const left_ident = try allocator.create(LowerIdentifierNode);
    left_ident.* = .{
        .identifier = try allocator.dupe(u8, "x"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "x",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const left = try allocator.create(Node);
    left.* = .{ .lower_identifier = left_ident };

    const right_ident = try allocator.create(LowerIdentifierNode);
    right_ident.* = .{
        .identifier = try allocator.dupe(u8, "y"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "y",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 5, .end = 6 },
                .src = .{ .line = 1, .col = 6 },
            },
        },
    };

    const right = try allocator.create(Node);
    right.* = .{ .lower_identifier = right_ident };

    const comparison_node = try allocator.create(ComparisonExprNode);
    comparison_node.* = .{
        .left = left,
        .right = right,
        .operator = .{
            .kind = .{ .operator = .LessThanEqual },
            .lexeme = "<=",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 2, .end = 4 },
                .src = .{ .line = 1, .col = 3 },
            },
        },
    };

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
    try testing.expectEqualStrings("x", expr.left.lower_identifier.identifier);

    // Test right operand
    try testing.expect(expr.right.* == .lower_identifier);
    try testing.expectEqualStrings("y", expr.right.lower_identifier.identifier);
}

test "[MatchExprNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        // Test basic constructor pattern matching
        // Test input: match opt on | Some(x) => x | None => 0

        // Action
        const opt_ident = try allocator.create(LowerIdentifierNode);
        opt_ident.* = .{
            .identifier = try allocator.dupe(u8, "opt"),
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "opt",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 6, .end = 9 },
                    .src = .{ .line = 1, .col = 7 },
                },
            },
        };

        const subject_node = try allocator.create(Node);
        subject_node.* = .{ .lower_identifier = opt_ident };

        // Case 1: Some x => x
        const x_name = try allocator.create(LowerIdentifierNode);
        x_name.* = .{
            .identifier = try allocator.dupe(u8, "x"),
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "x",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 20, .end = 21 },
                    .src = .{ .line = 1, .col = 21 },
                },
            },
        };

        const var_pattern = try allocator.create(PatternNode);
        var_pattern.* = .{
            .variable = .{
                .name = x_name,
                .token = .{
                    .kind = .{ .identifier = .Lower },
                    .lexeme = "x",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 20, .end = 21 },
                        .src = .{ .line = 1, .col = 21 },
                    },
                },
            },
        };

        var some_params = std.ArrayList(*PatternNode).init(allocator);
        try some_params.append(var_pattern);

        const some_pattern = try allocator.create(PatternNode);
        some_pattern.* = .{
            .constructor = .{
                .name = try allocator.dupe(u8, "Some"),
                .parameters = some_params,
                .token = .{
                    .kind = .{ .identifier = .Upper },
                    .lexeme = "Some",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 15, .end = 19 },
                        .src = .{ .line = 1, .col = 16 },
                    },
                },
            },
        };

        const x_expr_ident = try allocator.create(LowerIdentifierNode);
        x_expr_ident.* = .{
            .identifier = try allocator.dupe(u8, "x"),
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "x",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 26, .end = 27 },
                    .src = .{ .line = 1, .col = 27 },
                },
            },
        };

        const some_expr = try allocator.create(Node);
        some_expr.* = .{ .lower_identifier = x_expr_ident };

        const some_case = try allocator.create(MatchCase);
        some_case.* = .{
            .pattern = some_pattern,
            .expression = some_expr,
            .guard = null,
            .token = .{
                .kind = .{ .symbol = .Pipe },
                .lexeme = "|",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 13, .end = 14 },
                    .src = .{ .line = 1, .col = 14 },
                },
            },
        };

        // Case 2: None => 0
        const none_pattern = try allocator.create(PatternNode);
        none_pattern.* = .{
            .constructor = .{
                .name = try allocator.dupe(u8, "None"),
                .parameters = std.ArrayList(*PatternNode).init(allocator),
                .token = .{
                    .kind = .{ .identifier = .Upper },
                    .lexeme = "None",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 30, .end = 34 },
                        .src = .{ .line = 1, .col = 31 },
                    },
                },
            },
        };

        const none_expr = try allocator.create(Node);
        none_expr.* = .{
            .int_literal = .{
                .value = 0,
                .token = .{
                    .kind = .{ .literal = .Int },
                    .lexeme = "0",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 38, .end = 39 },
                        .src = .{ .line = 1, .col = 39 },
                    },
                },
            },
        };

        const none_case = try allocator.create(MatchCase);
        none_case.* = .{
            .pattern = none_pattern,
            .expression = none_expr,
            .guard = null,
            .token = .{
                .kind = .{ .symbol = .Pipe },
                .lexeme = "|",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 28, .end = 29 },
                    .src = .{ .line = 1, .col = 29 },
                },
            },
        };

        var cases = std.ArrayList(*MatchCase).init(allocator);
        try cases.append(some_case);
        try cases.append(none_case);

        const match_expr = try allocator.create(MatchExprNode);
        match_expr.* = .{
            .subject = subject_node,
            .cases = cases,
            .token = .{
                .kind = .{ .keyword = .Match },
                .lexeme = "match",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 5 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        };

        const node = try allocator.create(Node);
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        node.* = .{ .match_expr = match_expr };

        // Assertions
        // Verify the node is a match expression
        try testing.expect(node.* == .match_expr);

        const match = node.match_expr;

        // Verify the match expression has exactly two cases
        try testing.expectEqual(@as(usize, 2), match.cases.items.len);

        // Ensure the constructor name for the first case is "Some"
        const case1 = match.cases.items[0];
        try testing.expectEqualStrings("Some", case1.pattern.constructor.name);

        // Check the argument for the "Some" constructor is a variable named "x"
        try testing.expectEqualStrings("x", case1.pattern.constructor.parameters.items[0].variable.name.identifier);

        // Ensure the constructor name for the second case is "None"
        const case2 = match.cases.items[1];
        try testing.expectEqualStrings("None", case2.pattern.constructor.name);

        // Verify the "None" constructor in the second case has no arguments
        try testing.expectEqual(@as(usize, 0), case2.pattern.constructor.parameters.items.len);
    }

    {
        // Test list pattern with cons
        // Test input: match list on | head :: tail => head | [] => 0

        // Action
        // Value to match on (a list variable)
        const list_ident = try allocator.create(LowerIdentifierNode);
        list_ident.* = .{
            .identifier = try allocator.dupe(u8, "list"),
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "list",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 6, .end = 10 },
                    .src = .{ .line = 1, .col = 7 },
                },
            },
        };

        const subject_node = try allocator.create(Node);
        subject_node.* = .{ .lower_identifier = list_ident };

        // Case 1: head :: tail => head
        const head_ident = try allocator.create(LowerIdentifierNode);
        head_ident.* = .{
            .identifier = try allocator.dupe(u8, "head"),
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "head",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 15, .end = 19 },
                    .src = .{ .line = 1, .col = 16 },
                },
            },
        };

        const head_pattern = try allocator.create(PatternNode);
        head_pattern.* = .{
            .variable = .{
                .name = head_ident,
                .token = .{
                    .kind = .{ .identifier = .Lower },
                    .lexeme = "head",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 15, .end = 19 },
                        .src = .{ .line = 1, .col = 16 },
                    },
                },
            },
        };

        const tail_ident = try allocator.create(LowerIdentifierNode);
        tail_ident.* = .{
            .identifier = try allocator.dupe(u8, "tail"),
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "tail",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 23, .end = 27 },
                    .src = .{ .line = 1, .col = 24 },
                },
            },
        };

        const tail_pattern = try allocator.create(PatternNode);
        tail_pattern.* = .{
            .variable = .{
                .name = tail_ident,
                .token = .{
                    .kind = .{ .identifier = .Lower },
                    .lexeme = "tail",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 23, .end = 27 },
                        .src = .{ .line = 1, .col = 24 },
                    },
                },
            },
        };

        const cons_pattern = try allocator.create(PatternNode);
        cons_pattern.* = .{
            .cons = .{
                .head = head_pattern,
                .tail = tail_pattern,
                .token = .{
                    .kind = .{ .operator = .Cons },
                    .lexeme = "::",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 20, .end = 22 },
                        .src = .{ .line = 1, .col = 21 },
                    },
                },
            },
        };

        const head_expr_ident = try allocator.create(LowerIdentifierNode);
        head_expr_ident.* = .{
            .identifier = try allocator.dupe(u8, "head"),
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "head",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 32, .end = 36 },
                    .src = .{ .line = 1, .col = 33 },
                },
            },
        };

        const head_expr = try allocator.create(Node);
        head_expr.* = .{ .lower_identifier = head_expr_ident };

        const cons_case = try allocator.create(MatchCase);
        cons_case.* = .{
            .pattern = cons_pattern,
            .expression = head_expr,
            .guard = null,
            .token = .{
                .kind = .{ .symbol = .Pipe },
                .lexeme = "|",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 13, .end = 14 },
                    .src = .{ .line = 1, .col = 14 },
                },
            },
        };

        // Case 2: [] => 0
        const empty_pattern = try allocator.create(PatternNode);
        empty_pattern.* = .{
            .empty_list = .{
                .token = .{
                    .kind = .{ .delimiter = .LeftBracket },
                    .lexeme = "[]",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 39, .end = 41 },
                        .src = .{ .line = 1, .col = 40 },
                    },
                },
            },
        };

        const zero_node = try allocator.create(Node);
        zero_node.* = .{
            .int_literal = .{
                .value = 0,
                .token = .{
                    .kind = .{ .literal = .Int },
                    .lexeme = "0",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 45, .end = 46 },
                        .src = .{ .line = 1, .col = 46 },
                    },
                },
            },
        };

        const empty_case = try allocator.create(MatchCase);
        empty_case.* = .{
            .pattern = empty_pattern,
            .expression = zero_node,
            .guard = null,
            .token = .{
                .kind = .{ .symbol = .Pipe },
                .lexeme = "|",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 37, .end = 38 },
                    .src = .{ .line = 1, .col = 38 },
                },
            },
        };

        var cases = std.ArrayList(*MatchCase).init(allocator);
        try cases.append(cons_case);
        try cases.append(empty_case);

        const match_expr = try allocator.create(MatchExprNode);
        match_expr.* = .{
            .subject = subject_node,
            .cases = cases,
            .token = .{
                .kind = .{ .keyword = .Match },
                .lexeme = "match",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 5 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        };

        const node = try allocator.create(Node);
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        node.* = .{ .match_expr = match_expr };

        // Assertions
        // Verify the node is a match expression
        try testing.expect(node.* == .match_expr);

        const match = node.match_expr;

        // Verify the match expression has exactly two cases
        try testing.expectEqual(@as(usize, 2), match.cases.items.len);

        // Test the first case (cons pattern)
        const list_cons_case = match.cases.items[0];

        // Verify the pattern in the first case is a cons pattern (head :: tail)
        try testing.expect(list_cons_case.pattern.* == .cons);

        // Ensure the name of the head variable in the cons pattern is "head"
        try testing.expectEqualStrings("head", list_cons_case.pattern.cons.head.variable.name.identifier);

        // Ensure the name of the tail variable in the cons pattern is "tail"
        try testing.expectEqualStrings("tail", list_cons_case.pattern.cons.tail.variable.name.identifier);

        // Test the second case (empty list pattern)
        const empty_list_case = match.cases.items[1];

        // Verify the pattern in the second case is an empty list
        try testing.expect(empty_list_case.pattern.* == .empty_list);
    }

    {
        // Test guards
        // Test input: match x on | n when n > 0 => "positive" | n => "non-positive"

        // Action
        const x_ident = try allocator.create(LowerIdentifierNode);
        x_ident.* = .{
            .identifier = try allocator.dupe(u8, "x"),
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "x",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 6, .end = 7 },
                    .src = .{ .line = 1, .col = 7 },
                },
            },
        };

        const subject_node = try allocator.create(Node);
        subject_node.* = .{ .lower_identifier = x_ident };

        // Case 1: n when n > 0 => "positive"
        const n_ident = try allocator.create(LowerIdentifierNode);
        n_ident.* = .{
            .identifier = try allocator.dupe(u8, "n"),
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "n",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 13, .end = 14 },
                    .src = .{ .line = 1, .col = 14 },
                },
            },
        };

        const n_pattern = try allocator.create(PatternNode);
        n_pattern.* = .{
            .variable = .{
                .name = n_ident,
                .token = .{
                    .kind = .{ .identifier = .Lower },
                    .lexeme = "n",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 13, .end = 14 },
                        .src = .{ .line = 1, .col = 14 },
                    },
                },
            },
        };

        const n_ref_ident = try allocator.create(LowerIdentifierNode);
        n_ref_ident.* = .{
            .identifier = try allocator.dupe(u8, "n"),
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "n",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 15, .end = 16 },
                    .src = .{ .line = 1, .col = 16 },
                },
            },
        };

        const n_ref = try allocator.create(Node);
        n_ref.* = .{ .lower_identifier = n_ref_ident };

        const zero_node = try allocator.create(Node);
        zero_node.* = .{
            .int_literal = .{
                .value = 0,
                .token = .{
                    .kind = .{ .literal = .Int },
                    .lexeme = "0",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 19, .end = 20 },
                        .src = .{ .line = 1, .col = 20 },
                    },
                },
            },
        };

        const comp_expr = try allocator.create(ComparisonExprNode);
        comp_expr.* = .{
            .left = n_ref,
            .right = zero_node,
            .operator = .{
                .kind = .{ .operator = .GreaterThan },
                .lexeme = ">",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 17, .end = 18 },
                    .src = .{ .line = 1, .col = 18 },
                },
            },
        };

        const guard_expr = try allocator.create(Node);
        guard_expr.* = .{ .comparison_expr = comp_expr };

        const guard = try allocator.create(GuardNode);
        guard.* = .{
            .condition = guard_expr,
            .token = .{
                .kind = .{ .keyword = .When },
                .lexeme = "when",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 15, .end = 19 },
                    .src = .{ .line = 1, .col = 16 },
                },
            },
        };

        const positive_str = try allocator.create(StrLiteralNode);
        positive_str.* = .{
            .value = try allocator.dupe(u8, "positive"),
            .token = .{
                .kind = .{ .literal = .String },
                .lexeme = "\"positive\"",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 24, .end = 34 },
                    .src = .{ .line = 1, .col = 25 },
                },
            },
        };

        const positive_expr = try allocator.create(Node);
        positive_expr.* = .{ .str_literal = positive_str };

        const guarded_case = try allocator.create(MatchCase);
        guarded_case.* = .{
            .pattern = n_pattern,
            .expression = positive_expr,
            .guard = guard,
            .token = .{
                .kind = .{ .symbol = .Pipe },
                .lexeme = "|",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 12, .end = 13 },
                    .src = .{ .line = 1, .col = 13 },
                },
            },
        };

        // Case 2: n => "non-positive"
        const n2_ident = try allocator.create(LowerIdentifierNode);
        n2_ident.* = .{
            .identifier = try allocator.dupe(u8, "n"),
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "n",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 36, .end = 37 },
                    .src = .{ .line = 1, .col = 37 },
                },
            },
        };

        const n2_pattern = try allocator.create(PatternNode);
        n2_pattern.* = .{
            .variable = .{
                .name = n2_ident,
                .token = .{
                    .kind = .{ .identifier = .Lower },
                    .lexeme = "n",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 36, .end = 37 },
                        .src = .{ .line = 1, .col = 37 },
                    },
                },
            },
        };

        const non_pos_str = try allocator.create(StrLiteralNode);
        non_pos_str.* = .{
            .value = try allocator.dupe(u8, "non-positive"),
            .token = .{
                .kind = .{ .literal = .String },
                .lexeme = "\"non-positive\"",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 41, .end = 55 },
                    .src = .{ .line = 1, .col = 42 },
                },
            },
        };

        const non_positive_expr = try allocator.create(Node);
        non_positive_expr.* = .{ .str_literal = non_pos_str };

        const default_case = try allocator.create(MatchCase);
        default_case.* = .{
            .pattern = n2_pattern,
            .expression = non_positive_expr,
            .guard = null,
            .token = .{
                .kind = .{ .symbol = .Pipe },
                .lexeme = "|",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 35, .end = 36 },
                    .src = .{ .line = 1, .col = 36 },
                },
            },
        };

        var cases = std.ArrayList(*MatchCase).init(allocator);
        try cases.append(guarded_case);
        try cases.append(default_case);

        const match_expr = try allocator.create(MatchExprNode);
        match_expr.* = .{
            .subject = subject_node,
            .cases = cases,
            .token = .{
                .kind = .{ .keyword = .Match },
                .lexeme = "match",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 5 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        };

        const node = try allocator.create(Node);
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        node.* = .{ .match_expr = match_expr };

        // Assertions
        // Verify the node is a match expression
        try testing.expect(node.* == .match_expr);

        const match = node.match_expr;

        // Verify the match expression has exactly two cases
        try testing.expectEqual(@as(usize, 2), match.cases.items.len);

        // Test the first case (guarded case)
        const positive_case = match.cases.items[0];

        // Verify the pattern in the guarded case is a variable
        try testing.expect(positive_case.pattern.* == .variable);

        // Verify the name of the variable
        try testing.expectEqualStrings("n", positive_case.pattern.variable.name.identifier);

        // Ensure the guarded case has a guard condition
        try testing.expect(positive_case.guard != null);

        // Verify the expression in the guarded case is the string literal "positive"
        try testing.expectEqualStrings("positive", positive_case.expression.str_literal.value);

        // Test the second case (catch-all case)
        const non_positive_case = match.cases.items[1];

        // Verify the pattern in the catch-all case is a variable
        try testing.expect(non_positive_case.pattern.* == .variable);

        // Ensure the name of the variable in the pattern is "n"
        try testing.expectEqualStrings("n", non_positive_case.pattern.variable.name.identifier);

        // Ensure the catch-all case does not have a guard condition
        try testing.expect(non_positive_case.guard == null);

        // Verify the expression in the catch-all case is the string literal "non-positive"
        try testing.expectEqualStrings("non-positive", non_positive_case.expression.str_literal.value);
    }
}

test "[FunctionSignatureNode]" {
    // Test input: : (Int, Int) -> Int

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const int_type1 = try allocator.create(UpperIdentifierNode);
    int_type1.* = .{
        .identifier = try allocator.dupe(u8, "Int"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Int",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 1, .end = 4 },
                .src = .{ .line = 1, .col = 2 },
            },
        },
    };

    const int_node1 = try allocator.create(Node);
    int_node1.* = .{ .upper_identifier = int_type1 };

    const int_type2 = try allocator.create(UpperIdentifierNode);
    int_type2.* = .{
        .identifier = try allocator.dupe(u8, "Int"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Int",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 6, .end = 9 },
                .src = .{ .line = 1, .col = 7 },
            },
        },
    };

    const int_node2 = try allocator.create(Node);
    int_node2.* = .{ .upper_identifier = int_type2 };

    const int_type3 = try allocator.create(UpperIdentifierNode);
    int_type3.* = .{
        .identifier = try allocator.dupe(u8, "Int"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Int",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 14, .end = 17 },
                .src = .{ .line = 1, .col = 15 },
            },
        },
    };

    const return_type_node = try allocator.create(Node);
    return_type_node.* = .{ .upper_identifier = int_type3 };

    var parameter_types = std.ArrayList(*Node).init(allocator);
    try parameter_types.append(int_node1);
    try parameter_types.append(int_node2);

    const func_signature = try allocator.create(FunctionSignatureNode);
    defer allocator.destroy(func_signature);

    func_signature.* = .{
        .parameter_types = parameter_types,
        .return_type = return_type_node,
        .token = .{
            .kind = .{ .symbol = .ArrowRight },
            .lexeme = "->",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 11, .end = 13 },
                .src = .{ .line = 1, .col = 12 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .function_signature = func_signature };

    // Assertions
    // Verify the node is a function signature
    try testing.expect(node.* == .function_signature);

    const signature = node.function_signature;

    // Verify function signature has exactly two parameter types
    try testing.expectEqual(@as(usize, 2), signature.parameter_types.items.len);

    // Check the first parameter type is Int
    try testing.expect(signature.parameter_types.items[0].* == .upper_identifier);
    try testing.expectEqualStrings("Int", signature.parameter_types.items[0].upper_identifier.identifier);

    // Check the second parameter type is Int
    try testing.expect(signature.parameter_types.items[1].* == .upper_identifier);
    try testing.expectEqualStrings("Int", signature.parameter_types.items[1].upper_identifier.identifier);

    // Check the return type is Int
    try testing.expect(signature.return_type.* == .upper_identifier);
    try testing.expectEqualStrings("Int", signature.return_type.upper_identifier.identifier);

    // Verify the token is the arrow symbol
    try testing.expectEqual(lexer.TokenKind{ .symbol = .ArrowRight }, signature.token.kind);
    try testing.expectEqualStrings("->", signature.token.lexeme);
}

test "[LambdaExprNode]" {
    // Test input: fn(x, y) => x + y

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    var parameters = std.ArrayList(*ParamDeclNode).init(allocator);

    const x_param_ident = try allocator.create(LowerIdentifierNode);
    x_param_ident.* = .{
        .identifier = try allocator.dupe(u8, "x"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "x",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 3, .end = 4 },
                .src = .{ .line = 1, .col = 4 },
            },
        },
    };

    const x_param = try allocator.create(ParamDeclNode);
    x_param.* = .{
        .name = x_param_ident,
        .type_annotation = null,
        .token = x_param_ident.token,
    };

    const y_param_ident = try allocator.create(LowerIdentifierNode);
    y_param_ident.* = .{
        .identifier = try allocator.dupe(u8, "y"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "y",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 6, .end = 7 },
                .src = .{ .line = 1, .col = 7 },
            },
        },
    };

    const y_param = try allocator.create(ParamDeclNode);
    y_param.* = .{
        .name = y_param_ident,
        .type_annotation = null,
        .token = y_param_ident.token,
    };

    try parameters.append(x_param);
    try parameters.append(y_param);

    const x_ident = try allocator.create(LowerIdentifierNode);
    x_ident.* = .{
        .identifier = try allocator.dupe(u8, "x"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "x",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 12, .end = 13 },
                .src = .{ .line = 1, .col = 13 },
            },
        },
    };

    const x_node = try allocator.create(Node);
    x_node.* = .{ .lower_identifier = x_ident };

    const y_ident = try allocator.create(LowerIdentifierNode);
    y_ident.* = .{
        .identifier = try allocator.dupe(u8, "y"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "y",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 16, .end = 17 },
                .src = .{ .line = 1, .col = 17 },
            },
        },
    };

    const y_node = try allocator.create(Node);
    y_node.* = .{ .lower_identifier = y_ident };

    const arithmetic_node = try allocator.create(ArithmeticExprNode);
    arithmetic_node.* = .{
        .left = x_node,
        .right = y_node,
        .operator = .{
            .kind = .{ .operator = .IntAdd },
            .lexeme = "+",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 14, .end = 15 },
                .src = .{ .line = 1, .col = 15 },
            },
        },
    };

    const body_node = try allocator.create(Node);
    body_node.* = .{ .arithmetic_expr = arithmetic_node };

    const lambda_node = try allocator.create(LambdaExprNode);
    lambda_node.* = .{
        .parameters = parameters,
        .return_type = null,
        .body = body_node,
        .token = .{
            .kind = .{ .keyword = .Fn },
            .lexeme = "fn",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .lambda_expr = lambda_node };

    // Assertions
    // Verify the node is a lambda expression
    try testing.expect(node.* == .lambda_expr);

    const expr = node.lambda_expr;

    // Verify that the lambda expression has exactly two parameters
    try testing.expectEqual(@as(usize, 2), expr.parameters.items.len);

    // Verify the token kind matches
    try testing.expectEqual(lexer.TokenKind{ .keyword = .Fn }, expr.token.kind);

    // Verify the token lexeme matches
    try testing.expectEqualStrings("fn", expr.token.lexeme);

    // Check first parameter (x)
    const first_param = expr.parameters.items[0];
    try testing.expectEqualStrings("x", first_param.name.identifier);
    try testing.expect(first_param.type_annotation == null);

    // Check second parameter (y)
    const second_param = expr.parameters.items[1];
    try testing.expectEqualStrings("y", second_param.name.identifier);
    try testing.expect(second_param.type_annotation == null);

    const lambda_body = expr.body.arithmetic_expr;

    // Verify the operator in the arithmetic expression is an integer addition operator (+)
    try testing.expectEqual(lexer.TokenKind{ .operator = .IntAdd }, lambda_body.operator.kind);

    // Ensure the lexeme for the addition operator is "+"
    try testing.expectEqualStrings("+", lambda_body.operator.lexeme);

    // Check the left operand is a lower-case identifier
    try testing.expect(lambda_body.left.* == .lower_identifier);

    // Verify the name of the left identifier is "x"
    try testing.expectEqualStrings("x", lambda_body.left.lower_identifier.identifier);

    // Check the right operand is a lower-case identifier
    try testing.expect(lambda_body.right.* == .lower_identifier);

    // Verify the name of the right identifier is "y"
    try testing.expectEqualStrings("y", lambda_body.right.lower_identifier.identifier);
}

test "[FunctionCallNode]" {
    // Test input: not(True)

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const not_ident = try allocator.create(LowerIdentifierNode);
    not_ident.* = .{
        .identifier = try allocator.dupe(u8, "not"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "not",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const func_node = try allocator.create(Node);
    func_node.* = .{ .lower_identifier = not_ident };

    var arguments = std.ArrayList(*Node).init(allocator);

    const true_ident = try allocator.create(UpperIdentifierNode);
    true_ident.* = .{
        .identifier = try allocator.dupe(u8, "True"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "True",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 4, .end = 8 },
                .src = .{ .line = 1, .col = 5 },
            },
        },
    };

    const arg_node = try allocator.create(Node);
    arg_node.* = .{ .upper_identifier = true_ident };

    try arguments.append(arg_node);

    const function_call_node = try allocator.create(FunctionCallNode);
    function_call_node.* = .{
        .function = func_node,
        .arguments = arguments,
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "not",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .function_call = function_call_node };

    // Assertions
    // Verify the node is a function call
    try testing.expect(node.* == .function_call);

    const call = node.function_call;

    // Verify that the function is a lower identifier named "not"
    try testing.expect(call.function.* == .lower_identifier);
    try testing.expectEqualStrings("not", call.function.lower_identifier.identifier);
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, call.function.lower_identifier.token.kind);

    // Verify that the argument is an upper identifier named "True"
    const first_arg = call.arguments.items[0];
    try testing.expect(first_arg.* == .upper_identifier);
    try testing.expectEqualStrings("True", first_arg.upper_identifier.identifier);
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, first_arg.upper_identifier.token.kind);

    // Verify the function application token matches the function's token
    try testing.expectEqual(call.token.kind, call.function.lower_identifier.token.kind);
    try testing.expectEqualStrings(call.token.lexeme, call.function.lower_identifier.token.lexeme);
}

test "[ConsExprNode]" {
    // Test input: 1 :: [2, 3]

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    var elements = std.ArrayList(*Node).init(allocator);

    const one_node = try allocator.create(Node);
    one_node.* = .{
        .int_literal = .{
            .value = 1,
            .token = .{
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

    const two_node = try allocator.create(Node);
    two_node.* = .{
        .int_literal = .{
            .value = 2,
            .token = .{
                .kind = .{ .literal = .Int },
                .lexeme = "2",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 6, .end = 7 },
                    .src = .{ .line = 1, .col = 7 },
                },
            },
        },
    };

    const three_node = try allocator.create(Node);
    three_node.* = .{
        .int_literal = .{
            .value = 3,
            .token = .{
                .kind = .{ .literal = .Int },
                .lexeme = "3",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 9, .end = 10 },
                    .src = .{ .line = 1, .col = 10 },
                },
            },
        },
    };

    try elements.append(two_node);
    try elements.append(three_node);

    const list_node = try allocator.create(ListNode);
    list_node.* = .{
        .elements = elements,
        .token = .{
            .kind = .{ .delimiter = .LeftBracket },
            .lexeme = "[",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 5, .end = 6 },
                .src = .{ .line = 1, .col = 6 },
            },
        },
    };

    const tail_node = try allocator.create(Node);
    tail_node.* = .{ .list = list_node };

    const cons_node = try allocator.create(ConsExprNode);
    cons_node.* = .{
        .head = one_node,
        .tail = tail_node,
        .operator = .{
            .kind = .{ .operator = .Cons },
            .lexeme = "::",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 2, .end = 3 },
                .src = .{ .line = 1, .col = 3 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .cons_expr = cons_node };

    // Assertions
    // Verify the expression is a cons expression (::)
    try testing.expect(node.* == .cons_expr);

    const cons = node.cons_expr;

    // Verify the operator in the cons expression is a cons operator (::)
    try testing.expectEqual(lexer.TokenKind{ .operator = .Cons }, cons.operator.kind);

    // Verify the lexeme of the cons operator is "::"
    try testing.expectEqualStrings("::", cons.operator.lexeme);

    // Verify the head of the cons expression is an integer literal with the value 1
    try testing.expect(cons.head.* == .int_literal);
    try testing.expectEqual(@as(i64, 1), cons.head.int_literal.value);

    // Verify the tail of the cons expression is a list
    try testing.expect(cons.tail.* == .list);

    // Verify the list in the tail has exactly 2 elements
    try testing.expectEqual(@as(usize, 2), cons.tail.list.elements.items.len);

    const list_elements = cons.tail.list.elements.items;

    // Verify the first element in the list is an integer literal with the value 2
    try testing.expect(list_elements[0].* == .int_literal);
    try testing.expectEqual(@as(i64, 2), list_elements[0].int_literal.value);

    // Verify the second element in the list is an integer literal with the value 3
    try testing.expect(list_elements[1].* == .int_literal);
    try testing.expectEqual(@as(i64, 3), list_elements[1].int_literal.value);
}

test "[StrConcatExprNode]" {
    // Test input: "Hello" <> "World"

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const hello_str = try allocator.create(StrLiteralNode);
    hello_str.* = .{
        .value = try allocator.dupe(u8, "Hello"),
        .token = .{
            .kind = .{ .literal = .String },
            .lexeme = "\"Hello\"",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 7 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const left_node = try allocator.create(Node);
    left_node.* = .{ .str_literal = hello_str };

    const world_str = try allocator.create(StrLiteralNode);
    world_str.* = .{
        .value = try allocator.dupe(u8, "World"),
        .token = .{
            .kind = .{ .literal = .String },
            .lexeme = "\"World\"",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 11, .end = 18 },
                .src = .{ .line = 1, .col = 12 },
            },
        },
    };

    const right_node = try allocator.create(Node);
    right_node.* = .{ .str_literal = world_str };

    const concat_node = try allocator.create(StrConcatExprNode);
    concat_node.* = .{
        .left = left_node,
        .right = right_node,
        .operator = .{
            .kind = .{ .operator = .StrConcat },
            .lexeme = "<>",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 8, .end = 10 },
                .src = .{ .line = 1, .col = 9 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .str_concat_expr = concat_node };

    // Assertions
    // Verify the expression is a string concat expression (<>)
    try testing.expect(node.* == .str_concat_expr);

    const concat = node.str_concat_expr;

    // Verify the operator in the string concatenation expression is a string concatenation operator (<>)
    try testing.expectEqual(lexer.TokenKind{ .operator = .StrConcat }, concat.operator.kind);

    // Verify the lexeme of the string concatenation operator is "<>"
    try testing.expectEqualStrings("<>", concat.operator.lexeme);

    // Verify the left operand of the string concatenation is a string literal
    try testing.expectEqualStrings("Hello", concat.left.str_literal.value);

    // Verify the right operand of the string concatenation is a string literal
    try testing.expectEqualStrings("World", concat.right.str_literal.value);
}

test "[ListConcatExprNode]" {
    // Test input: [1, 2] ++ [3, 4]

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    var left_elements = std.ArrayList(*Node).init(allocator);

    const one_node = try allocator.create(Node);
    one_node.* = .{
        .int_literal = .{
            .value = 1,
            .token = .{
                .kind = .{ .literal = .Int },
                .lexeme = "1",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 1, .end = 2 },
                    .src = .{ .line = 1, .col = 2 },
                },
            },
        },
    };

    const two_node = try allocator.create(Node);
    two_node.* = .{
        .int_literal = .{
            .value = 2,
            .token = .{
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

    try left_elements.append(one_node);
    try left_elements.append(two_node);

    const left_list = try allocator.create(ListNode);
    left_list.* = .{
        .elements = left_elements,
        .token = .{
            .kind = .{ .delimiter = .LeftBracket },
            .lexeme = "[",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const left_node = try allocator.create(Node);
    left_node.* = .{ .list = left_list };

    var right_elements = std.ArrayList(*Node).init(allocator);

    const three_node = try allocator.create(Node);
    three_node.* = .{
        .int_literal = .{
            .value = 3,
            .token = .{
                .kind = .{ .literal = .Int },
                .lexeme = "3",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 10, .end = 11 },
                    .src = .{ .line = 1, .col = 11 },
                },
            },
        },
    };

    const four_node = try allocator.create(Node);
    four_node.* = .{
        .int_literal = .{
            .value = 4,
            .token = .{
                .kind = .{ .literal = .Int },
                .lexeme = "4",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 13, .end = 14 },
                    .src = .{ .line = 1, .col = 14 },
                },
            },
        },
    };

    try right_elements.append(three_node);
    try right_elements.append(four_node);

    const right_list = try allocator.create(ListNode);
    right_list.* = .{
        .elements = right_elements,
        .token = .{
            .kind = .{ .delimiter = .LeftBracket },
            .lexeme = "[",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 9, .end = 10 },
                .src = .{ .line = 1, .col = 10 },
            },
        },
    };

    const right_node = try allocator.create(Node);
    right_node.* = .{ .list = right_list };

    const concat_node = try allocator.create(ListConcatExprNode);
    concat_node.* = .{
        .left = left_node,
        .right = right_node,
        .operator = .{
            .kind = .{ .operator = .ListConcat },
            .lexeme = "++",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 6, .end = 8 },
                .src = .{ .line = 1, .col = 7 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .list_concat_expr = concat_node };

    // Assertions
    // Verify the expression is a list concat expression (++)
    try testing.expect(node.* == .list_concat_expr);

    const expr = node.list_concat_expr;

    // Verify the operator in the expression is a list concatenation operator (++)
    try testing.expectEqual(lexer.TokenKind{ .operator = .ListConcat }, expr.operator.kind);

    // Verify the lexeme of the list concatenation operator is "++"
    try testing.expectEqualStrings("++", expr.operator.lexeme);

    // Verify the left operand of the list concatenation is a list
    try testing.expect(expr.left.* == .list);

    // Verify the left list has exactly 2 elements
    try testing.expectEqual(@as(usize, 2), expr.left.list.elements.items.len);

    // Verify the first element of the left list is an integer literal with the value 1
    try testing.expect(expr.left.list.elements.items[0].* == .int_literal);
    try testing.expectEqual(@as(i64, 1), expr.left.list.elements.items[0].int_literal.value);

    // Verify the second element of the left list is an integer literal with the value 2
    try testing.expect(expr.left.list.elements.items[1].* == .int_literal);
    try testing.expectEqual(@as(i64, 2), expr.left.list.elements.items[1].int_literal.value);

    // Verify the right operand of the list concatenation is a list
    try testing.expect(expr.right.* == .list);

    // Verify the right list has exactly 2 elements
    try testing.expectEqual(@as(usize, 2), expr.right.list.elements.items.len);

    // Verify the first element of the right list is an integer literal with the value 3
    try testing.expect(expr.right.list.elements.items[0].* == .int_literal);
    try testing.expectEqual(@as(i64, 3), expr.right.list.elements.items[0].int_literal.value);

    // Verify the second element of the right list is an integer literal with the value 4
    try testing.expect(expr.right.list.elements.items[1].* == .int_literal);
    try testing.expectEqual(@as(i64, 4), expr.right.list.elements.items[1].int_literal.value);
}

test "[PipeExprNode]" {
    // Test input: x |> f() (pipe right)

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const x_ident = try allocator.create(LowerIdentifierNode);
    x_ident.* = .{
        .identifier = try allocator.dupe(u8, "x"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "x",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const value_node = try allocator.create(Node);
    value_node.* = .{ .lower_identifier = x_ident };

    const f_ident = try allocator.create(LowerIdentifierNode);
    f_ident.* = .{
        .identifier = try allocator.dupe(u8, "f"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "f",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 5, .end = 6 },
                .src = .{ .line = 1, .col = 6 },
            },
        },
    };

    const f_node = try allocator.create(Node);
    f_node.* = .{ .lower_identifier = f_ident };

    const arguments = std.ArrayList(*Node).init(allocator);

    const func_call_node = try allocator.create(FunctionCallNode);
    func_call_node.* = .{
        .function = f_node,
        .arguments = arguments,
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "f",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 5, .end = 6 },
                .src = .{ .line = 1, .col = 6 },
            },
        },
    };

    const func_node = try allocator.create(Node);
    func_node.* = .{ .function_call = func_call_node };

    const pipe_node = try allocator.create(PipeExprNode);
    pipe_node.* = .{
        .value = value_node,
        .func = func_node,
        .operator = .{
            .kind = .{ .operator = .PipeRight },
            .lexeme = "|>",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 2, .end = 4 },
                .src = .{ .line = 1, .col = 3 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .pipe_expr = pipe_node };

    // Assertions
    // Verify the expression is a pipe expression
    try testing.expect(node.* == .pipe_expr);

    const expr = node.pipe_expr;

    // Verify the operator in the expression is a pipe-right operator (|>)
    try testing.expectEqual(lexer.TokenKind{ .operator = .PipeRight }, expr.operator.kind);

    // Verify the lexeme of the pipe-right operator
    try testing.expectEqualStrings("|>", expr.operator.lexeme);

    // Verify the value being piped is a lower identifier
    try testing.expect(expr.value.* == .lower_identifier);

    // Verify the function being applied is a function call
    try testing.expect(expr.func.* == .function_call);

    const func_call = expr.func.function_call;

    // Verify the function call's function is a lower identifier with name "f"
    try testing.expect(func_call.function.* == .lower_identifier);
    try testing.expectEqualStrings("f", func_call.function.lower_identifier.identifier);

    // Verify the function call has no arguments (empty list)
    try testing.expectEqual(@as(usize, 0), func_call.arguments.items.len);
}

test "[IfThenElseStmtNode]" {
    // Test input: if x == y then True else False

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const x_ident = try allocator.create(LowerIdentifierNode);
    x_ident.* = .{
        .identifier = try allocator.dupe(u8, "x"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "x",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 3, .end = 4 },
                .src = .{ .line = 1, .col = 4 },
            },
        },
    };

    const x_node = try allocator.create(Node);
    x_node.* = .{ .lower_identifier = x_ident };

    const y_ident = try allocator.create(LowerIdentifierNode);
    y_ident.* = .{
        .identifier = try allocator.dupe(u8, "y"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "y",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 8, .end = 9 },
                .src = .{ .line = 1, .col = 9 },
            },
        },
    };

    const y_node = try allocator.create(Node);
    y_node.* = .{ .lower_identifier = y_ident };

    const condition_node = try allocator.create(ComparisonExprNode);
    condition_node.* = .{
        .left = x_node,
        .right = y_node,
        .operator = .{
            .kind = .{ .operator = .Equality },
            .lexeme = "==",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 5, .end = 7 },
                .src = .{ .line = 1, .col = 6 },
            },
        },
    };

    const cond_node = try allocator.create(Node);
    cond_node.* = .{ .comparison_expr = condition_node };

    const true_ident = try allocator.create(UpperIdentifierNode);
    true_ident.* = .{
        .identifier = try allocator.dupe(u8, "True"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "True",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 15, .end = 19 },
                .src = .{ .line = 1, .col = 16 },
            },
        },
    };

    const then_node = try allocator.create(Node);
    then_node.* = .{ .upper_identifier = true_ident };

    const false_ident = try allocator.create(UpperIdentifierNode);
    false_ident.* = .{
        .identifier = try allocator.dupe(u8, "False"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "False",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 25, .end = 30 },
                .src = .{ .line = 1, .col = 26 },
            },
        },
    };

    const else_node = try allocator.create(Node);
    else_node.* = .{ .upper_identifier = false_ident };

    const if_node = try allocator.create(IfThenElseStmtNode);
    if_node.* = .{
        .condition = cond_node,
        .then_branch = then_node,
        .else_branch = else_node,
        .token = .{
            .kind = .{ .keyword = .If },
            .lexeme = "if",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .if_then_else_stmt = if_node };

    // Assertions
    // Verify the expression is an if-then-else statement
    try testing.expect(node.* == .if_then_else_stmt);

    // Verify the condition is a comparison expression
    try testing.expect(node.if_then_else_stmt.condition.* == .comparison_expr);

    const expr = node.if_then_else_stmt.condition.comparison_expr;

    // Check the operator in the comparison expression is an equality operator (==)
    try testing.expectEqual(lexer.TokenKind{ .operator = .Equality }, expr.operator.kind);

    // Ensure the lexeme for the equality operator is "=="
    try testing.expectEqualStrings("==", expr.operator.lexeme);

    // Verify the left operand of the condition is a lower-case identifier (x)
    try testing.expect(expr.left.* == .lower_identifier);

    // Check the name of the left identifier is "x"
    try testing.expectEqualStrings("x", expr.left.lower_identifier.identifier);

    // Ensure the token kind of the left identifier is a lower-case identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, expr.left.lower_identifier.token.kind);

    // Verify the right operand of the condition is a lower-case identifier (y)
    try testing.expect(expr.right.* == .lower_identifier);

    // Check the name of the right identifier is "y"
    try testing.expectEqualStrings("y", expr.right.lower_identifier.identifier);

    // Ensure the token kind of the right identifier is a lower-case identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, expr.right.lower_identifier.token.kind);

    const stmt = node.if_then_else_stmt;

    // Verify the "then" branch is an upper-case identifier (True)
    try testing.expect(stmt.then_branch.* == .upper_identifier);

    // Check the name of the "then" branch is "True"
    try testing.expectEqualStrings("True", stmt.then_branch.upper_identifier.identifier);

    // Ensure the token kind of the "then" branch is an upper-case identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, stmt.then_branch.upper_identifier.token.kind);

    // Verify the "else" branch is an upper-case identifier (False)
    try testing.expect(stmt.else_branch.* == .upper_identifier);

    // Check the name of the "else" branch is "False"
    try testing.expectEqualStrings("False", stmt.else_branch.upper_identifier.identifier);

    // Ensure the token kind of the "else" branch is an upper-case identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, stmt.else_branch.upper_identifier.token.kind);
}

test "[TypedHoleNode]" {
    // Test input: ?

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const hole_node = try allocator.create(TypedHoleNode);
    defer allocator.destroy(hole_node);

    hole_node.* = .{
        .token = .{
            .kind = .{ .special = .Hole },
            .lexeme = "?",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .typed_hole = hole_node.* };

    // Assertions
    // Verify the node is a typed hole
    try testing.expect(node.* == .typed_hole);

    // Verify the token kind is a question mark
    try testing.expectEqual(lexer.TokenKind{ .special = .Hole }, node.typed_hole.token.kind);

    // Verify the lexeme is a question mark
    try testing.expectEqualStrings("?", node.typed_hole.token.lexeme);
}

test "[TypeApplicationNode]" {
    // Test input: Map(k, v)

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const map_ident = try allocator.create(UpperIdentifierNode);
    map_ident.* = .{
        .identifier = try allocator.dupe(u8, "Map"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Map",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const k_ident = try allocator.create(LowerIdentifierNode);
    k_ident.* = .{
        .identifier = try allocator.dupe(u8, "k"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "k",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 4, .end = 5 },
                .src = .{ .line = 1, .col = 5 },
            },
        },
    };

    const k_node = try allocator.create(Node);
    k_node.* = .{ .lower_identifier = k_ident };

    const v_ident = try allocator.create(LowerIdentifierNode);
    v_ident.* = .{
        .identifier = try allocator.dupe(u8, "v"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "v",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 7, .end = 8 },
                .src = .{ .line = 1, .col = 8 },
            },
        },
    };

    const v_node = try allocator.create(Node);
    v_node.* = .{ .lower_identifier = v_ident };

    var args = std.ArrayList(*Node).init(allocator);
    try args.append(k_node);
    try args.append(v_node);

    const type_application = try allocator.create(TypeApplicationNode);
    type_application.* = .{
        .constructor = map_ident,
        .args = args,
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Map",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .type_application = type_application };

    // Assertions
    // Verify the node is a type application
    try testing.expect(node.* == .type_application);

    const app = node.type_application;

    // Verify the base type of the type application is an upper identifier named "Map"
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, app.constructor.token.kind);
    try testing.expectEqualStrings("Map", app.constructor.identifier);

    // Verify that the type application has exactly two arguments
    try testing.expectEqual(@as(usize, 2), app.args.items.len);

    // Verify the first argument is a lower identifier with the name "k"
    try testing.expect(app.args.items[0].* == .lower_identifier);
    try testing.expectEqualStrings("k", app.args.items[0].lower_identifier.identifier);

    // Verify the second argument is a lower identifier with the name "v"
    try testing.expect(app.args.items[1].* == .lower_identifier);
    try testing.expectEqualStrings("v", app.args.items[1].lower_identifier.identifier);
}

test "[TypeAliasNode]" {
    // Test input: type alias UserId = String

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const user_id_ident = try allocator.create(UpperIdentifierNode);
    user_id_ident.* = .{
        .identifier = try allocator.dupe(u8, "UserId"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "UserId",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 11, .end = 17 },
                .src = .{ .line = 1, .col = 12 },
            },
        },
    };

    const type_params = std.ArrayList([]const u8).init(allocator);

    const string_ident = try allocator.create(UpperIdentifierNode);
    string_ident.* = .{
        .identifier = try allocator.dupe(u8, "String"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "String",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 20, .end = 26 },
                .src = .{ .line = 1, .col = 21 },
            },
        },
    };

    const value_node = try allocator.create(Node);
    value_node.* = .{ .upper_identifier = string_ident };

    const type_alias = try allocator.create(TypeAliasNode);
    type_alias.* = .{
        .name = user_id_ident,
        .type_params = type_params,
        .value = value_node,
        .token = .{
            .kind = .{ .keyword = .Type },
            .lexeme = "type",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 4 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .type_alias = type_alias };

    // Assertions
    // Verify the node is a type alias
    try testing.expect(node.* == .type_alias);

    // Verify the type alias has no type parameters
    try testing.expectEqual(@as(usize, 0), node.type_alias.type_params.items.len);

    // Verify the "type" keyword starts the type alias declaration
    try testing.expectEqual(lexer.TokenKind{ .keyword = .Type }, node.type_alias.token.kind);

    // Verify the lexeme of the "type" keyword
    try testing.expectEqualStrings("type", node.type_alias.token.lexeme);

    // Verify the name of the type alias
    try testing.expectEqualStrings("UserId", node.type_alias.name.identifier);

    // Check the value node is an upper identifier
    try testing.expect(node.type_alias.value.* == .upper_identifier);

    const aliased_type = node.type_alias.value.upper_identifier;

    // Verify the name of the upper identifier
    try testing.expectEqualStrings("String", aliased_type.identifier);

    // Verify the token kind of the upper identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, aliased_type.token.kind);

    // Verify the lexeme of the upper identifier
    try testing.expectEqualStrings("String", aliased_type.token.lexeme);
}

test "[VariantTypeNode]" {
    // Test input: type Result(e, a) = | Err(e) | Ok(a)

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const result_ident = try allocator.create(UpperIdentifierNode);
    result_ident.* = .{
        .identifier = try allocator.dupe(u8, "Result"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Result",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 5, .end = 11 },
                .src = .{ .line = 1, .col = 6 },
            },
        },
    };

    var type_params = std.ArrayList([]const u8).init(allocator);
    try type_params.append(try allocator.dupe(u8, "e"));
    try type_params.append(try allocator.dupe(u8, "a"));

    const e_ident = try allocator.create(LowerIdentifierNode);
    e_ident.* = .{
        .identifier = try allocator.dupe(u8, "e"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "e",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 22, .end = 23 },
                .src = .{ .line = 1, .col = 23 },
            },
        },
    };

    const e_node = try allocator.create(Node);
    e_node.* = .{ .lower_identifier = e_ident };

    var err_params = std.ArrayList(*Node).init(allocator);
    try err_params.append(e_node);

    const err_ident = try allocator.create(UpperIdentifierNode);
    err_ident.* = .{
        .identifier = try allocator.dupe(u8, "Err"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Err",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 18, .end = 21 },
                .src = .{ .line = 1, .col = 19 },
            },
        },
    };

    const err_constructor = try allocator.create(VariantConstructorNode);
    err_constructor.* = .{
        .name = err_ident,
        .parameters = err_params,
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Err",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 18, .end = 21 },
                .src = .{ .line = 1, .col = 19 },
            },
        },
    };

    const a_ident = try allocator.create(LowerIdentifierNode);
    a_ident.* = .{
        .identifier = try allocator.dupe(u8, "a"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "a",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 29, .end = 30 },
                .src = .{ .line = 1, .col = 30 },
            },
        },
    };

    const a_node = try allocator.create(Node);
    a_node.* = .{ .lower_identifier = a_ident };

    var ok_params = std.ArrayList(*Node).init(allocator);
    try ok_params.append(a_node);

    const ok_ident = try allocator.create(UpperIdentifierNode);
    ok_ident.* = .{
        .identifier = try allocator.dupe(u8, "Ok"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Ok",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 26, .end = 28 },
                .src = .{ .line = 1, .col = 27 },
            },
        },
    };

    const ok_constructor = try allocator.create(VariantConstructorNode);
    ok_constructor.* = .{
        .name = ok_ident,
        .parameters = ok_params,
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Ok",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 26, .end = 28 },
                .src = .{ .line = 1, .col = 27 },
            },
        },
    };

    var constructors = std.ArrayList(*VariantConstructorNode).init(allocator);
    try constructors.append(err_constructor);
    try constructors.append(ok_constructor);

    const variant_type = try allocator.create(VariantTypeNode);
    variant_type.* = .{
        .name = result_ident,
        .type_params = type_params,
        .constructors = constructors,
        .token = .{
            .kind = .{ .keyword = .Type },
            .lexeme = "type",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 4 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .variant_type = variant_type };

    // Assertions
    // Verify the node is a variant type
    try testing.expect(node.* == .variant_type);

    const vtype = node.variant_type;

    // Verify the name of the variant type is "Result"
    try testing.expectEqualStrings("Result", vtype.name.identifier);

    // Verify the variant type has exactly two type parameters
    try testing.expectEqual(@as(usize, 2), vtype.type_params.items.len);

    // Verify the name of the first type parameter is "e"
    try testing.expectEqualStrings("e", vtype.type_params.items[0]);

    // Verify the name of the second type parameter is "a"
    try testing.expectEqualStrings("a", vtype.type_params.items[1]);

    const err_variant = vtype.constructors.items[0];

    // Verify the name of the first constructor is "Err"
    try testing.expectEqualStrings("Err", err_variant.name.identifier);

    // Verify the variant type has exactly two constructors
    try testing.expectEqual(@as(usize, 2), node.variant_type.constructors.items.len);

    // Verify the parameter for the "Err" constructor is "e"
    try testing.expect(err_variant.parameters.items[0].* == .lower_identifier);
    try testing.expectEqualStrings("e", err_variant.parameters.items[0].lower_identifier.identifier);

    const ok_variant = vtype.constructors.items[1];

    // Verify the name of the second constructor is "Ok"
    try testing.expectEqualStrings("Ok", ok_variant.name.identifier);

    // Verify the "Ok" constructor has one parameter
    try testing.expectEqual(@as(usize, 1), ok_variant.parameters.items.len);

    // Verify the parameter for the "Ok" constructor is "a"
    try testing.expect(ok_variant.parameters.items[0].* == .lower_identifier);
    try testing.expectEqualStrings("a", ok_variant.parameters.items[0].lower_identifier.identifier);
}

test "[RecordTypeNode]" {
    // Test input: type Point a = { x : a, y : a }

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const point_ident = try allocator.create(UpperIdentifierNode);
    point_ident.* = .{
        .identifier = try allocator.dupe(u8, "Point"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Point",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 5, .end = 10 },
                .src = .{ .line = 1, .col = 6 },
            },
        },
    };

    var type_params = std.ArrayList([]const u8).init(allocator);
    try type_params.append(try allocator.dupe(u8, "a"));

    var fields = std.ArrayList(*RecordFieldNode).init(allocator);

    const x_ident = try allocator.create(LowerIdentifierNode);
    x_ident.* = .{
        .identifier = try allocator.dupe(u8, "x"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "x",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 17, .end = 18 },
                .src = .{ .line = 1, .col = 18 },
            },
        },
    };

    const a_ident1 = try allocator.create(LowerIdentifierNode);
    a_ident1.* = .{
        .identifier = try allocator.dupe(u8, "a"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "a",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 19, .end = 20 },
                .src = .{ .line = 1, .col = 20 },
            },
        },
    };

    const a_type1 = try allocator.create(Node);
    a_type1.* = .{ .lower_identifier = a_ident1 };

    const field_x = try allocator.create(RecordFieldNode);
    field_x.* = .{
        .name = x_ident,
        .type = a_type1,
        .token = x_ident.token,
    };

    const y_ident = try allocator.create(LowerIdentifierNode);
    y_ident.* = .{
        .identifier = try allocator.dupe(u8, "y"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "y",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 23, .end = 24 },
                .src = .{ .line = 1, .col = 24 },
            },
        },
    };

    const a_ident2 = try allocator.create(LowerIdentifierNode);
    a_ident2.* = .{
        .identifier = try allocator.dupe(u8, "a"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "a",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 25, .end = 26 },
                .src = .{ .line = 1, .col = 26 },
            },
        },
    };

    const a_type2 = try allocator.create(Node);
    a_type2.* = .{ .lower_identifier = a_ident2 };

    const field_y = try allocator.create(RecordFieldNode);
    field_y.* = .{
        .name = y_ident,
        .type = a_type2,
        .token = y_ident.token,
    };

    try fields.append(field_x);
    try fields.append(field_y);

    const rtype_node = try allocator.create(RecordTypeNode);
    rtype_node.* = .{
        .name = point_ident,
        .type_params = type_params,
        .fields = fields,
        .token = .{
            .kind = .{ .keyword = .Type },
            .lexeme = "type",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 4 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .record_type = rtype_node };

    // Assertions
    // Verify the node is a record type
    try testing.expect(node.* == .record_type);

    const record = node.record_type;

    // Verify the name of the record type is "Point"
    try testing.expectEqualStrings("Point", record.name.identifier);

    // Ensure the record type has exactly one type parameter
    try testing.expectEqual(@as(usize, 1), record.type_params.items.len);

    // Check the name of the type parameter is "a"
    try testing.expectEqualStrings("a", record.type_params.items[0]);

    // Verify the record type has exactly two fields
    try testing.expectEqual(@as(usize, 2), record.fields.items.len);

    // Test the first field (x)
    const x_field = record.fields.items[0];

    // Ensure the name of the first field is "x"
    try testing.expectEqualStrings("x", x_field.name.identifier);

    // Verify the type of the first field is a lower-case identifier
    try testing.expect(x_field.type.* == .lower_identifier);

    // Check the name of the type for the first field is "a"
    try testing.expectEqualStrings("a", x_field.type.lower_identifier.identifier);

    // Test the second field (y)
    const y_field = record.fields.items[1];

    // Ensure the name of the second field is "y"
    try testing.expectEqualStrings("y", y_field.name.identifier);

    // Verify the type of the second field is a lower-case identifier
    try testing.expect(y_field.type.* == .lower_identifier);

    // Check the name of the type for the second field is "a"
    try testing.expectEqualStrings("a", y_field.type.lower_identifier.identifier);

    // Verify the token kind for the record is "Type"
    try testing.expectEqual(lexer.TokenKind{ .keyword = .Type }, record.token.kind);

    // Ensure the lexeme for the record token is "type"
    try testing.expectEqualStrings("type", record.token.lexeme);
}

test "[ModulePathNode]" {
    // Test input: Std.List

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    var segments = std.ArrayList(*UpperIdentifierNode).init(allocator);

    const std_ident = try allocator.create(UpperIdentifierNode);
    std_ident.* = .{
        .identifier = try allocator.dupe(u8, "Std"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Std",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const list_ident = try allocator.create(UpperIdentifierNode);
    list_ident.* = .{
        .identifier = try allocator.dupe(u8, "List"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "List",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 4, .end = 8 },
                .src = .{ .line = 1, .col = 5 },
            },
        },
    };

    try segments.append(std_ident);
    try segments.append(list_ident);

    const path_node = try allocator.create(ModulePathNode);
    defer allocator.destroy(path_node);

    path_node.* = .{
        .segments = segments,
        .token = std_ident.token,
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .module_path = path_node };

    // Assertions
    // Verify the node is a module path
    try testing.expect(node.* == .module_path);

    const path = node.module_path;

    // Verify the include path consists of exactly two segments
    try testing.expectEqual(@as(usize, 2), path.segments.items.len);

    // Ensure the first segment of the include path is "Std"
    try testing.expectEqualStrings("Std", path.segments.items[0].identifier);

    // Ensure the second segment of the include path is "List"
    try testing.expectEqualStrings("List", path.segments.items[1].identifier);
}

test "[ExportSpecNode]" {
    // Test input: exposing (func1, Type1(..))

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const item1 = ExportItem{
        .name = try allocator.dupe(u8, "func1"),
        .expose_constructors = false,
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "func1",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 10, .end = 15 },
                .src = .{ .line = 1, .col = 11 },
            },
        },
    };

    const item2 = ExportItem{
        .name = try allocator.dupe(u8, "Type1"),
        .expose_constructors = true,
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Type1",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 17, .end = 22 },
                .src = .{ .line = 1, .col = 18 },
            },
        },
    };

    var items = std.ArrayList(ExportItem).init(allocator);
    try items.append(item1);
    try items.append(item2);

    const export_node = try allocator.create(ExportSpecNode);
    export_node.* = .{
        .exposing_all = false,
        .items = items,
        .token = .{
            .kind = .{ .keyword = .Exposing },
            .lexeme = "exposing",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 8 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);

        allocator.destroy(export_node);
    }

    node.* = .{ .export_spec = export_node };

    // Assertions
    // Verify the node is an export specification
    try testing.expect(node.* == .export_spec);

    // Verify it's not exposing all items
    try testing.expect(!node.export_spec.exposing_all);

    // Verify items list contains two exports
    try testing.expectEqual(@as(usize, 2), node.export_spec.items.?.items.len);

    // Test first export (func1)
    try testing.expectEqualStrings("func1", node.export_spec.items.?.items[0].name);
    try testing.expect(!node.export_spec.items.?.items[0].expose_constructors);

    // Test second export (Type1(..))
    try testing.expectEqualStrings("Type1", node.export_spec.items.?.items[1].name);
    try testing.expect(node.export_spec.items.?.items[1].expose_constructors);
}

test "[ImportSpecNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        // Test input: open MyModule

        var segments = std.ArrayList(*UpperIdentifierNode).init(allocator);

        const module_ident = try allocator.create(UpperIdentifierNode);
        module_ident.* = .{
            .identifier = try allocator.dupe(u8, "MyModule"),
            .token = .{
                .kind = .{ .identifier = .Upper },
                .lexeme = "MyModule",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 5, .end = 13 },
                    .src = .{ .line = 1, .col = 6 },
                },
            },
        };

        try segments.append(module_ident);

        const path_node = try allocator.create(ModulePathNode);
        path_node.* = .{
            .segments = segments,
            .token = module_ident.token,
        };

        const import_node = try allocator.create(ImportSpecNode);
        import_node.* = .{
            .path = path_node,
            .kind = .Simple,
            .alias = null,
            .items = null,
            .token = .{
                .kind = .{ .keyword = .Open },
                .lexeme = "open",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 4 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        };

        const node = try allocator.create(Node);
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        node.* = .{ .import_spec = import_node };

        // Assertions
        // Verify the node is an import specification
        try testing.expect(node.* == .import_spec);

        const spec = node.import_spec;

        // Verify it's a simple import
        try testing.expectEqual(ImportKind.Simple, spec.kind);

        // Verify path has exactly one segment
        try testing.expectEqual(@as(usize, 1), spec.path.segments.items.len);

        // Verify the module name
        try testing.expectEqualStrings("MyModule", spec.path.segments.items[0].identifier);

        // Verify it has no alias or items
        try testing.expect(spec.alias == null);
        try testing.expect(spec.items == null);
    }

    {
        // Test input: open MyModule as M

        var segments = std.ArrayList(*UpperIdentifierNode).init(allocator);

        const module_ident = try allocator.create(UpperIdentifierNode);
        module_ident.* = .{
            .identifier = try allocator.dupe(u8, "MyModule"),
            .token = .{
                .kind = .{ .identifier = .Upper },
                .lexeme = "MyModule",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 5, .end = 13 },
                    .src = .{ .line = 1, .col = 6 },
                },
            },
        };

        try segments.append(module_ident);

        const path_node = try allocator.create(ModulePathNode);
        path_node.* = .{
            .segments = segments,
            .token = module_ident.token,
        };

        const alias_ident = try allocator.create(UpperIdentifierNode);
        alias_ident.* = .{
            .identifier = try allocator.dupe(u8, "M"),
            .token = .{
                .kind = .{ .identifier = .Upper },
                .lexeme = "M",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 17, .end = 18 },
                    .src = .{ .line = 1, .col = 18 },
                },
            },
        };

        const import_node = try allocator.create(ImportSpecNode);
        import_node.* = .{
            .path = path_node,
            .kind = .Alias,
            .alias = alias_ident,
            .items = null,
            .token = .{
                .kind = .{ .keyword = .Open },
                .lexeme = "open",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 4 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        };

        const node = try allocator.create(Node);
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        node.* = .{ .import_spec = import_node };

        // Assertions
        // Verify the node is an import specification
        try testing.expect(node.* == .import_spec);

        const spec = node.import_spec;

        // Verify it's an alias import
        try testing.expectEqual(ImportKind.Alias, spec.kind);

        // Verify path has exactly one segment
        try testing.expectEqual(@as(usize, 1), spec.path.segments.items.len);

        // Verify the module name
        try testing.expectEqualStrings("MyModule", spec.path.segments.items[0].identifier);

        // Verify the alias
        try testing.expect(spec.alias != null);
        try testing.expectEqualStrings("M", spec.alias.?.identifier);

        // Verify it has no items
        try testing.expect(spec.items == null);
    }

    {
        // Test input: open MyModule using (map, filter, Maybe, Either(..))

        var segments = std.ArrayList(*UpperIdentifierNode).init(allocator);

        const module_ident = try allocator.create(UpperIdentifierNode);
        module_ident.* = .{
            .identifier = try allocator.dupe(u8, "MyModule"),
            .token = .{
                .kind = .{ .identifier = .Upper },
                .lexeme = "MyModule",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 5, .end = 13 },
                    .src = .{ .line = 1, .col = 6 },
                },
            },
        };

        try segments.append(module_ident);

        const path_node = try allocator.create(ModulePathNode);
        path_node.* = .{
            .segments = segments,
            .token = module_ident.token,
        };

        const map_item = try allocator.create(ImportItem);
        map_item.* = .{
            .function = .{
                .name = try allocator.dupe(u8, "map"),
                .alias = null,
            },
        };

        const filter_item = try allocator.create(ImportItem);
        filter_item.* = .{
            .function = .{
                .name = try allocator.dupe(u8, "filter"),
                .alias = null,
            },
        };

        const maybe_item = try allocator.create(ImportItem);
        maybe_item.* = .{
            .type = .{
                .name = try allocator.dupe(u8, "Maybe"),
                .expose_constructors = false,
                .alias = null,
            },
        };

        const either_item = try allocator.create(ImportItem);
        either_item.* = .{
            .type = .{
                .name = try allocator.dupe(u8, "Either"),
                .expose_constructors = true,
                .alias = null,
            },
        };

        var items = std.ArrayList(*ImportItem).init(allocator);
        try items.append(map_item);
        try items.append(filter_item);
        try items.append(maybe_item);
        try items.append(either_item);

        const import_node = try allocator.create(ImportSpecNode);
        import_node.* = .{
            .path = path_node,
            .kind = .Using,
            .alias = null,
            .items = items,
            .token = .{
                .kind = .{ .keyword = .Open },
                .lexeme = "open",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 4 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        };

        const node = try allocator.create(Node);
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        node.* = .{ .import_spec = import_node };

        // Assertions
        // Verify the node is an import specification
        try testing.expect(node.* == .import_spec);

        const spec = node.import_spec;

        // Verify it's a using import
        try testing.expectEqual(ImportKind.Using, spec.kind);

        // Verify path has exactly one segment
        try testing.expectEqual(@as(usize, 1), spec.path.segments.items.len);

        // Verify the module name
        try testing.expectEqualStrings("MyModule", spec.path.segments.items[0].identifier);

        // Verify it has no alias
        try testing.expect(spec.alias == null);

        // Verify items list
        try testing.expect(spec.items != null);
        try testing.expectEqual(@as(usize, 4), spec.items.?.items.len);

        // Verify first two items are functions
        try testing.expect(spec.items.?.items[0].* == .function);
        try testing.expectEqualStrings("map", spec.items.?.items[0].function.name);
        try testing.expect(spec.items.?.items[0].function.alias == null);
        try testing.expect(spec.items.?.items[1].* == .function);
        try testing.expectEqualStrings("filter", spec.items.?.items[1].function.name);
        try testing.expect(spec.items.?.items[1].function.alias == null);

        // Verify last two items are types
        try testing.expect(spec.items.?.items[2].* == .type);
        try testing.expectEqualStrings("Maybe", spec.items.?.items[2].type.name);
        try testing.expect(spec.items.?.items[2].type.alias == null);
        try testing.expect(spec.items.?.items[2].type.expose_constructors == false);

        try testing.expect(spec.items.?.items[3].* == .type);
        try testing.expectEqualStrings("Either", spec.items.?.items[3].type.name);
        try testing.expect(spec.items.?.items[3].type.alias == null);
        try testing.expect(spec.items.?.items[3].type.expose_constructors == true);
    }

    {
        // Test input: open MyModule using (map as list_map)

        var segments = std.ArrayList(*UpperIdentifierNode).init(allocator);

        const module_ident = try allocator.create(UpperIdentifierNode);
        module_ident.* = .{
            .identifier = try allocator.dupe(u8, "MyModule"),
            .token = .{
                .kind = .{ .identifier = .Upper },
                .lexeme = "MyModule",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 5, .end = 13 },
                    .src = .{ .line = 1, .col = 6 },
                },
            },
        };

        try segments.append(module_ident);

        const path_node = try allocator.create(ModulePathNode);
        path_node.* = .{
            .segments = segments,
            .token = module_ident.token,
        };

        var items = std.ArrayList(*ImportItem).init(allocator);

        const map_item = try allocator.create(ImportItem);
        map_item.* = .{
            .function = .{
                .name = try allocator.dupe(u8, "map"),
                .alias = try allocator.dupe(u8, "list_map"),
            },
        };

        try items.append(map_item);

        const import_node = try allocator.create(ImportSpecNode);
        import_node.* = .{
            .path = path_node,
            .kind = .Using,
            .alias = null,
            .items = items,
            .token = .{
                .kind = .{ .keyword = .Open },
                .lexeme = "open",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 4 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        };

        const node = try allocator.create(Node);
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        node.* = .{ .import_spec = import_node };

        // Assertions
        // Verify the node is an import specification
        try testing.expect(node.* == .import_spec);

        const spec = node.import_spec;

        // Verify it's an using import
        try testing.expectEqual(ImportKind.Using, spec.kind);

        // Verify path has exactly one segment
        try testing.expectEqual(@as(usize, 1), spec.path.segments.items.len);

        // Verify the module name
        try testing.expectEqualStrings("MyModule", spec.path.segments.items[0].identifier);

        // Verify it has no alias
        try testing.expect(spec.alias == null);

        // Verify items list contains one item
        try testing.expect(spec.items != null);
        try testing.expectEqual(@as(usize, 1), spec.items.?.items.len);
        try testing.expect(spec.items.?.items[0].* == .function);
        try testing.expectEqualStrings("map", spec.items.?.items[0].function.name);
        try testing.expectEqualStrings("list_map", spec.items.?.items[0].function.alias.?);
    }

    {
        // Test input: open MyModule hiding (internal_func)

        var segments = std.ArrayList(*UpperIdentifierNode).init(allocator);

        const module_ident = try allocator.create(UpperIdentifierNode);
        module_ident.* = .{
            .identifier = try allocator.dupe(u8, "MyModule"),
            .token = .{
                .kind = .{ .identifier = .Upper },
                .lexeme = "MyModule",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 5, .end = 13 },
                    .src = .{ .line = 1, .col = 6 },
                },
            },
        };

        try segments.append(module_ident);

        const path_node = try allocator.create(ModulePathNode);
        path_node.* = .{
            .segments = segments,
            .token = module_ident.token,
        };

        var items = std.ArrayList(*ImportItem).init(allocator);

        const internal_func = try allocator.create(ImportItem);
        internal_func.* = .{
            .function = .{
                .name = try allocator.dupe(u8, "internal_func"),
                .alias = null,
            },
        };

        try items.append(internal_func);

        const import_node = try allocator.create(ImportSpecNode);
        import_node.* = .{
            .path = path_node,
            .kind = .Hiding,
            .alias = null,
            .items = items,
            .token = .{
                .kind = .{ .keyword = .Open },
                .lexeme = "open",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 4 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        };

        const node = try allocator.create(Node);
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        node.* = .{ .import_spec = import_node };

        // Assertions
        // Verify the node is an import specification
        try testing.expect(node.* == .import_spec);

        const spec = node.import_spec;

        // Verify it's a hiding import
        try testing.expectEqual(ImportKind.Hiding, spec.kind);

        // Verify path has exactly one segment
        try testing.expectEqual(@as(usize, 1), spec.path.segments.items.len);

        // Verify the module name
        try testing.expectEqualStrings("MyModule", spec.path.segments.items[0].identifier);

        // Verify it has no alias
        try testing.expect(spec.alias == null);

        // Verify items list contains one item to hide
        try testing.expect(spec.items != null);
        try testing.expectEqual(@as(usize, 1), spec.items.?.items.len);
        try testing.expect(spec.items.?.items[0].* == .function);
        try testing.expectEqualStrings("internal_func", spec.items.?.items[0].function.name);
        try testing.expect(spec.items.?.items[0].function.alias == null);
    }
}

test "[IncludeNode]" {
    // Test input: include Std.List

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    var segments = std.ArrayList(*UpperIdentifierNode).init(allocator);

    const std_ident = try allocator.create(UpperIdentifierNode);
    std_ident.* = .{
        .identifier = try allocator.dupe(u8, "Std"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Std",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 8, .end = 11 },
                .src = .{ .line = 1, .col = 9 },
            },
        },
    };

    const list_ident = try allocator.create(UpperIdentifierNode);
    list_ident.* = .{
        .identifier = try allocator.dupe(u8, "List"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "List",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 12, .end = 15 },
                .src = .{ .line = 1, .col = 13 },
            },
        },
    };

    try segments.append(std_ident);
    try segments.append(list_ident);

    const path_node = try allocator.create(ModulePathNode);
    path_node.* = .{
        .segments = segments,
        .token = std_ident.token,
    };

    const include_node = try allocator.create(IncludeNode);
    include_node.* = .{
        .path = path_node,
        .token = .{
            .kind = .{ .keyword = .Include },
            .lexeme = "include",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 7 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .include = include_node };

    // Assertions
    // Verify the node is an include declaration
    try testing.expect(node.* == .include);

    const include = node.include;

    // Verify the include path consists of exactly two segments
    try testing.expectEqual(@as(usize, 2), include.path.segments.items.len);

    // Ensure the first segment of the include path is "Std"
    try testing.expectEqualStrings("Std", include.path.segments.items[0].identifier);

    // Ensure the second segment of the include path is "List"
    try testing.expectEqualStrings("List", include.path.segments.items[1].identifier);
}

test "[FunctionDeclNode]" {
    // Test input: let add(x : Int, y : Int) -> Int = x + y

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const add_ident = try allocator.create(LowerIdentifierNode);
    add_ident.* = .{
        .identifier = try allocator.dupe(u8, "add"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "add",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 4, .end = 7 },
                .src = .{ .line = 1, .col = 5 },
            },
        },
    };

    const x_ident = try allocator.create(LowerIdentifierNode);
    x_ident.* = .{
        .identifier = try allocator.dupe(u8, "x"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "x",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 8, .end = 9 },
                .src = .{ .line = 1, .col = 9 },
            },
        },
    };

    const int_type1 = try allocator.create(UpperIdentifierNode);
    int_type1.* = .{
        .identifier = try allocator.dupe(u8, "Int"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Int",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 12, .end = 15 },
                .src = .{ .line = 1, .col = 13 },
            },
        },
    };

    const x_type_node = try allocator.create(Node);
    x_type_node.* = .{ .upper_identifier = int_type1 };

    const x_param = try allocator.create(ParamDeclNode);
    x_param.* = .{
        .name = x_ident,
        .type_annotation = x_type_node,
        .token = x_ident.token,
    };

    const y_ident = try allocator.create(LowerIdentifierNode);
    y_ident.* = .{
        .identifier = try allocator.dupe(u8, "y"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "y",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 17, .end = 18 },
                .src = .{ .line = 1, .col = 18 },
            },
        },
    };

    const int_type2 = try allocator.create(UpperIdentifierNode);
    int_type2.* = .{
        .identifier = try allocator.dupe(u8, "Int"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Int",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 21, .end = 24 },
                .src = .{ .line = 1, .col = 22 },
            },
        },
    };

    const y_type_node = try allocator.create(Node);
    y_type_node.* = .{ .upper_identifier = int_type2 };

    const y_param = try allocator.create(ParamDeclNode);
    y_param.* = .{
        .name = y_ident,
        .type_annotation = y_type_node,
        .token = y_ident.token,
    };

    var parameters = std.ArrayList(*ParamDeclNode).init(allocator);
    try parameters.append(x_param);
    try parameters.append(y_param);

    const int_type3 = try allocator.create(UpperIdentifierNode);
    int_type3.* = .{
        .identifier = try allocator.dupe(u8, "Int"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Int",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 29, .end = 32 },
                .src = .{ .line = 1, .col = 30 },
            },
        },
    };

    const return_type_node = try allocator.create(Node);
    return_type_node.* = .{ .upper_identifier = int_type3 };

    const x_body_ident = try allocator.create(LowerIdentifierNode);
    x_body_ident.* = .{
        .identifier = try allocator.dupe(u8, "x"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "x",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 35, .end = 36 },
                .src = .{ .line = 1, .col = 36 },
            },
        },
    };

    const x_body_node = try allocator.create(Node);
    x_body_node.* = .{ .lower_identifier = x_body_ident };

    const y_body_ident = try allocator.create(LowerIdentifierNode);
    y_body_ident.* = .{
        .identifier = try allocator.dupe(u8, "y"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "y",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 39, .end = 40 },
                .src = .{ .line = 1, .col = 40 },
            },
        },
    };

    const y_body_node = try allocator.create(Node);
    y_body_node.* = .{ .lower_identifier = y_body_ident };

    const arith_node = try allocator.create(ArithmeticExprNode);
    arith_node.* = .{
        .left = x_body_node,
        .right = y_body_node,
        .operator = .{
            .kind = .{ .operator = .IntAdd },
            .lexeme = "+",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 37, .end = 38 },
                .src = .{ .line = 1, .col = 38 },
            },
        },
    };

    const body_node = try allocator.create(Node);
    body_node.* = .{ .arithmetic_expr = arith_node };

    const func_decl = try allocator.create(FunctionDeclNode);
    func_decl.* = .{
        .name = add_ident,
        .parameters = parameters,
        .return_type = return_type_node,
        .value = body_node,
        .token = .{
            .kind = .{ .keyword = .Let },
            .lexeme = "let",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .function_decl = func_decl };

    // Assertions
    // Verify the node is a function declaration
    try testing.expect(node.* == .function_decl);

    const decl = node.function_decl;

    // Verify the function name
    try testing.expectEqualStrings("add", decl.name.identifier);

    // Verify the declaration has exactly two parameters
    try testing.expectEqual(@as(usize, 2), decl.parameters.items.len);

    // Check the first parameter (x : Int)
    const first_param = decl.parameters.items[0];
    try testing.expectEqualStrings("x", first_param.name.identifier);
    try testing.expect(first_param.type_annotation != null);
    try testing.expect(first_param.type_annotation.?.* == .upper_identifier);
    try testing.expectEqualStrings("Int", first_param.type_annotation.?.upper_identifier.identifier);

    // Check the second parameter (y : Int)
    const second_param = decl.parameters.items[1];
    try testing.expectEqualStrings("y", second_param.name.identifier);
    try testing.expect(second_param.type_annotation != null);
    try testing.expect(second_param.type_annotation.?.* == .upper_identifier);
    try testing.expectEqualStrings("Int", second_param.type_annotation.?.upper_identifier.identifier);

    // Check the return type (Int)
    try testing.expect(decl.return_type != null);
    try testing.expect(decl.return_type.?.* == .upper_identifier);
    try testing.expectEqualStrings("Int", decl.return_type.?.upper_identifier.identifier);

    // Check the function body (x + y)
    try testing.expect(decl.value.* == .arithmetic_expr);

    const body = decl.value.arithmetic_expr;

    // Check the operator
    try testing.expectEqual(lexer.TokenKind{ .operator = .IntAdd }, body.operator.kind);
    try testing.expectEqualStrings("+", body.operator.lexeme);

    // Check left operand
    try testing.expect(body.left.* == .lower_identifier);
    try testing.expectEqualStrings("x", body.left.lower_identifier.identifier);

    // Check right operand
    try testing.expect(body.right.* == .lower_identifier);
    try testing.expectEqualStrings("y", body.right.lower_identifier.identifier);
}

test "[ForeignFunctionDeclNode]" {
    // Test input: foreign sqrt(x : Float) -> Float = "c_sqrt"

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const float_type1 = try allocator.create(UpperIdentifierNode);
    float_type1.* = .{
        .identifier = try allocator.dupe(u8, "Float"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Float",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 17, .end = 22 },
                .src = .{ .line = 1, .col = 18 },
            },
        },
    };

    const x_type_node = try allocator.create(Node);
    x_type_node.* = .{ .upper_identifier = float_type1 };

    const x_ident = try allocator.create(LowerIdentifierNode);
    x_ident.* = .{
        .identifier = try allocator.dupe(u8, "x"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "x",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 13, .end = 14 },
                .src = .{ .line = 1, .col = 14 },
            },
        },
    };

    const x_param = try allocator.create(ParamDeclNode);
    x_param.* = .{
        .name = x_ident,
        .type_annotation = x_type_node,
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "x",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 13, .end = 14 },
                .src = .{ .line = 1, .col = 14 },
            },
        },
    };

    var parameters = std.ArrayList(*ParamDeclNode).init(allocator);
    try parameters.append(x_param);

    const float_type2 = try allocator.create(UpperIdentifierNode);
    float_type2.* = .{
        .identifier = try allocator.dupe(u8, "Float"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Float",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 26, .end = 31 },
                .src = .{ .line = 1, .col = 27 },
            },
        },
    };

    const return_type_node = try allocator.create(Node);
    return_type_node.* = .{ .upper_identifier = float_type2 };

    const external_name_node = try allocator.create(StrLiteralNode);
    external_name_node.* = .{
        .value = try allocator.dupe(u8, "c_sqrt"),
        .token = .{
            .kind = .{ .literal = .String },
            .lexeme = "\"c_sqrt\"",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 34, .end = 42 },
                .src = .{ .line = 1, .col = 35 },
            },
        },
    };

    const sqrt_ident = try allocator.create(LowerIdentifierNode);
    sqrt_ident.* = .{
        .identifier = try allocator.dupe(u8, "sqrt"),
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "sqrt",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 8, .end = 12 },
                .src = .{ .line = 1, .col = 9 },
            },
        },
    };

    const foreign_func_decl = try allocator.create(ForeignFunctionDeclNode);
    foreign_func_decl.* = .{
        .name = sqrt_ident,
        .parameters = parameters,
        .return_type = return_type_node,
        .external_name = external_name_node,
        .token = .{
            .kind = .{ .keyword = .Foreign },
            .lexeme = "foreign",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 7 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .foreign_function_decl = foreign_func_decl };

    // Assertions
    // Verify the node is a foreign function declaration
    try testing.expect(node.* == .foreign_function_decl);

    const decl = node.foreign_function_decl;

    // Verify the function name
    try testing.expectEqualStrings("sqrt", decl.name.identifier);

    // Verify the external name
    try testing.expectEqualStrings("c_sqrt", decl.external_name.value);

    // Verify the declaration has exactly one parameter
    try testing.expectEqual(@as(usize, 1), decl.parameters.items.len);

    // Check the parameter (x : Float)
    const param = decl.parameters.items[0];
    try testing.expectEqualStrings("x", param.name.identifier);
    try testing.expect(param.type_annotation != null);
    try testing.expect(param.type_annotation.?.* == .upper_identifier);
    try testing.expectEqualStrings("Float", param.type_annotation.?.upper_identifier.identifier);

    // Check the return type (Float)
    try testing.expect(decl.return_type.* == .upper_identifier);
    try testing.expectEqualStrings("Float", decl.return_type.upper_identifier.identifier);

    // Verify the token is the "foreign" keyword
    try testing.expectEqual(lexer.TokenKind{ .keyword = .Foreign }, decl.token.kind);
    try testing.expectEqualStrings("foreign", decl.token.lexeme);
}

test "[ModuleDeclNode]" {
    // Test input: module MyModule exposing (..) ... end

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    var segments = std.ArrayList(*UpperIdentifierNode).init(allocator);

    const module_ident = try allocator.create(UpperIdentifierNode);
    module_ident.* = .{
        .identifier = try allocator.dupe(u8, "MyModule"),
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "MyModule",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 6, .end = 14 },
                .src = .{ .line = 1, .col = 7 },
            },
        },
    };

    try segments.append(module_ident);

    const path_node = try allocator.create(ModulePathNode);
    path_node.* = .{
        .segments = segments,
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "MyModule",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 7, .end = 15 },
                .src = .{ .line = 1, .col = 8 },
            },
        },
    };

    const export_node = try allocator.create(ExportSpecNode);
    export_node.* = .{
        .exposing_all = true,
        .items = null,
        .token = .{
            .kind = .{ .keyword = .Exposing },
            .lexeme = "exposing",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 16, .end = 24 },
                .src = .{ .line = 1, .col = 17 },
            },
        },
    };

    // Create a dummy declaration
    const comment_node = try allocator.create(CommentNode);
    comment_node.* = .{
        .text = try allocator.dupe(u8, "dummy declaration"),
        .token = .{
            .kind = .{ .comment = .Regular },
            .lexeme = "#",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    var declarations = std.ArrayList(*Node).init(allocator);

    const comment_decl = try allocator.create(Node);
    comment_decl.* = .{ .comment = comment_node };

    try declarations.append(comment_decl);

    const module_node = try allocator.create(ModuleDeclNode);
    module_node.* = .{
        .path = path_node,
        .exports = export_node,
        .declarations = declarations,
        .token = .{
            .kind = .{ .keyword = .Module },
            .lexeme = "module",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 6 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .module_decl = module_node };

    // Assertions
    // Verify the node is a module declaration
    try testing.expect(node.* == .module_decl);

    // Verify module path
    try testing.expectEqual(@as(usize, 1), node.module_decl.path.segments.items.len);
    try testing.expectEqualStrings("MyModule", node.module_decl.path.segments.items[0].identifier);

    // Verify exports
    try testing.expect(node.module_decl.exports.exposing_all);
    try testing.expect(node.module_decl.exports.items == null);

    // Verify declarations
    try testing.expectEqual(@as(usize, 1), node.module_decl.declarations.items.len);
    try testing.expect(node.module_decl.declarations.items[0].* == .comment);
}

test "[ProgramNode]" {
    // Test input: A program with a single declaration

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const comment_node = try allocator.create(CommentNode);
    comment_node.* = .{
        .text = try allocator.dupe(u8, "program statement"),
        .token = .{
            .kind = .{ .comment = .Regular },
            .lexeme = "#",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    var statements = std.ArrayList(*Node).init(allocator);

    const stmt = try allocator.create(Node);
    stmt.* = .{ .comment = comment_node };

    try statements.append(stmt);

    const program_node = try allocator.create(ProgramNode);
    program_node.* = .{
        .statements = statements,
        .token = .{
            .kind = .{ .keyword = .Module },
            .lexeme = "module",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 6 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    };

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .program = program_node };

    // Assertions
    // Verify the node is a program
    try testing.expect(node.* == .program);

    // Verify program contains one statement
    try testing.expectEqual(@as(usize, 1), node.program.statements.items.len);
    try testing.expect(node.program.statements.items[0].* == .comment);
}
