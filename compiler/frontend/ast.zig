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

    pub fn init(allocator: std.mem.Allocator, content: []const u8, token: lexer.Token) !*CommentNode {
        const node = try allocator.create(CommentNode);

        node.* = .{
            .content = try allocator.dupe(u8, content),
            .token = token,
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

    pub fn init(allocator: std.mem.Allocator, content: []const u8, token: lexer.Token) !*DocCommentNode {
        const node = try allocator.create(DocCommentNode);

        node.* = .{
            .content = try allocator.dupe(u8, content),
            .token = token,
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

    pub fn init(value: i64, token: lexer.Token) IntLiteralNode {
        return .{
            .value = value,
            .token = token,
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

    pub fn init(value: f64, token: lexer.Token) FloatLiteralNode {
        return .{
            .value = value,
            .token = token,
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

    pub fn init(value: u21, token: lexer.Token) CharLiteralNode {
        return .{
            .value = value,
            .token = token,
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

    pub fn init(allocator: std.mem.Allocator, value: []const u8, token: lexer.Token) !*StrLiteralNode {
        const node = try allocator.create(StrLiteralNode);

        node.* = .{
            .value = try allocator.dupe(u8, value),
            .token = token,
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

    pub fn init(allocator: std.mem.Allocator, value: []const u8, token: lexer.Token) !*MultilineStrLiteralNode {
        const node = try allocator.create(MultilineStrLiteralNode);

        node.* = .{
            .value = try allocator.dupe(u8, value),
            .token = token,
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

    pub fn init(allocator: std.mem.Allocator, name: []const u8, token: lexer.Token) !*LowerIdentifierNode {
        const node = try allocator.create(LowerIdentifierNode);

        node.* = .{
            .name = try allocator.dupe(u8, name),
            .token = token,
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

    pub fn init(allocator: std.mem.Allocator, name: []const u8, token: lexer.Token) !*UpperIdentifierNode {
        const node = try allocator.create(UpperIdentifierNode);

        node.* = .{
            .name = try allocator.dupe(u8, name),
            .token = token,
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

    pub fn init(allocator: std.mem.Allocator, elements: std.ArrayList(*Node), token: lexer.Token) !*ListNode {
        const node = try allocator.create(ListNode);

        node.* = .{
            .elements = elements,
            .token = token,
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

    pub fn init(allocator: std.mem.Allocator, elements: std.ArrayList(*Node), token: lexer.Token) !*TupleNode {
        const node = try allocator.create(TupleNode);

        node.* = .{
            .elements = elements,
            .token = token,
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

    pub fn init(allocator: std.mem.Allocator, operand: *Node, operator: lexer.Token) !*UnaryExprNode {
        const node = try allocator.create(UnaryExprNode);

        node.* = .{
            .operand = operand,
            .operator = operator,
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

    pub fn init(allocator: std.mem.Allocator, left: *Node, right: *Node, operator: lexer.Token) !*BinaryOp {
        const node = try allocator.create(BinaryOp);

        node.* = .{
            .left = left,
            .right = right,
            .operator = operator,
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
    },
    variable: struct {
        /// The name of the variable to bind.
        name: []const u8,

        /// The token representing the variable.
        token: lexer.Token,
    },
    constructor: struct {
        /// The name of the constructor.
        name: []const u8,

        /// Array of patterns for matching constructor arguments.
        args: std.ArrayList(*PatternNode),

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

    pub fn init(allocator: std.mem.Allocator, param_types: std.ArrayList(*Node), token: lexer.Token) !*FunctionTypeNode {
        const node = try allocator.create(FunctionTypeNode);

        node.* = .{
            .param_types = param_types,
            .token = token,
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
    params: std.ArrayList([]const u8),

    /// The AST node representing the function body expression.
    body: *Node,

    /// The token representing the start of this declaration.
    token: lexer.Token,

    pub fn init(allocator: std.mem.Allocator, params: std.ArrayList([]const u8), body: *Node, token: lexer.Token) !*LambdaExprNode {
        const node = try allocator.create(LambdaExprNode);

        node.* = .{
            .params = params,
            .body = body,
            .token = token,
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

    pub fn init(allocator: std.mem.Allocator, function: *Node, argument: *Node, token: lexer.Token) !*FuncApplicationNode {
        const node = try allocator.create(FuncApplicationNode);

        node.* = .{
            .function = function,
            .argument = argument,
            .token = token,
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
};

/// Represents a field in a record type with a name and type.
pub const RecordFieldNode = struct {
    /// The name of the field.
    name: []const u8,

    /// The AST node representing the field's type.
    type: *Node,

    /// The token representing this field declaration.
    token: lexer.Token,
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
};

pub const ExportItem = struct {
    /// The name of the item being exported.
    name: []const u8,

    /// Whether to expose constructors for exported types.
    expose_constructors: bool,

    /// The token representing the start of this declaration.
    token: lexer.Token,
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
};

/// The root AST node containing a sequence of top-level declarations like
/// function definitions, type declarations, imports, and module definitions.
pub const ProgramNode = struct {
    /// Array of AST nodes representing top-level declarations.
    statements: std.ArrayList(*Node),

    /// The token representing the start of this declaration.
    token: lexer.Token,
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
    pattern: PatternNode,
    match_expr: MatchExprNode,

    // Functions and Applications
    function_type: *FunctionTypeNode,
    lambda_expr: *LambdaExprNode,
    function_application: *FuncApplicationNode,

    // Advanced Expressions
    cons_expr: ConsExprNode,
    str_concat_expr: StrConcatExprNode,
    list_concat_expr: ListConcatExprNode,
    composition_expr: CompositionExprNode,
    pipe_expr: PipeExprNode,

    // Control Flow
    if_then_else_stmt: IfThenElseStmtNode,

    // Type System
    typed_hole: TypedHoleNode,
    type_application: TypeApplicationNode,
    type_alias: TypeAliasNode,
    variant_type: VariantTypeNode,
    record_type: RecordTypeNode,

    // Module System
    module_path: ModulePathNode,
    export_spec: ExportSpecNode,
    import_spec: ImportSpecNode,
    include: IncludeNode,

    // Top-Level Declarations
    function_decl: FunctionDeclNode,
    foreign_function_decl: ForeignFunctionDeclNode,
    module_decl: ModuleDeclNode,
    program: ProgramNode,

    /// Cleans up resources associated with this node.
    ///
    /// - `allocator`: The memory allocator used to deallocate child nodes.
    ///
    /// Recursively deinitializes any child nodes that this node owns,
    /// ensuring no memory leaks occur.
    pub fn deinit(self: *Node, allocator: std.mem.Allocator) void {
        switch (self.*) {
            // Basic Literals
            .comment => |comment| comment.deinit(allocator),
            .doc_comment => |comment| comment.deinit(allocator),
            .int_literal => {}, // do nothing
            .float_literal => {}, // do nothing
            .char_literal => {}, // do nothing
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
            .pattern => |*pat| {
                deinitPattern(allocator, pat);
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

            // Functions and Applications
            .function_type => |ftype| ftype.deinit(allocator),
            .lambda_expr => |expr| expr.deinit(allocator),
            .function_application => |expr| expr.deinit(allocator),

            // Advanced Expressions
            .cons_expr => |*expr| {
                expr.head.deinit(allocator);
                expr.tail.deinit(allocator);
                allocator.destroy(expr.head);
                allocator.destroy(expr.tail);
            },
            .str_concat_expr => |*expr| {
                expr.left.deinit(allocator);
                expr.right.deinit(allocator);
                allocator.destroy(expr.left);
                allocator.destroy(expr.right);
            },
            .list_concat_expr => |*expr| {
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
            .pipe_expr => |*expr| {
                expr.value.deinit(allocator);
                expr.func.deinit(allocator);
                allocator.destroy(expr.value);
                allocator.destroy(expr.func);
            },

            // Control Flow
            .if_then_else_stmt => |*stmt| {
                stmt.condition.deinit(allocator);
                stmt.else_branch.deinit(allocator);
                stmt.then_branch.deinit(allocator);
                allocator.destroy(stmt.condition);
                allocator.destroy(stmt.else_branch);
                allocator.destroy(stmt.then_branch);
            },

            // Type System
            .typed_hole => {
                // do nothing
            },
            .type_application => |*app| {
                app.base.deinit(allocator);
                allocator.destroy(app.base);

                for (app.args.items) |arg| {
                    arg.deinit(allocator);
                    allocator.destroy(arg);
                }

                app.args.deinit();
            },
            .type_alias => |*alias| {
                allocator.free(alias.name);

                for (alias.type_params.items) |param| {
                    allocator.free(param);
                }

                alias.type_params.deinit();

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
            .record_type => |*rtype| {
                allocator.free(rtype.name);

                for (rtype.type_params.items) |param| {
                    allocator.free(param);
                }

                rtype.type_params.deinit();

                for (rtype.fields.items) |field| {
                    allocator.free(field.name);
                    field.type.deinit(allocator);
                    allocator.destroy(field.type);
                }

                rtype.fields.deinit();
            },

            // Module System
            .module_path => |*path| {
                for (path.segments.items) |segment| {
                    allocator.free(segment);
                }

                path.segments.deinit();
            },
            .export_spec => |*spec| {
                if (spec.items) |*items| {
                    for (items.items) |item| {
                        allocator.free(item.name);
                    }

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
                            .function => |f| {
                                allocator.free(f.name);

                                if (f.alias) |alias| {
                                    allocator.free(alias);
                                }
                            },
                            .operator => |op| {
                                allocator.free(op.symbol);

                                if (op.alias) |alias| {
                                    allocator.free(alias);
                                }
                            },
                            .type => |t| {
                                allocator.free(t.name);

                                if (t.alias) |alias| {
                                    allocator.free(alias);
                                }
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

            // Top-Level Declarations
            .function_decl => |*decl| {
                if (decl.type_annotation) |type_annotation| {
                    type_annotation.deinit(allocator);
                    allocator.destroy(type_annotation);
                }

                decl.value.deinit(allocator);
                allocator.destroy(decl.value);
            },
            .foreign_function_decl => |*decl| {
                decl.type_annotation.deinit(allocator);
                allocator.destroy(decl.type_annotation);
                allocator.free(decl.external_name);
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
            .program => |*prog| {
                for (prog.statements.items) |stmt| {
                    stmt.deinit(allocator);
                    allocator.destroy(stmt);
                }

                prog.statements.deinit();
                allocator.destroy(self);
            },
        }
    }

    fn deinitPattern(allocator: std.mem.Allocator, pattern: *PatternNode) void {
        switch (pattern.*) {
            .wildcard,
            .int_literal,
            .float_literal,
            .char_literal,
            .empty_list,
            .variable,
            => {
                // do nothing
            },
            .string_literal => |lit| {
                allocator.free(lit.value);
            },
            .constructor => |*con| {
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
    const comment_node = try CommentNode.init(
        allocator,
        content,
        lexer.Token{
            .kind = lexer.TokenKind{ .comment = .Regular },
            .lexeme = "#",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 18 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    );

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
    const comment_node = try DocCommentNode.init(
        allocator,
        content,
        lexer.Token{
            .kind = lexer.TokenKind{ .comment = .Doc },
            .lexeme = "##",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 23 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    );

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
    const int_node = IntLiteralNode.init(
        value,
        lexer.Token{
            .kind = lexer.TokenKind{ .literal = .Int },
            .lexeme = "42",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    );

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
    const float_node = FloatLiteralNode.init(
        value,
        lexer.Token{
            .kind = lexer.TokenKind{ .literal = .Float },
            .lexeme = "42.0",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 3 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    );

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
    const char_node = CharLiteralNode.init(
        value,
        lexer.Token{
            .kind = lexer.TokenKind{ .literal = .Char },
            .lexeme = "\'",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    );

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
    const literal_node = try StrLiteralNode.init(
        allocator,
        value,
        lexer.Token{
            .kind = lexer.TokenKind{ .literal = .String },
            .lexeme = "\"",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    );

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
    const literal_node = try MultilineStrLiteralNode.init(
        allocator,
        value,
        lexer.Token{
            .kind = lexer.TokenKind{ .literal = .MultilineString },
            .lexeme = "\"\"\"",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 38 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    );

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
    const ident_node = try LowerIdentifierNode.init(
        allocator,
        name,
        lexer.Token{
            .kind = lexer.TokenKind{ .identifier = .Lower },
            .lexeme = name,
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 10 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    );

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
    const ident_node = try UpperIdentifierNode.init(
        allocator,
        name,
        lexer.Token{
            .kind = lexer.TokenKind{ .identifier = .Upper },
            .lexeme = name,
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 8 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    );

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
        .int_literal = .{
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
        },
    };

    const elem2 = try allocator.create(Node);
    elem2.* = .{
        .int_literal = .{
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
        },
    };

    try elements.append(elem1);
    try elements.append(elem2);

    const list_node = try ListNode.init(
        allocator,
        elements,
        lexer.Token{
            .kind = lexer.TokenKind{ .delimiter = .LeftBracket },
            .lexeme = "[",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 6 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    );

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
        .int_literal = .{
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
        },
    };

    const str_literal = try StrLiteralNode.init(
        allocator,
        "hello",
        lexer.Token{
            .kind = lexer.TokenKind{ .literal = .String },
            .lexeme = "\"hello\"",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 4, .end = 11 },
                .src = .{ .line = 1, .col = 5 },
            },
        },
    );

    const second = try allocator.create(Node);
    second.* = .{ .str_literal = str_literal };

    try elements.append(first);
    try elements.append(second);

    const tuple_node = try TupleNode.init(
        allocator,
        elements,
        lexer.Token{
            .kind = lexer.TokenKind{ .delimiter = .LeftParen },
            .lexeme = "(",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    );

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
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .literal = .Int },
                .lexeme = "42",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 1, .end = 3 },
                    .src = .{ .line = 1, .col = 2 },
                },
            },
        },
    };

    const unary_node = try UnaryExprNode.init(
        allocator,
        operand,
        lexer.Token{
            .kind = lexer.TokenKind{ .operator = .IntSub },
            .lexeme = "-",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    );

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

    const arithmetic_node = try ArithmeticExprNode.init(
        allocator,
        left,
        right,
        lexer.Token{
            .kind = .{ .operator = .IntMul },
            .lexeme = "*",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 2, .end = 3 },
                .src = .{ .line = 1, .col = 3 },
            },
        },
    );

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
    const a_node = try LowerIdentifierNode.init(
        allocator,
        "a",
        lexer.Token{
            .kind = .{ .identifier = .Lower },
            .lexeme = "a",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    );

    const b_node = try LowerIdentifierNode.init(
        allocator,
        "b",
        lexer.Token{
            .kind = .{ .identifier = .Lower },
            .lexeme = "b",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 5, .end = 6 },
                .src = .{ .line = 1, .col = 6 },
            },
        },
    );

    const left = try allocator.create(Node);
    left.* = .{ .lower_identifier = a_node };

    const right = try allocator.create(Node);
    right.* = .{ .lower_identifier = b_node };

    const logical_node = try LogicalExprNode.init(
        allocator,
        left,
        right,
        lexer.Token{
            .kind = .{ .operator = .LogicalAnd },
            .lexeme = "&&",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 2, .end = 4 },
                .src = .{ .line = 1, .col = 3 },
            },
        },
    );

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
    const x_node = try LowerIdentifierNode.init(
        allocator,
        "x",
        lexer.Token{
            .kind = .{ .identifier = .Lower },
            .lexeme = "x",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    );

    const y_node = try LowerIdentifierNode.init(
        allocator,
        "y",
        lexer.Token{
            .kind = .{ .identifier = .Lower },
            .lexeme = "y",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 5, .end = 6 },
                .src = .{ .line = 1, .col = 6 },
            },
        },
    );

    const left = try allocator.create(Node);
    left.* = .{ .lower_identifier = x_node };

    const right = try allocator.create(Node);
    right.* = .{ .lower_identifier = y_node };

    const comparison_node = try ComparisonExprNode.init(
        allocator,
        left,
        right,
        lexer.Token{
            .kind = .{ .operator = .LessThanEqual },
            .lexeme = "<=",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 2, .end = 4 },
                .src = .{ .line = 1, .col = 3 },
            },
        },
    );

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

test "[MatchExprNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        // Test basic constructor pattern matching
        // Test input: match opt on | Some x => x | None => 0

        // Action
        const value = try allocator.create(Node);
        value.* = .{
            .upper_identifier = .{
                .name = "Some",
                .token = .{
                    .kind = .{ .identifier = .Upper },
                    .lexeme = "Some",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 4 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        // Create patterns
        var cases = std.ArrayList(MatchCase).init(allocator);

        // Case 1: Some x => x
        const some_pattern = try allocator.create(PatternNode);
        var some_args = std.ArrayList(*PatternNode).init(allocator);

        const var_pattern = try allocator.create(PatternNode);
        var_pattern.* = .{
            .variable = .{
                .name = "x",
                .token = .{
                    .kind = .{ .identifier = .Lower },
                    .lexeme = "x",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 1 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        try some_args.append(var_pattern);

        some_pattern.* = .{
            .constructor = .{
                .name = "Some",
                .args = some_args,
                .token = .{
                    .kind = .{ .identifier = .Upper },
                    .lexeme = "Some",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 4 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        const some_expr = try allocator.create(Node);
        some_expr.* = .{
            .lower_identifier = .{
                .name = "x",
                .token = .{
                    .kind = .{ .identifier = .Lower },
                    .lexeme = "x",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 1 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        try cases.append(.{
            .pattern = some_pattern,
            .expression = some_expr,
            .guard = null,
            .token = .{
                .kind = .{ .symbol = .Pipe },
                .lexeme = "|",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        });

        // Case 2: None => 0
        const none_pattern = try allocator.create(PatternNode);
        none_pattern.* = .{
            .constructor = .{
                .name = "None",
                .args = std.ArrayList(*PatternNode).init(allocator),
                .token = .{
                    .kind = .{ .identifier = .Upper },
                    .lexeme = "None",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 4 },
                        .src = .{ .line = 1, .col = 1 },
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
                        .span = .{ .start = 0, .end = 1 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        try cases.append(.{
            .pattern = none_pattern,
            .expression = none_expr,
            .guard = null,
            .token = .{
                .kind = .{ .symbol = .Pipe },
                .lexeme = "|",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        });

        const node = try allocator.create(Node);
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        node.* = .{
            .match_expr = .{
                .value = value,
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
            },
        };

        // Assertions
        // Verify the node is a match expression
        try testing.expect(node.* == .match_expr);

        const match = node.match_expr;

        // Verify the match expression has exactly two cases
        try testing.expectEqual(@as(usize, 2), match.cases.items.len);

        // Ensure the constructor name for the first case is "Some"
        try testing.expect(match.value.* == .upper_identifier);
        try testing.expectEqualStrings("Some", match.cases.items[0].pattern.constructor.name);

        // Check the argument for the "Some" constructor is a variable named "x"
        try testing.expectEqualStrings("x", match.cases.items[0].pattern.constructor.args.items[0].variable.name);

        // Ensure the constructor name for the second case is "None"
        try testing.expectEqualStrings("None", match.cases.items[1].pattern.constructor.name);

        // Verify the "None" constructor in the second case has no arguments
        try testing.expectEqual(@as(usize, 0), match.cases.items[1].pattern.constructor.args.items.len);
    }

    {
        // Test list pattern with cons
        // Test input: match list on | head :: tail => head | [] => 0

        // Action
        // Value to match on (a list variable)
        const value = try allocator.create(Node);
        value.* = .{
            .lower_identifier = .{
                .name = "list",
                .token = .{
                    .kind = .{ .identifier = .Lower },
                    .lexeme = "list",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 4 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        var cases = std.ArrayList(MatchCase).init(allocator);

        // Case 1: head :: tail => head
        const head_pattern = try allocator.create(PatternNode);
        head_pattern.* = .{
            .variable = .{
                .name = "head",
                .token = .{
                    .kind = .{ .identifier = .Lower },
                    .lexeme = "head",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 4 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        const tail_pattern = try allocator.create(PatternNode);
        tail_pattern.* = .{
            .variable = .{
                .name = "tail",
                .token = .{
                    .kind = .{ .identifier = .Lower },
                    .lexeme = "tail",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 4 },
                        .src = .{ .line = 1, .col = 1 },
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
                        .span = .{ .start = 0, .end = 2 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        const head_expr = try allocator.create(Node);
        head_expr.* = .{
            .lower_identifier = .{
                .name = "head",
                .token = .{
                    .kind = .{ .identifier = .Lower },
                    .lexeme = "head",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 4 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        try cases.append(.{
            .pattern = cons_pattern,
            .expression = head_expr,
            .guard = null,
            .token = .{
                .kind = .{ .symbol = .Pipe },
                .lexeme = "|",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        });

        // Case 2: [] => 0
        const empty_pattern = try allocator.create(PatternNode);
        empty_pattern.* = .{
            .empty_list = .{
                .token = .{
                    .kind = .{ .delimiter = .LeftBracket },
                    .lexeme = "[]",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 2 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        const zero_expr = try allocator.create(Node);
        zero_expr.* = .{
            .int_literal = .{
                .value = 0,
                .token = .{
                    .kind = .{ .literal = .Int },
                    .lexeme = "0",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 1 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        try cases.append(.{
            .pattern = empty_pattern,
            .expression = zero_expr,
            .guard = null,
            .token = .{
                .kind = .{ .symbol = .Pipe },
                .lexeme = "|",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        });

        const node = try allocator.create(Node);
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        node.* = .{
            .match_expr = .{
                .value = value,
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
            },
        };

        // Assertions
        // Verify the node is a match expression
        try testing.expect(node.* == .match_expr);

        const match = node.match_expr;

        // Verify the match expression has exactly two cases
        try testing.expectEqual(@as(usize, 2), match.cases.items.len);

        // Test the first case (cons pattern)
        const cons_case = match.cases.items[0];

        // Verify the pattern in the first case is a cons pattern (head :: tail)
        try testing.expect(cons_case.pattern.* == .cons);

        // Ensure the name of the head variable in the cons pattern is "head"
        try testing.expectEqualStrings("head", cons_case.pattern.cons.head.variable.name);

        // Ensure the name of the tail variable in the cons pattern is "tail"
        try testing.expectEqualStrings("tail", cons_case.pattern.cons.tail.variable.name);

        // Test the second case (empty list pattern)
        const empty_case = match.cases.items[1];

        // Verify the pattern in the second case is an empty list
        try testing.expect(empty_case.pattern.* == .empty_list);
    }

    {
        // Test guards
        // Test input: match x on | n when n > 0 => "positive" | n => "non-positive"

        // Action
        const value = try allocator.create(Node);
        value.* = .{
            .lower_identifier = .{
                .name = "x",
                .token = .{
                    .kind = .{ .identifier = .Lower },
                    .lexeme = "x",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 1 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        var cases = std.ArrayList(MatchCase).init(allocator);

        // Case 1: n when n > 0 => "positive"
        const n_pattern = try allocator.create(PatternNode);
        n_pattern.* = .{
            .variable = .{
                .name = "n",
                .token = .{
                    .kind = .{ .identifier = .Lower },
                    .lexeme = "n",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 1 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        // Build n > 0 guard expression
        const n_ref = try allocator.create(Node);
        n_ref.* = .{
            .lower_identifier = .{
                .name = "n",
                .token = .{
                    .kind = .{ .identifier = .Lower },
                    .lexeme = "n",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 1 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        const zero = try allocator.create(Node);
        zero.* = .{
            .int_literal = .{
                .value = 0,
                .token = .{
                    .kind = .{ .literal = .Int },
                    .lexeme = "0",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 1 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        const guard_expr = try allocator.create(Node);
        guard_expr.* = .{
            .comparison_expr = .{
                .left = n_ref,
                .operator = .{
                    .kind = .{ .operator = .GreaterThan },
                    .lexeme = ">",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 1 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
                .right = zero,
            },
        };

        const guard = try allocator.create(GuardNode);
        guard.* = .{
            .condition = guard_expr,
            .token = .{
                .kind = .{ .keyword = .When },
                .lexeme = "when",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 4 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        };

        const positive_expr = try allocator.create(Node);
        positive_expr.* = .{
            .str_literal = .{
                .value = try allocator.dupe(u8, "positive"),
                .token = .{
                    .kind = .{ .literal = .String },
                    .lexeme = "\"positive\"",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 10 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        try cases.append(.{
            .pattern = n_pattern,
            .expression = positive_expr,
            .guard = guard,
            .token = .{
                .kind = .{ .symbol = .Pipe },
                .lexeme = "|",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        });

        // Case 2: n => "non-positive"
        const n2_pattern = try allocator.create(PatternNode);
        n2_pattern.* = .{
            .variable = .{
                .name = "n",
                .token = .{
                    .kind = .{ .identifier = .Lower },
                    .lexeme = "n",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 1 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        const non_positive_expr = try allocator.create(Node);
        non_positive_expr.* = .{
            .str_literal = .{
                .value = try allocator.dupe(u8, "non-positive"),
                .token = .{
                    .kind = .{ .literal = .String },
                    .lexeme = "\"non-positive\"",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 14 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        try cases.append(.{
            .pattern = n2_pattern,
            .expression = non_positive_expr,
            .guard = null,
            .token = .{
                .kind = .{ .symbol = .Pipe },
                .lexeme = "|",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        });

        const node = try allocator.create(Node);
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        node.* = .{
            .match_expr = .{
                .value = value,
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
            },
        };

        // Assertions
        // Verify the node is a match expression
        try testing.expect(node.* == .match_expr);

        const match = node.match_expr;

        // Verify the match expression has exactly two cases
        try testing.expectEqual(@as(usize, 2), match.cases.items.len);

        // Test the first case (guarded case)
        const guarded_case = match.cases.items[0];

        // Verify the pattern in the guarded case is a variable
        try testing.expect(guarded_case.pattern.* == .variable);

        // Verify the name of the variable
        try testing.expectEqualStrings("n", guarded_case.pattern.variable.name);

        // Ensure the guarded case has a guard condition
        try testing.expect(guarded_case.guard != null);

        // Verify the expression in the guarded case is the string literal "positive"
        try testing.expectEqualStrings("positive", guarded_case.expression.str_literal.value);

        // Test the second case (catch-all case)
        const catchall_case = match.cases.items[1];

        // Verify the pattern in the catch-all case is a variable
        try testing.expect(catchall_case.pattern.* == .variable);

        // Ensure the name of the variable in the pattern is "n"
        try testing.expectEqualStrings("n", catchall_case.pattern.variable.name);

        // Ensure the catch-all case does not have a guard condition
        try testing.expect(catchall_case.guard == null);

        // Verify the expression in the catch-all case is the string literal "non-positive"
        try testing.expectEqualStrings("non-positive", catchall_case.expression.str_literal.value);
    }
}

test "[FunctionTypeNode]" {
    // Test input: : Int -> Int -> Int

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    var param_types = std.ArrayList(*Node).init(allocator);

    const int_type1 = try UpperIdentifierNode.init(
        allocator,
        "Int",
        lexer.Token{
            .kind = lexer.TokenKind{ .identifier = .Upper },
            .lexeme = "Int",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 2, .end = 5 },
                .src = .{ .line = 1, .col = 3 },
            },
        },
    );

    const int_type2 = try UpperIdentifierNode.init(
        allocator,
        "Int",
        lexer.Token{
            .kind = lexer.TokenKind{ .identifier = .Upper },
            .lexeme = "Int",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 9, .end = 12 },
                .src = .{ .line = 1, .col = 10 },
            },
        },
    );

    const int_type3 = try UpperIdentifierNode.init(
        allocator,
        "Int",
        lexer.Token{
            .kind = lexer.TokenKind{ .identifier = .Upper },
            .lexeme = "Int",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 16, .end = 19 },
                .src = .{ .line = 1, .col = 17 },
            },
        },
    );

    try param_types.append(int_type1);
    try param_types.append(int_type2);
    try param_types.append(int_type3);

    const ftype_node = try FunctionTypeNode.init(
        allocator,
        param_types,
        lexer.Token{
            .kind = lexer.TokenKind{ .delimiter = .Colon },
            .lexeme = ":",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 1 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    );

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{ .function_type = ftype_node };

    // Assertions
    // Verify the node is a function type
    try testing.expect(node.* == .function_type);

    const ftype = node.function_type;

    // Check the delimiter in the function type is a colon (:)
    try testing.expectEqual(lexer.TokenKind{ .delimiter = .Colon }, ftype.token.kind);

    // Verify the lexeme
    try testing.expectEqualStrings(":", ftype.token.lexeme);

    // Check the function type has exactly three parameter types
    try testing.expectEqual(@as(usize, 3), ftype.param_types.items.len);

    for (ftype.param_types.items) |type_node| {
        // Verify the parameter type is an upper-case identifier
        try testing.expect(type_node.* == .upper_identifier);

        // Ensure the token kind of the parameter type is an upper-case identifier
        try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, type_node.upper_identifier.token.kind);

        // Check the name of the parameter type
        try testing.expectEqualStrings("Int", type_node.upper_identifier.name);

        // Check the lexeme for the parameter type
        try testing.expectEqualStrings("Int", type_node.upper_identifier.token.lexeme);
    }

    // Test specific positions of each Int
    const first_int = ftype.param_types.items[0];
    try testing.expectEqual(@as(usize, 2), first_int.upper_identifier.token.loc.span.start);
    try testing.expectEqual(@as(usize, 3), first_int.upper_identifier.token.loc.src.col);

    const second_int = ftype.param_types.items[1];
    try testing.expectEqual(@as(usize, 9), second_int.upper_identifier.token.loc.span.start);
    try testing.expectEqual(@as(usize, 10), second_int.upper_identifier.token.loc.src.col);

    const third_int = ftype.param_types.items[2];
    try testing.expectEqual(@as(usize, 16), third_int.upper_identifier.token.loc.span.start);
    try testing.expectEqual(@as(usize, 17), third_int.upper_identifier.token.loc.src.col);
}

test "[LambdaExprNode]" {
    // Test input: \x y => x + y

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    var params = std.ArrayList([]const u8).init(allocator);
    try params.append("x");
    try params.append("y");

    const left = try LowerIdentifierNode.init(
        allocator,
        "x",
        lexer.Token{
            .kind = lexer.TokenKind{ .identifier = .Lower },
            .lexeme = "x",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 7, .end = 8 },
                .src = .{ .line = 1, .col = 8 },
            },
        },
    );

    const right = try allocator.create(Node);
    right.* = .{
        .lower_identifier = .{
            .name = "y",
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .identifier = .Lower },
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
            .right = right,
            .operator = lexer.Token{
                .kind = lexer.TokenKind{ .operator = .IntAdd },
                .lexeme = "+",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 8, .end = 9 },
                    .src = .{ .line = 1, .col = 9 },
                },
            },
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
                .kind = lexer.TokenKind{ .operator = .Lambda },
                .lexeme = "\\",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    // Assertions
    try testing.expect(lambda.* == .lambda_expr);

    const expr = lambda.lambda_expr;

    // Verify that the lambda expression has exactly two parameters
    try testing.expectEqual(@as(usize, 2), expr.params.items.len);

    // Ensure the first parameter is named "x"
    try testing.expectEqualStrings("x", expr.params.items[0]);

    // Ensure the second parameter is named "y"
    try testing.expectEqualStrings("y", expr.params.items[1]);

    // Verify the token kind matches
    try testing.expectEqual(lexer.TokenKind{ .operator = .Lambda }, expr.token.kind);

    // Verify the token lexeme matches
    try testing.expectEqualStrings("\\", expr.token.lexeme);

    const lambda_body = expr.body.arithmetic_expr;

    // Verify the operator in the arithmetic expression is an integer addition operator (+)
    try testing.expectEqual(lexer.TokenKind{ .operator = .IntAdd }, lambda_body.operator.kind);

    // Ensure the lexeme for the addition operator is "+"
    try testing.expectEqualStrings("+", lambda_body.operator.lexeme);

    // Check the left operand is a lower-case identifier
    try testing.expect(lambda_body.left.* == .lower_identifier);

    // Verify the name of the left identifier is "x"
    try testing.expectEqualStrings("x", lambda_body.left.lower_identifier.name);

    // Ensure the token kind of the left identifier is a lower-case identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, lambda_body.left.lower_identifier.token.kind);
}

test "[FuncApplicationNode]" {
    // Test input: not True

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const func = try allocator.create(Node);
    func.* = .{
        .lower_identifier = .{
            .name = "not",
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .identifier = .Lower },
                .lexeme = "not",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 3 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    const arg = try allocator.create(Node);
    arg.* = .{
        .upper_identifier = .{
            .name = "True",
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .identifier = .Upper },
                .lexeme = "True",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 4, .end = 8 },
                    .src = .{ .line = 1, .col = 5 },
                },
            },
        },
    };

    const func_app = try allocator.create(Node);
    defer {
        func_app.deinit(allocator);
        allocator.destroy(func_app);
    }

    func_app.* = .{
        .function_application = .{
            .function = func,
            .argument = arg,
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .identifier = .Lower },
                .lexeme = "not",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 3 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    // Assertions
    const app = func_app.function_application;

    // Verify that the function is a lower identifier named "not"
    try testing.expect(app.function.* == .lower_identifier);
    try testing.expectEqualStrings("not", app.function.lower_identifier.name);
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, app.function.lower_identifier.token.kind);
    try testing.expectEqualStrings("not", app.function.lower_identifier.token.lexeme);

    // Verify that the argument is an upper identifier named "True"
    try testing.expect(app.argument.* == .upper_identifier);
    try testing.expectEqualStrings("True", app.argument.upper_identifier.name);
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, app.argument.upper_identifier.token.kind);
    try testing.expectEqualStrings("True", app.argument.upper_identifier.token.lexeme);

    // Verify the function application token matches the function's token
    try testing.expectEqual(app.token.kind, app.function.lower_identifier.token.kind);
    try testing.expectEqualStrings(app.token.lexeme, app.function.lower_identifier.token.lexeme);
}

test "[ConsExprNode]" {
    // Test input: 1 :: [2, 3]

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const head = try allocator.create(Node);
    head.* = .{
        .int_literal = .{
            .value = 1,
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .literal = .Int },
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
                .kind = lexer.TokenKind{ .literal = .Int },
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
                .kind = lexer.TokenKind{ .literal = .Int },
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
                .kind = lexer.TokenKind{ .delimiter = .LeftBracket },
                .lexeme = "[",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 5, .end = 6 },
                    .src = .{ .line = 1, .col = 6 },
                },
            },
        },
    };

    const cons = try allocator.create(Node);
    defer {
        cons.deinit(allocator);
        allocator.destroy(cons);
    }

    cons.* = .{
        .cons_expr = .{
            .head = head,
            .tail = tail,
            .operator = lexer.Token{
                .kind = lexer.TokenKind{ .operator = .Cons },
                .lexeme = "::",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 2, .end = 3 },
                    .src = .{ .line = 1, .col = 3 },
                },
            },
        },
    };

    // Assertions
    // Verify the expression is a cons expression (::)
    try testing.expect(cons.* == .cons_expr);

    // Verify the operator in the cons expression is a cons operator (::)
    try testing.expectEqual(lexer.TokenKind{ .operator = .Cons }, cons.cons_expr.operator.kind);

    // Verify the lexeme of the cons operator is "::"
    try testing.expectEqualStrings("::", cons.cons_expr.operator.lexeme);

    // Verify the head of the cons expression is an integer literal with the value 1
    try testing.expect(cons.cons_expr.head.* == .int_literal);
    try testing.expectEqual(@as(i64, 1), cons.cons_expr.head.int_literal.value);

    // Verify the tail of the cons expression is a list
    try testing.expect(cons.cons_expr.tail.* == .list);

    // Verify the list in the tail has exactly 2 elements
    try testing.expectEqual(@as(usize, 2), cons.cons_expr.tail.list.elements.items.len);

    const list_elements = cons.cons_expr.tail.list.elements.items;

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
    const left = try allocator.create(Node);
    const left_str = try allocator.dupe(u8, "Hello");
    left.* = .{
        .str_literal = .{
            .value = left_str,
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .literal = .String },
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
                .kind = lexer.TokenKind{ .literal = .String },
                .lexeme = "\"World\"",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 11, .end = 18 },
                    .src = .{ .line = 1, .col = 12 },
                },
            },
        },
    };

    const concat = try allocator.create(Node);
    defer {
        concat.deinit(allocator);
        allocator.destroy(concat);
    }

    concat.* = .{
        .str_concat_expr = .{
            .left = left,
            .right = right,
            .operator = lexer.Token{
                .kind = lexer.TokenKind{ .operator = .StrConcat },
                .lexeme = "<>",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 8, .end = 10 },
                    .src = .{ .line = 1, .col = 9 },
                },
            },
        },
    };

    // Assertions
    // Verify the operator in the string concatenation expression is a string concatenation operator (<>)
    try testing.expectEqual(lexer.TokenKind{ .operator = .StrConcat }, concat.str_concat_expr.operator.kind);

    // Verify the lexeme of the string concatenation operator is "<>"
    try testing.expectEqualStrings("<>", concat.str_concat_expr.operator.lexeme);

    // Verify the left operand of the string concatenation is a string literal
    try testing.expectEqualStrings("Hello", concat.str_concat_expr.left.str_literal.value);

    // Verify the right operand of the string concatenation is a string literal
    try testing.expectEqualStrings("World", concat.str_concat_expr.right.str_literal.value);
}

test "[ListConcatExprNode]" {
    // Test input: [1, 2] ++ [3, 4]

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const left = try allocator.create(Node);
    var left_elements = std.ArrayList(*Node).init(allocator);

    const left_elem1 = try allocator.create(Node);
    left_elem1.* = .{
        .int_literal = .{
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
        },
    };

    const left_elem2 = try allocator.create(Node);
    left_elem2.* = .{
        .int_literal = .{
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
        },
    };

    try left_elements.append(left_elem1);
    try left_elements.append(left_elem2);

    left.* = .{
        .list = .{
            .elements = left_elements,
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .delimiter = .LeftBracket },
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
                .kind = lexer.TokenKind{ .literal = .Int },
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
                .kind = lexer.TokenKind{ .literal = .Int },
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
                .kind = lexer.TokenKind{ .delimiter = .LeftBracket },
                .lexeme = "[",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 10, .end = 11 },
                    .src = .{ .line = 1, .col = 11 },
                },
            },
        },
    };

    const concat = try allocator.create(Node);
    defer {
        concat.deinit(allocator);
        allocator.destroy(concat);
    }

    concat.* = .{
        .list_concat_expr = .{
            .left = left,
            .right = right,
            .operator = lexer.Token{
                .kind = lexer.TokenKind{ .operator = .ListConcat },
                .lexeme = "++",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 7, .end = 9 },
                    .src = .{ .line = 1, .col = 8 },
                },
            },
        },
    };

    // Assertions
    const expr = concat.list_concat_expr;

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

test "[CompositionExprNode]" {
    // Test input: f >> g (compose right)

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const f = try allocator.create(Node);
    f.* = .{
        .lower_identifier = .{
            .name = "f",
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .identifier = .Lower },
                .lexeme = "f",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    const g = try allocator.create(Node);
    g.* = .{
        .lower_identifier = .{
            .name = "g",
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .identifier = .Lower },
                .lexeme = "g",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 5, .end = 6 },
                    .src = .{ .line = 1, .col = 6 },
                },
            },
        },
    };

    const compose = try allocator.create(Node);
    defer {
        compose.deinit(allocator);
        allocator.destroy(compose);
    }

    compose.* = .{
        .composition_expr = .{
            .first = f,
            .second = g,
            .operator = lexer.Token{
                .kind = lexer.TokenKind{ .operator = .ComposeRight },
                .lexeme = ">>",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 2, .end = 4 },
                    .src = .{ .line = 1, .col = 3 },
                },
            },
        },
    };

    // Assertions
    const expr = compose.composition_expr;

    // Verify the operator in the composition expression is a compose-right operator (>>)
    try testing.expectEqual(lexer.TokenKind{ .operator = .ComposeRight }, expr.operator.kind);

    // Verify the lexeme of the compose-right operator
    try testing.expectEqualStrings(">>", expr.operator.lexeme);

    // Verify the first function in the composition is a lower identifier
    try testing.expect(expr.first.* == .lower_identifier);

    // Verify the token kind of the first function is a lower identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, expr.first.lower_identifier.token.kind);

    // Verify the name of the first function is "f"
    try testing.expectEqualStrings("f", expr.first.lower_identifier.name);

    // Verify the second function in the composition is a lower identifier
    try testing.expect(expr.second.* == .lower_identifier);

    // Verify the token kind of the second function is a lower identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, expr.second.lower_identifier.token.kind);

    // Verify the name of the second function is "g"
    try testing.expectEqualStrings("g", expr.second.lower_identifier.name);
}

test "[PipeExprNode]" {
    // Test input: x |> f (pipe right)

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const value = try allocator.create(Node);
    value.* = .{
        .lower_identifier = .{
            .name = "x",
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .identifier = .Lower },
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
                .kind = lexer.TokenKind{ .identifier = .Lower },
                .lexeme = "f",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 5, .end = 6 },
                    .src = .{ .line = 1, .col = 6 },
                },
            },
        },
    };

    const pipe = try allocator.create(Node);
    defer {
        pipe.deinit(allocator);
        allocator.destroy(pipe);
    }

    pipe.* = .{
        .pipe_expr = .{
            .value = value,
            .func = func,
            .operator = lexer.Token{
                .kind = lexer.TokenKind{ .operator = .PipeRight },
                .lexeme = "|>",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 2, .end = 4 },
                    .src = .{ .line = 1, .col = 3 },
                },
            },
        },
    };

    // Assertions
    const expr = pipe.pipe_expr;

    // Verify the operator in the expression is a pipe-right operator (|>)
    try testing.expectEqual(lexer.TokenKind{ .operator = .PipeRight }, expr.operator.kind);

    // Verify the lexeme of the pipe-right operator
    try testing.expectEqualStrings("|>", expr.operator.lexeme);

    // Verify the value being piped is a lower identifier
    try testing.expect(expr.value.* == .lower_identifier);

    // Verify the token kind of the value being piped is a lower identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, expr.value.lower_identifier.token.kind);

    // Verify the name of the value being piped is "x"
    try testing.expectEqualStrings("x", expr.value.lower_identifier.name);

    // Verify the function being applied is a lower identifier
    try testing.expect(expr.func.* == .lower_identifier);

    // Verify the token kind of the function being applied is a lower identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, expr.func.lower_identifier.token.kind);

    // Verify the name of the function being applied is "f".
    try testing.expectEqualStrings("f", expr.func.lower_identifier.name);
}

test "[IfThenElseStmtNode]" {
    // Test input: if x == y then True else False

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const left = try allocator.create(Node);
    left.* = .{
        .lower_identifier = .{
            .name = "x",
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .identifier = .Lower },
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
                .kind = lexer.TokenKind{ .identifier = .Lower },
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
            .right = right,
            .operator = lexer.Token{
                .kind = lexer.TokenKind{ .operator = .Equality },
                .lexeme = "==",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 5, .end = 7 },
                    .src = .{ .line = 1, .col = 6 },
                },
            },
        },
    };

    const then_branch = try allocator.create(Node);
    then_branch.* = .{
        .upper_identifier = .{
            .name = "True",
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .identifier = .Upper },
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
                .kind = lexer.TokenKind{ .identifier = .Upper },
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

    // Assertions
    try testing.expect(if_then_else.* == .if_then_else_stmt);

    const cond = if_then_else.if_then_else_stmt.condition.comparison_expr;

    // Verify the condition is a comparison expression
    try testing.expect(if_then_else.if_then_else_stmt.condition.* == .comparison_expr);

    // Check the operator in the comparison expression is an equality operator (==)
    try testing.expectEqual(lexer.TokenKind{ .operator = .Equality }, cond.operator.kind);

    // Ensure the lexeme for the equality operator is "=="
    try testing.expectEqualStrings("==", cond.operator.lexeme);

    // Verify the left operand of the condition is a lower-case identifier (x)
    try testing.expect(cond.left.* == .lower_identifier);

    // Check the name of the left identifier is "x"
    try testing.expectEqualStrings("x", cond.left.lower_identifier.name);

    // Ensure the token kind of the left identifier is a lower-case identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, cond.left.lower_identifier.token.kind);

    // Verify the right operand of the condition is a lower-case identifier (y)
    try testing.expect(cond.right.* == .lower_identifier);

    // Check the name of the right identifier is "y"
    try testing.expectEqualStrings("y", cond.right.lower_identifier.name);

    // Ensure the token kind of the right identifier is a lower-case identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, cond.right.lower_identifier.token.kind);

    // Verify the "then" branch is an upper-case identifier (True)
    try testing.expect(if_then_else.if_then_else_stmt.then_branch.* == .upper_identifier);

    // Check the name of the "then" branch is "True"
    try testing.expectEqualStrings("True", if_then_else.if_then_else_stmt.then_branch.upper_identifier.name);

    // Ensure the token kind of the "then" branch is an upper-case identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, if_then_else.if_then_else_stmt.then_branch.upper_identifier.token.kind);

    // Verify the "else" branch is an upper-case identifier (False)
    try testing.expect(if_then_else.if_then_else_stmt.else_branch.* == .upper_identifier);

    // Check the name of the "else" branch is "False"
    try testing.expectEqualStrings("False", if_then_else.if_then_else_stmt.else_branch.upper_identifier.name);

    // Ensure the token kind of the "else" branch is an upper-case identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, if_then_else.if_then_else_stmt.else_branch.upper_identifier.token.kind);
}

test "[TypedHoleNode]" {
    // Test input: ?

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
        .typed_hole = .{
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .special = .Hole },
                .lexeme = "?",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    // Assertions
    // Verify the node is a typed hole
    try testing.expect(node.* == .typed_hole);

    // Verify the token kind is a question mark
    try testing.expectEqual(lexer.TokenKind{ .special = .Hole }, node.typed_hole.token.kind);

    // Verify the lexeme is a question mark
    try testing.expectEqualStrings("?", node.typed_hole.token.lexeme);
}

test "[TypeApplicationNode]" {
    // Test input: Map k v

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const base = try allocator.create(Node);
    base.* = .{
        .upper_identifier = .{
            .name = "Map",
            .token = .{
                .kind = .{ .identifier = .Upper },
                .lexeme = "Map",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 3 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    var args = std.ArrayList(*Node).init(allocator);

    const k = try allocator.create(Node);
    k.* = .{
        .lower_identifier = .{
            .name = "k",
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "k",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 4, .end = 5 },
                    .src = .{ .line = 1, .col = 5 },
                },
            },
        },
    };

    const v = try allocator.create(Node);
    v.* = .{
        .lower_identifier = .{
            .name = "v",
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "v",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 6, .end = 7 },
                    .src = .{ .line = 1, .col = 7 },
                },
            },
        },
    };

    try args.append(k);
    try args.append(v);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
        .type_application = .{
            .base = base,
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
        },
    };

    // Assertions
    // Verify the node is a type application
    try testing.expect(node.* == .type_application);

    const app = node.type_application;

    // Verify the base type of the type application is an upper identifier named "Map"
    try testing.expect(app.base.* == .upper_identifier);
    try testing.expectEqualStrings("Map", app.base.upper_identifier.name);

    // Verify that the type application has exactly two arguments
    try testing.expectEqual(@as(usize, 2), app.args.items.len);

    // Verify the first argument is a lower identifier with the name "k"
    try testing.expect(app.args.items[0].* == .lower_identifier);
    try testing.expectEqualStrings("k", app.args.items[0].lower_identifier.name);

    // Verify the second argument is a lower identifier with the name "v"
    try testing.expect(app.args.items[1].* == .lower_identifier);
    try testing.expectEqualStrings("v", app.args.items[1].lower_identifier.name);
}

test "[TypeAliasNode]" {
    // Test input: type alias UserId = String

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const value = try allocator.create(Node);
    value.* = .{
        .upper_identifier = .{
            .name = "String",
            .token = .{
                .kind = .{ .identifier = .Upper },
                .lexeme = "String",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 20, .end = 26 },
                    .src = .{ .line = 1, .col = 21 },
                },
            },
        },
    };

    const type_params = std.ArrayList([]const u8).init(allocator);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
        .type_alias = .{
            .name = try allocator.dupe(u8, "UserId"),
            .type_params = type_params,
            .value = value,
            .token = .{
                .kind = .{ .keyword = .Type },
                .lexeme = "type",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 4 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

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
    try testing.expectEqualStrings("UserId", node.type_alias.name);

    // Check the value node is an upper identifier
    try testing.expect(node.type_alias.value.* == .upper_identifier);

    const value_node = node.type_alias.value.upper_identifier;

    // Verify the name of the upper identifier
    try testing.expectEqualStrings("String", value_node.name);

    // Verify the token kind of the upper identifier
    try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, value_node.token.kind);

    // Verify the lexeme of the upper identifier
    try testing.expectEqualStrings("String", value_node.token.lexeme);
}

test "[VariantTypeNode]" {
    // Test input: type Result e a = | Err e | Ok a

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    // First constructor: Err e
    var err_params = std.ArrayList(*Node).init(allocator);
    const err_param = try allocator.create(Node);

    err_param.* = .{
        .lower_identifier = .{
            .name = "e",
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "e",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 0 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    try err_params.append(err_param);

    // Second constructor: Ok a
    var ok_params = std.ArrayList(*Node).init(allocator);
    const ok_param = try allocator.create(Node);

    ok_param.* = .{
        .lower_identifier = .{
            .name = "a",
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "a",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 0 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    try ok_params.append(ok_param);

    // Create constructors list
    var constructors = std.ArrayList(VariantConstructorNode).init(allocator);
    try constructors.append(.{
        .name = "Err",
        .params = err_params,
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Err",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 0 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    });
    try constructors.append(.{
        .name = "Ok",
        .params = ok_params,
        .token = .{
            .kind = .{ .identifier = .Upper },
            .lexeme = "Ok",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 0 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    });

    // Create type parameters list
    var type_params = std.ArrayList([]const u8).init(allocator);
    try type_params.append(try allocator.dupe(u8, "e"));
    try type_params.append(try allocator.dupe(u8, "a"));

    // Create the variant type node
    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
        .variant_type = .{
            .name = try allocator.dupe(u8, "Result"),
            .type_params = type_params,
            .constructors = constructors,
            .token = .{
                .kind = .{ .keyword = .Type },
                .lexeme = "type",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 0 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    // Assertions
    // Verify the node is a variant type
    try testing.expect(node.* == .variant_type);

    // Verify the name of the variant type is "Result"
    try testing.expectEqualStrings("Result", node.variant_type.name);

    // Verify the variant type has exactly two type parameters
    try testing.expectEqual(@as(usize, 2), node.variant_type.type_params.items.len);

    // Verify the name of the first type parameter is "e"
    try testing.expectEqualStrings("e", node.variant_type.type_params.items[0]);

    // Verify the name of the second type parameter is "a"
    try testing.expectEqualStrings("a", node.variant_type.type_params.items[1]);

    // Verify the variant type has exactly two constructors
    try testing.expectEqual(@as(usize, 2), node.variant_type.constructors.items.len);

    // Verify the name of the first constructor is "Err"
    try testing.expectEqualStrings("Err", node.variant_type.constructors.items[0].name);

    // Verify the name of the second constructor is "Ok"
    try testing.expectEqualStrings("Ok", node.variant_type.constructors.items[1].name);

    // "Err" constructor
    const err_constructor = node.variant_type.constructors.items[0];

    // Verify the "Err" constructor has one parameter
    try testing.expectEqual(@as(usize, 1), err_constructor.params.items.len);

    // Verify the name of the parameter for the "Err" constructor is "e"
    try testing.expectEqualStrings("e", err_constructor.params.items[0].lower_identifier.name);

    // "Ok" constructor
    const ok_constructor = node.variant_type.constructors.items[1];

    // Verify the "Ok" constructor has one parameter.
    try testing.expectEqual(@as(usize, 1), ok_constructor.params.items.len);

    // Verify the name of the parameter for the "Ok" constructor is "a".
    try testing.expectEqualStrings("a", ok_constructor.params.items[0].lower_identifier.name);
}

test "[RecordTypeNode]" {
    // Test input: type Point a = { x: a, y: a }

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    var type_params = std.ArrayList([]const u8).init(allocator);
    try type_params.append(try allocator.dupe(u8, "a"));

    // Create fields
    var fields = std.ArrayList(RecordFieldNode).init(allocator);

    // Create x field type (a)
    const x_type = try allocator.create(Node);
    x_type.* = .{
        .lower_identifier = .{
            .name = "a",
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "a",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 15, .end = 16 },
                    .src = .{ .line = 1, .col = 16 },
                },
            },
        },
    };

    // Create y field type (a)
    const y_type = try allocator.create(Node);
    y_type.* = .{
        .lower_identifier = .{
            .name = "a",
            .token = .{
                .kind = .{ .identifier = .Lower },
                .lexeme = "a",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 21, .end = 22 },
                    .src = .{ .line = 1, .col = 22 },
                },
            },
        },
    };

    try fields.append(.{
        .name = try allocator.dupe(u8, "x"),
        .type = x_type,
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "x",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 13, .end = 14 },
                .src = .{ .line = 1, .col = 14 },
            },
        },
    });

    try fields.append(.{
        .name = try allocator.dupe(u8, "y"),
        .type = y_type,
        .token = .{
            .kind = .{ .identifier = .Lower },
            .lexeme = "y",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 19, .end = 20 },
                .src = .{ .line = 1, .col = 20 },
            },
        },
    });

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
        .record_type = .{
            .name = try allocator.dupe(u8, "Point"),
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
        },
    };

    // Assertions
    // Verify the node is a record type
    try testing.expect(node.* == .record_type);

    const record = node.record_type;

    // Verify the name of the record type is "Point"
    try testing.expectEqualStrings("Point", record.name);

    // Ensure the record type has exactly one type parameter
    try testing.expectEqual(@as(usize, 1), record.type_params.items.len);

    // Check the name of the type parameter is "a"
    try testing.expectEqualStrings("a", record.type_params.items[0]);

    // Verify the record type has exactly two fields
    try testing.expectEqual(@as(usize, 2), record.fields.items.len);

    // Test the first field (x)
    const x_field = record.fields.items[0];

    // Ensure the name of the first field is "x"
    try testing.expectEqualStrings("x", x_field.name);

    // Verify the type of the first field is a lower-case identifier
    try testing.expect(x_field.type.* == .lower_identifier);

    // Check the name of the type for the first field is "a"
    try testing.expectEqualStrings("a", x_field.type.lower_identifier.name);

    // Test the second field (y)
    const y_field = record.fields.items[1];

    // Ensure the name of the second field is "y"
    try testing.expectEqualStrings("y", y_field.name);

    // Verify the type of the second field is a lower-case identifier
    try testing.expect(y_field.type.* == .lower_identifier);

    // Check the name of the type for the second field is "a"
    try testing.expectEqualStrings("a", y_field.type.lower_identifier.name);

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
    var segments = std.ArrayList([]const u8).init(allocator);
    try segments.append(try allocator.dupe(u8, "Std"));
    try segments.append(try allocator.dupe(u8, "List"));

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
        .module_path = .{
            .segments = segments,
            .token = .{
                .kind = .{ .identifier = .Upper },
                .lexeme = "Std",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 3 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    // Assertions
    // Verify the node is a module path
    try testing.expect(node.* == .module_path);

    const path = node.module_path;

    // Verify the include path consists of exactly two segments
    try testing.expectEqual(@as(usize, 2), path.segments.items.len);

    // Ensure the first segment of the include path is "Std"
    try testing.expectEqualStrings("Std", path.segments.items[0]);

    // Ensure the second segment of the include path is "List"
    try testing.expectEqualStrings("List", path.segments.items[1]);
}

test "[ExportSpecNode]" {
    // Test input: exposing (func1, Type1(..))

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    var items = std.ArrayList(ExportItem).init(allocator);
    try items.append(.{
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
    });
    try items.append(.{
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
    });

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
        .export_spec = .{
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
        },
    };

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
        var segments = std.ArrayList([]const u8).init(allocator);
        try segments.append(try allocator.dupe(u8, "MyModule"));

        const import = try allocator.create(Node);
        defer {
            import.deinit(allocator);
            allocator.destroy(import);
        }

        import.* = .{
            .import_spec = .{
                .path = .{
                    .segments = segments,
                    .token = lexer.Token{
                        .kind = lexer.TokenKind{ .identifier = .Upper },
                        .lexeme = "MyModule",
                        .loc = .{
                            .filename = TEST_FILE,
                            .span = .{ .start = 5, .end = 13 },
                            .src = .{ .line = 1, .col = 6 },
                        },
                    },
                },
                .kind = .Simple,
                .alias = null,
                .items = null,
                .token = lexer.Token{
                    .kind = lexer.TokenKind{ .keyword = .Open },
                    .lexeme = "open",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 4 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        // Assertions
        // Verify the node is an import specification
        try testing.expect(import.* == .import_spec);

        // Verify the node is an import specification
        try testing.expect(import.* == .import_spec);

        const spec = import.import_spec;

        // Verify it's a simple import
        try testing.expectEqual(ImportKind.Simple, spec.kind);

        // Verify path has exactly one segment
        try testing.expectEqual(@as(usize, 1), spec.path.segments.items.len);

        // Verify the module name
        try testing.expectEqualStrings("MyModule", spec.path.segments.items[0]);

        // Verify it has no alias or items
        try testing.expect(spec.alias == null);
        try testing.expect(spec.items == null);
    }

    {
        // Test input: open MyModule as M
        var segments = std.ArrayList([]const u8).init(allocator);
        try segments.append(try allocator.dupe(u8, "MyModule"));

        const import = try allocator.create(Node);
        defer {
            import.deinit(allocator);
            allocator.destroy(import);
        }

        import.* = .{
            .import_spec = .{
                .path = .{
                    .segments = segments,
                    .token = lexer.Token{
                        .kind = lexer.TokenKind{ .identifier = .Upper },
                        .lexeme = "MyModule",
                        .loc = .{
                            .filename = TEST_FILE,
                            .span = .{ .start = 5, .end = 13 },
                            .src = .{ .line = 1, .col = 6 },
                        },
                    },
                },
                .kind = .Alias,
                .alias = try allocator.dupe(u8, "M"),
                .items = null,
                .token = lexer.Token{
                    .kind = lexer.TokenKind{ .keyword = .Open },
                    .lexeme = "open",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 4 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        // Assertions
        // Verify the node is an import specification
        try testing.expect(import.* == .import_spec);

        const spec = import.import_spec;

        // Verify it's an alias import
        try testing.expectEqual(ImportKind.Alias, spec.kind);

        // Verify path has exactly one segment
        try testing.expectEqual(@as(usize, 1), spec.path.segments.items.len);

        // Verify the module name
        try testing.expectEqualStrings("MyModule", spec.path.segments.items[0]);

        // Verify the alias
        try testing.expect(spec.alias != null);
        try testing.expectEqualStrings("M", spec.alias.?);

        // Verify it has no items
        try testing.expect(spec.items == null);
    }

    {
        // Test input: open MyModule using (map, filter, Maybe, Either(..))
        var segments = std.ArrayList([]const u8).init(allocator);
        try segments.append(try allocator.dupe(u8, "MyModule"));

        var items = std.ArrayList(ImportItem).init(allocator);
        try items.append(.{
            .function = .{
                .name = try allocator.dupe(u8, "map"),
                .alias = null,
            },
        });
        try items.append(.{
            .function = .{
                .name = try allocator.dupe(u8, "filter"),
                .alias = null,
            },
        });
        try items.append(.{
            .type = .{
                .name = try allocator.dupe(u8, "Maybe"),
                .expose_constructors = false,
                .alias = null,
            },
        });
        try items.append(.{
            .type = .{
                .name = try allocator.dupe(u8, "Either"),
                .expose_constructors = true,
                .alias = null,
            },
        });

        const import = try allocator.create(Node);
        defer {
            import.deinit(allocator);
            allocator.destroy(import);
        }

        import.* = .{
            .import_spec = .{
                .path = .{
                    .segments = segments,
                    .token = lexer.Token{
                        .kind = lexer.TokenKind{ .identifier = .Upper },
                        .lexeme = "MyModule",
                        .loc = .{
                            .filename = TEST_FILE,
                            .span = .{ .start = 5, .end = 13 },
                            .src = .{ .line = 1, .col = 6 },
                        },
                    },
                },
                .kind = .Using,
                .alias = null,
                .items = items,
                .token = lexer.Token{
                    .kind = lexer.TokenKind{ .keyword = .Open },
                    .lexeme = "open",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 4 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        // Assertions
        // Verify the node is an import specification
        try testing.expect(import.* == .import_spec);

        const spec = import.import_spec;

        // Verify it's a using import
        try testing.expectEqual(ImportKind.Using, spec.kind);

        // Verify path has exactly one segment
        try testing.expectEqual(@as(usize, 1), spec.path.segments.items.len);

        // Verify the module name
        try testing.expectEqualStrings("MyModule", spec.path.segments.items[0]);

        // Verify it has no alias
        try testing.expect(spec.alias == null);

        // Verify items list
        try testing.expect(spec.items != null);
        try testing.expectEqual(@as(usize, 4), spec.items.?.items.len);

        // Verify first two items are functions
        try testing.expect(spec.items.?.items[0] == .function);
        try testing.expectEqualStrings("map", spec.items.?.items[0].function.name);
        try testing.expect(spec.items.?.items[0].function.alias == null);
        try testing.expect(spec.items.?.items[1] == .function);
        try testing.expectEqualStrings("filter", spec.items.?.items[1].function.name);
        try testing.expect(spec.items.?.items[1].function.alias == null);

        // Verify last two items are types
        try testing.expect(spec.items.?.items[2] == .type);
        try testing.expectEqualStrings("Maybe", spec.items.?.items[2].type.name);
        try testing.expect(spec.items.?.items[2].type.alias == null);
        try testing.expect(spec.items.?.items[2].type.expose_constructors == false);

        try testing.expect(spec.items.?.items[3] == .type);
        try testing.expectEqualStrings("Either", spec.items.?.items[3].type.name);
        try testing.expect(spec.items.?.items[3].type.alias == null);
        try testing.expect(spec.items.?.items[3].type.expose_constructors == true);
    }

    {
        // Test input: open MyModule using (map as list_map)
        var segments = std.ArrayList([]const u8).init(allocator);
        try segments.append(try allocator.dupe(u8, "MyModule"));

        var items = std.ArrayList(ImportItem).init(allocator);
        try items.append(.{
            .function = .{
                .name = try allocator.dupe(u8, "map"),
                .alias = try allocator.dupe(u8, "list_map"),
            },
        });

        const import = try allocator.create(Node);
        defer {
            import.deinit(allocator);
            allocator.destroy(import);
        }

        import.* = .{
            .import_spec = .{
                .path = .{
                    .segments = segments,
                    .token = lexer.Token{
                        .kind = lexer.TokenKind{ .identifier = .Upper },
                        .lexeme = "MyModule",
                        .loc = .{
                            .filename = TEST_FILE,
                            .span = .{ .start = 5, .end = 13 },
                            .src = .{ .line = 1, .col = 6 },
                        },
                    },
                },
                .kind = .Using,
                .alias = null,
                .items = items,
                .token = lexer.Token{
                    .kind = lexer.TokenKind{ .keyword = .Open },
                    .lexeme = "open",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 4 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        // Assertions
        // Verify the node is an import specification
        try testing.expect(import.* == .import_spec);

        const spec = import.import_spec;

        // Verify it's an using import
        try testing.expectEqual(ImportKind.Using, spec.kind);

        // Verify path has exactly one segment
        try testing.expectEqual(@as(usize, 1), spec.path.segments.items.len);

        // Verify the module name
        try testing.expectEqualStrings("MyModule", spec.path.segments.items[0]);

        // Verify it has no alias
        try testing.expect(spec.alias == null);

        // Verify items list contains one item
        try testing.expect(spec.items != null);
        try testing.expectEqual(@as(usize, 1), spec.items.?.items.len);
        try testing.expect(spec.items.?.items[0] == .function);
        try testing.expectEqualStrings("map", spec.items.?.items[0].function.name);
        try testing.expectEqualStrings("list_map", spec.items.?.items[0].function.alias.?);
    }

    {
        // Test input: open MyModule hiding (internal_func)
        var segments = std.ArrayList([]const u8).init(allocator);
        try segments.append(try allocator.dupe(u8, "MyModule"));

        var items = std.ArrayList(ImportItem).init(allocator);
        try items.append(.{
            .function = .{
                .name = try allocator.dupe(u8, "internal_func"),
                .alias = null,
            },
        });

        const import = try allocator.create(Node);
        defer {
            import.deinit(allocator);
            allocator.destroy(import);
        }

        import.* = .{
            .import_spec = .{
                .path = .{
                    .segments = segments,
                    .token = lexer.Token{
                        .kind = lexer.TokenKind{ .identifier = .Upper },
                        .lexeme = "MyModule",
                        .loc = .{
                            .filename = TEST_FILE,
                            .span = .{ .start = 5, .end = 13 },
                            .src = .{ .line = 1, .col = 6 },
                        },
                    },
                },
                .kind = .Hiding,
                .alias = null,
                .items = items,
                .token = lexer.Token{
                    .kind = lexer.TokenKind{ .keyword = .Open },
                    .lexeme = "open",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 4 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        // Assertions
        // Verify the node is an import specification
        try testing.expect(import.* == .import_spec);

        const spec = import.import_spec;

        // Verify it's a hiding import
        try testing.expectEqual(ImportKind.Hiding, spec.kind);

        // Verify path has exactly one segment
        try testing.expectEqual(@as(usize, 1), spec.path.segments.items.len);

        // Verify the module name
        try testing.expectEqualStrings("MyModule", spec.path.segments.items[0]);

        // Verify it has no alias
        try testing.expect(spec.alias == null);

        // Verify items list contains one item to hide
        try testing.expect(spec.items != null);
        try testing.expectEqual(@as(usize, 1), spec.items.?.items.len);
        try testing.expect(spec.items.?.items[0] == .function);
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
    var segments = std.ArrayList([]const u8).init(allocator);
    try segments.append(try allocator.dupe(u8, "Std"));
    try segments.append(try allocator.dupe(u8, "List"));

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
        .include = .{
            .path = .{
                .segments = segments,
                .token = .{
                    .kind = .{ .identifier = .Upper },
                    .lexeme = "Std",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 8, .end = 12 },
                        .src = .{ .line = 1, .col = 9 },
                    },
                },
            },
            .token = .{
                .kind = .{ .keyword = .Include },
                .lexeme = "include",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 7 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    // Assertions
    // Verify the node is an include declaration
    try testing.expect(node.* == .include);

    const include = node.include;

    // Verify the include path consists of exactly two segments
    try testing.expectEqual(@as(usize, 2), include.path.segments.items.len);

    // Ensure the first segment of the include path is "Std"
    try testing.expectEqualStrings("Std", include.path.segments.items[0]);

    // Ensure the second segment of the include path is "List"
    try testing.expectEqualStrings("List", include.path.segments.items[1]);
}

test "[FunctionDeclNode]" {
    // Test input: let add : Int -> Int -> Int = \x y => x + y

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    var param_types = std.ArrayList(*Node).init(allocator);

    const int_type1 = try allocator.create(Node);
    int_type1.* = .{
        .upper_identifier = .{
            .name = "Int",
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .identifier = .Upper },
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
                .kind = lexer.TokenKind{ .identifier = .Upper },
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
                .kind = lexer.TokenKind{ .identifier = .Upper },
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
                .kind = lexer.TokenKind{ .delimiter = .Colon },
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
                .kind = lexer.TokenKind{ .identifier = .Lower },
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
                .kind = lexer.TokenKind{ .identifier = .Lower },
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
            .right = right,
            .operator = lexer.Token{
                .kind = lexer.TokenKind{ .operator = .IntAdd },
                .lexeme = "+",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 39, .end = 40 },
                    .src = .{ .line = 1, .col = 40 },
                },
            },
        },
    };

    const lambda = try allocator.create(Node);
    lambda.* = .{
        .lambda_expr = .{
            .params = params,
            .body = body,
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .operator = .Lambda },
                .lexeme = "\\",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 30, .end = 31 },
                    .src = .{ .line = 1, .col = 31 },
                },
            },
        },
    };

    const func_decl = try allocator.create(Node);
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
                .kind = lexer.TokenKind{ .keyword = .Let },
                .lexeme = "let",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 3 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    // Assertions
    // Check the keyword for the function declaration is "let"
    try testing.expectEqual(lexer.TokenKind{ .keyword = .Let }, func_decl.function_decl.token.kind);

    // Verify the lexeme for the "let" keyword matches "let"
    try testing.expectEqualStrings("let", func_decl.function_decl.token.lexeme);

    // Verify the function name is "add"
    try testing.expectEqualStrings("add", func_decl.function_decl.name);

    const type_annot = func_decl.function_decl.type_annotation.?;

    // Check the type annotation is a function type
    try testing.expect(type_annot.* == .function_type);

    // Check the token for the type annotation is a colon (":")
    try testing.expectEqual(lexer.TokenKind{ .delimiter = .Colon }, type_annot.function_type.token.kind);

    // Check the function type has exactly 3 parameter types (Int -> Int -> Int)
    try testing.expectEqual(@as(usize, 3), type_annot.function_type.param_types.items.len);

    // Verify lambda parameter count matches function type (excluding return type)
    try testing.expectEqual(type_annot.function_type.param_types.items.len - 1, lambda.lambda_expr.params.items.len);

    for (type_annot.function_type.param_types.items) |type_node| {
        // Check the type node is an upper identifier
        try testing.expect(type_node.* == .upper_identifier);

        // Check the token kind for the type node is an upper identifier
        try testing.expectEqual(lexer.TokenKind{ .identifier = .Upper }, type_node.upper_identifier.token.kind);

        // Check the name of the type node is "Int"
        try testing.expectEqualStrings("Int", type_node.upper_identifier.name);
    }

    const lambda_value = func_decl.function_decl.value;

    // Check the function body is a lambda expression
    try testing.expect(lambda_value.* == .lambda_expr);

    // Check the token for the lambda expression is a lambda operator ("\")
    try testing.expectEqual(lexer.TokenKind{ .operator = .Lambda }, lambda_value.lambda_expr.token.kind);

    // Check the lambda has exactly 2 parameters
    try testing.expectEqual(@as(usize, 2), lambda_value.lambda_expr.params.items.len);

    // Verify the names of the lambda parameters are "x" and "y"
    try testing.expectEqualStrings("x", lambda_value.lambda_expr.params.items[0]);
    try testing.expectEqualStrings("y", lambda_value.lambda_expr.params.items[1]);

    const lambda_body = lambda_value.lambda_expr.body;

    // Check the lambda body is an arithmetic expression
    try testing.expect(lambda_body.* == .arithmetic_expr);

    // Check the operator in the arithmetic expression is integer addition ("+")
    try testing.expectEqual(lexer.TokenKind{ .operator = .IntAdd }, lambda_body.arithmetic_expr.operator.kind);

    // Check the lexeme for the addition operator matches "+"
    try testing.expectEqualStrings("+", lambda_body.arithmetic_expr.operator.lexeme);

    const body_left = lambda_body.arithmetic_expr.left;

    // Check the left operand is a lower identifier with the name "x"
    try testing.expect(body_left.* == .lower_identifier);
    try testing.expectEqualStrings("x", body_left.lower_identifier.name);

    const body_right = lambda_body.arithmetic_expr.right;

    // Check the left operand is a lower identifier with the name "y"
    try testing.expect(body_right.* == .lower_identifier);
    try testing.expectEqualStrings("y", body_right.lower_identifier.name);
}

test "[ForeignFunctionDeclNode]" {
    // Test input: foreign sqrt : Float -> Float = "c_sqrt"

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    const type_node = try allocator.create(Node);
    const float_type1 = try allocator.create(Node);
    const float_type2 = try allocator.create(Node);

    var param_types = std.ArrayList(*Node).init(allocator);
    try param_types.append(float_type1);
    try param_types.append(float_type2);

    float_type1.* = .{
        .upper_identifier = .{
            .name = "Float",
            .token = .{
                .kind = .{ .identifier = .Upper },
                .lexeme = "Float",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 5 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    float_type2.* = .{
        .upper_identifier = .{
            .name = "Float",
            .token = .{
                .kind = .{ .identifier = .Upper },
                .lexeme = "Float",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 5 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    type_node.* = .{
        .function_type = .{
            .param_types = param_types,
            .token = .{
                .kind = .{ .symbol = .ArrowRight },
                .lexeme = "->",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 2 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    const foreign_func = try allocator.create(Node);
    defer {
        foreign_func.deinit(allocator);
        allocator.destroy(foreign_func);
    }

    foreign_func.* = .{
        .foreign_function_decl = .{
            .name = "sqrt",
            .type_annotation = type_node,
            .external_name = try allocator.dupe(u8, "c_sqrt"),
            .token = .{
                .kind = .{ .keyword = .Foreign },
                .lexeme = "foreign",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 7 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    // Assertions
    const decl = foreign_func.foreign_function_decl;

    // Verify the name of the declaration
    try testing.expectEqualStrings("sqrt", decl.name);

    // Verify the external name of the declaration
    try testing.expectEqualStrings("c_sqrt", decl.external_name);

    // Check the function type annotation specifies exactly two parameter types
    try testing.expectEqual(@as(usize, 2), decl.type_annotation.function_type.param_types.items.len);

    // Verify both parameter types are Float
    const fn_param_types = decl.type_annotation.function_type.param_types.items;
    try testing.expect(fn_param_types[0].* == .upper_identifier);
    try testing.expectEqualStrings("Float", fn_param_types[0].upper_identifier.name);
    try testing.expect(fn_param_types[1].* == .upper_identifier);
    try testing.expectEqualStrings("Float", fn_param_types[1].upper_identifier.name);
}

test "[ModuleDeclNode]" {
    // Test input: module MyModule exposing (..) ... end

    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Action
    var segments = std.ArrayList([]const u8).init(allocator);
    try segments.append(try allocator.dupe(u8, "MyModule"));

    var declarations = std.ArrayList(*Node).init(allocator);

    const dummy_decl = try allocator.create(Node);
    dummy_decl.* = .{
        .comment = .{
            .content = try allocator.dupe(u8, "dummy declaration"),
            .token = .{
                .kind = .{ .comment = .Regular },
                .lexeme = "#",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    try declarations.append(dummy_decl);

    const node = try allocator.create(Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
        .module_decl = .{
            .path = .{
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
            },
            .exports = .{
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
            },
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
        },
    };

    // Assertions
    // Verify the node is a module declaration
    try testing.expect(node.* == .module_decl);

    // Verify module path
    try testing.expectEqual(@as(usize, 1), node.module_decl.path.segments.items.len);
    try testing.expectEqualStrings("MyModule", node.module_decl.path.segments.items[0]);

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
    var statements = std.ArrayList(*Node).init(allocator);
    const stmt = try allocator.create(Node);

    stmt.* = .{
        .comment = .{
            .content = try allocator.dupe(u8, "program statement"),
            .token = .{
                .kind = .{ .comment = .Regular },
                .lexeme = "#",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    try statements.append(stmt);

    const node = try allocator.create(Node);
    defer {
        for (node.program.statements.items) |statement| {
            statement.deinit(allocator);
            allocator.destroy(statement);
        }

        node.program.statements.deinit();
        allocator.destroy(node);
    }

    node.* = .{
        .program = .{
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
        },
    };

    // Assertions
    // Verify the node is a program
    try testing.expect(node.* == .program);

    // Verify program contains one statement
    try testing.expectEqual(@as(usize, 1), node.program.statements.items.len);
    try testing.expect(node.program.statements.items[0].* == .comment);
}
