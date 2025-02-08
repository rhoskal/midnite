const std = @import("std");

const lexer = @import("lexer.zig");

/// Represents a regular, single line comment.
pub const CommentNode = struct {
    /// The content of the comment, excluding the comment marker.
    content: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,
};

/// Represents a documentation comment that will be processed as markdown.
pub const DocCommentNode = struct {
    /// The content of the documentation comment, excluding the comment marker.
    content: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,
};

/// Represents a literal integer value.
pub const IntLiteralNode = struct {
    /// The parsed integer value.
    value: i64,

    /// The token representing the start of this declaration.
    token: lexer.Token,
};

/// Represents a literal floating-point value.
pub const FloatLiteralNode = struct {
    /// The parsed floating-point value.
    value: f64,

    /// The token representing the start of this declaration.
    token: lexer.Token,
};

/// Represents a string literal value.
pub const StrLiteralNode = struct {
    /// The parsed string value with escape sequences processed.
    value: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,
};

/// Represents a multiline string literal value.
pub const MultilineStrLiteralNode = struct {
    /// The parsed multiline string value with escape sequences processed.
    value: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,
};

/// Represents a char literal value.
pub const CharLiteralNode = struct {
    /// The Unicode codepoint value of the character.
    value: u21,

    /// The token representing the start of this declaration.
    token: lexer.Token,
};

/// Represents a list of comma-separated expressions surrounded by square brackets.
///
/// Examples:
/// - `[1, 2, 3]`
/// - `[]` (empty list)
/// - `[True, False, True]`
pub const ListNode = struct {
    /// Array of pointers to the AST nodes representing list elements.
    elements: std.ArrayList(*Node),

    /// The token representing the start of this declaration.
    token: lexer.Token,
};

/// Common structure for binary operations.
pub const BinaryOp = struct {
    /// The AST node for the left operand.
    left: *Node,

    /// The AST node for the right operand.
    right: *Node,

    /// The token representing the operator.
    operator: lexer.Token,
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
    /// The AST node representing the operand.
    operand: *Node,

    /// The token representing the operator.
    operator: lexer.Token,
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

/// A single pattern matching case with an optional guard condition.
/// When pattern matches and guard evaluates true, the expression is evaluated.
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

/// Represents a function type annotation.
///
/// Examples:
/// - `Int -> Int -> Int`
pub const FunctionTypeNode = struct {
    /// Array of AST nodes representing parameter types, with last being return type.
    param_types: std.ArrayList(*Node),

    /// The token representing the start of this declaration.
    token: lexer.Token,
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
};

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

/// Represents a lowercase identifier reference (variable names, function names, etc).
pub const LowerIdentifierNode = struct {
    /// The text of the identifier.
    name: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,
};

/// Represents an uppercase identifier reference (type names, type constructors, etc).
pub const UpperIdentifierNode = struct {
    /// The text of the identifier.
    name: []const u8,

    /// The token representing the start of this declaration.
    token: lexer.Token,
};

/// Represents a binary operation that constructs a list by prepending
/// an element to the front of a list.
///
/// Examples:
/// - `1 :: [2, 3]` evaluates to `[1, 2, 3]`
/// - `x :: xs` pattern matches head and tail of list
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
/// - `x |> f` applies f to x (forward pipe)
/// - `f <| x` applies f to x (backward pipe)
pub const PipeExprNode = struct {
    /// The AST node representing the value being piped.
    value: *Node,

    /// The AST node representing the function receiving the piped value.
    func: *Node,

    /// The token representing the pipe operator.
    operator: lexer.Token,
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

pub const ExportItem = struct {
    /// The name of the item being exported.
    name: []const u8,

    /// Whether to expose constructors for exported types.
    expose_constructors: bool,

    /// The token representing the start of this declaration.
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
    /// Array of identifiers forming the module path.
    segments: std.ArrayList([]const u8),

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
        /// The original name of the imported item.
        old_name: []const u8,

        /// The new name to use locally.
        new_name: []const u8,

        /// The token representing the start of this declaration.
        token: lexer.Token,
    };

    /// The module path being imported.
    path: ModulePathNode,

    /// The kind of import being performed.
    kind: ImportKind,

    /// Optional alias name for the imported module.
    alias: ?[]const u8,

    /// Optional list of items being imported or renamed.
    items: ?std.ArrayList(union(enum) {
        name: []const u8,
        rename: RenameItem,
    }),

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

/// Represents a typed hole - a placeholder in code where type inference
/// should determine the appropriate type. Useful during development
/// and for type-driven development.
///
/// Examples:
/// - `?` marks a hole to be filled
/// - `let x : ? = expr` requests type inference
pub const TypedHoleNode = struct {
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

/// Represents an application of type arguments to a type constructor.
/// For example: in `Map k v`, Map is the base type being applied to args k and v.
///
/// Examples:
/// - `List a` applies type variable a to List constructor
/// - `Map k v` applies type variables k and v to Map constructor
/// - `Result e a` applies type variables e and a to Result constructor
/// - `Tree (Maybe a)` applies complex type (Maybe a) to Tree constructor
pub const TypeApplicationNode = struct {
    /// The type constructor being applied (e.g., Map, List, Maybe).
    base: *Node,

    /// The type arguments being applied.
    args: std.ArrayList(*Node),

    /// The token representing the type application.
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

/// The root AST node containing a sequence of top-level declarations like
/// function definitions, type declarations, imports, and module definitions.
pub const ProgramNode = struct {
    /// Array of AST nodes representing top-level declarations.
    statements: std.ArrayList(*Node),

    /// The token representing the start of this declaration.
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
    function_application: FuncApplicationNode,
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
    foreign_function_decl: ForeignFunctionDeclNode,
    function_decl: FunctionDeclNode,
    module_decl: ModuleDeclNode,

    export_spec: ExportSpecNode,
    import_spec: ImportSpecNode,
    include: IncludeNode,
    module_path: ModulePathNode,

    // Types
    function_type: FunctionTypeNode,
    record_type: RecordTypeNode,
    type_alias: TypeAliasNode,
    type_application: TypeApplicationNode,
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
                allocator.destroy(expr.head);
                allocator.destroy(expr.tail);
            },
            .function_application => |*expr| {
                expr.argument.deinit(allocator);
                expr.function.deinit(allocator);
                allocator.destroy(expr.argument);
                allocator.destroy(expr.function);
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
            .foreign_function_decl => |*decl| {
                decl.type_annotation.deinit(allocator);
                allocator.destroy(decl.type_annotation);
                allocator.free(decl.external_name);
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

                for (alias.type_params.items) |param| {
                    allocator.free(param);
                }

                alias.type_params.deinit();

                alias.value.deinit(allocator);
                allocator.destroy(alias.value);
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
                allocator.destroy(self);
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
            .variable,
            => {},
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

test "[BinaryExprNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // 42 + 24

    // Action
    const left = try allocator.create(Node);
    left.* = .{
        .int_literal = .{
            .value = 42,
            .token = lexer.Token{
                .kind = lexer.TokenKind{ .literal = .Int },
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
                .kind = lexer.TokenKind{ .literal = .Int },
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
                .kind = lexer.TokenKind{ .operator = .IntAdd },
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

    // Assertions
    const expr = binary.arithmetic_expr;

    // Verify the operator in the expression is an integer addition operator (+)
    try testing.expectEqual(lexer.TokenKind{ .operator = .IntAdd }, expr.operator.kind);

    // Check the left operand of the binary expression
    try testing.expect(expr.left.int_literal.value == 42);

    // Check the right operand of the binary expression
    try testing.expect(expr.right.int_literal.value == 24);
}

test "[UnaryExprNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // -42

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

    var unary = try allocator.create(Node);
    defer {
        unary.deinit(allocator);
        allocator.destroy(unary);
    }

    unary.* = .{
        .unary_expr = .{
            .operator = lexer.Token{
                .kind = lexer.TokenKind{ .operator = .IntSub },
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

    // Assertions
    const expr = unary.unary_expr;

    // Verify the operator in the unary expression is an integer subtraction operator (-)
    try testing.expectEqual(lexer.TokenKind{ .operator = .IntSub }, expr.operator.kind);

    // Ensure the lexeme for the operator is the string "-"
    try testing.expectEqualStrings("-", expr.operator.lexeme);

    // Verify the operand is an integer literal with the correct token kind
    try testing.expectEqual(lexer.TokenKind{ .literal = .Int }, expr.operand.int_literal.token.kind);

    // Check the value of the integer literal operand
    try testing.expect(expr.operand.int_literal.value == 42);
}

test "[LambdaExprNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // \x y => x + y

    // Action
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
            .operator = lexer.Token{
                .kind = lexer.TokenKind{ .operator = .IntAdd },
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

test "[IfThenElseStmtNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // if x == y then True else False

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
            .operator = lexer.Token{
                .kind = lexer.TokenKind{ .operator = .Equality },
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

test "[ForeignFunctionDeclNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // foreign sqrt : Float -> Float = "c_sqrt"

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
}

test "[FunctionTypeNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // : Int -> Int -> Int

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

    const int_type3 = try allocator.create(Node);
    int_type3.* = .{
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
                .kind = lexer.TokenKind{ .delimiter = .Colon },
                .lexeme = ":",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    // Assertions
    // Check the delimiter in the function type is a colon (:)
    try testing.expectEqual(lexer.TokenKind{ .delimiter = .Colon }, func_type.function_type.token.kind);

    // Verify the lexeme
    try testing.expectEqualStrings(":", func_type.function_type.token.lexeme);

    // Check the function type has exactly three parameter types
    try testing.expectEqual(@as(usize, 3), func_type.function_type.param_types.items.len);

    for (func_type.function_type.param_types.items) |type_node| {
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

test "[FunctionDeclNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // let add : Int -> Int -> Int = \x y => x + y

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
            .operator = lexer.Token{
                .kind = lexer.TokenKind{ .operator = .IntAdd },
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

test "[ConsExprNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // 1 :: [2, 3]

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

    var cons = try allocator.create(Node);
    defer {
        cons.deinit(allocator);
        allocator.destroy(cons);
    }

    cons.* = .{
        .cons_expr = .{
            .head = head,
            .operator = lexer.Token{
                .kind = lexer.TokenKind{ .operator = .Cons },
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

test "[FuncApplicationNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // not True

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

    var func_app = try allocator.create(Node);
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
}

test "[StrConcatExprNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // "Hello" <> "World"

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

    var concat = try allocator.create(Node);
    defer {
        concat.deinit(allocator);
        allocator.destroy(concat);
    }

    concat.* = .{
        .str_concat_expr = .{
            .left = left,
            .operator = lexer.Token{
                .kind = lexer.TokenKind{ .operator = .StrConcat },
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
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // [1, 2] ++ [3, 4]

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

    var concat = try allocator.create(Node);
    defer {
        concat.deinit(allocator);
        allocator.destroy(concat);
    }

    concat.* = .{
        .list_concat_expr = .{
            .left = left,
            .operator = lexer.Token{
                .kind = lexer.TokenKind{ .operator = .ListConcat },
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

    var compose = try allocator.create(Node);
    defer {
        compose.deinit(allocator);
        allocator.destroy(compose);
    }

    {
        // f >> g (compose right)

        compose.* = .{
            .composition_expr = .{
                .first = f,
                .operator = lexer.Token{
                    .kind = lexer.TokenKind{ .operator = .ComposeRight },
                    .lexeme = ">>",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 2, .end = 4 },
                        .src = .{ .line = 1, .col = 3 },
                    },
                },
                .second = g,
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

    {
        // g << f (compose left)

        compose.* = .{
            .composition_expr = .{
                .first = g,
                .operator = lexer.Token{
                    .kind = lexer.TokenKind{ .operator = .ComposeLeft },
                    .lexeme = "<<",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 2, .end = 4 },
                        .src = .{ .line = 1, .col = 3 },
                    },
                },
                .second = f,
            },
        };

        // Assertions
        const expr = compose.composition_expr;

        // Verify the operator in the composition expression is a compose-left operator (<<)
        try testing.expectEqual(lexer.TokenKind{ .operator = .ComposeLeft }, expr.operator.kind);

        // Verify the lexeme of the compose-left operator
        try testing.expectEqualStrings("<<", expr.operator.lexeme);

        // Verify the first function in the composition is a lower identifier
        try testing.expect(expr.first.* == .lower_identifier);

        // Verify the token kind of the first function is a lower identifier
        try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, expr.first.lower_identifier.token.kind);

        // Verify the name of the first function is "g".
        try testing.expectEqualStrings("g", expr.first.lower_identifier.name);

        // Verify the second function in the composition is a lower identifier
        try testing.expect(expr.second.* == .lower_identifier);

        // Verify the token kind of the second function is a lower identifier
        try testing.expectEqual(lexer.TokenKind{ .identifier = .Lower }, expr.second.lower_identifier.token.kind);

        // Verify the name of the second function is "f".
        try testing.expectEqualStrings("f", expr.second.lower_identifier.name);
    }
}

test "[PipeExprNode]" {
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

    var pipe = try allocator.create(Node);
    defer {
        pipe.deinit(allocator);
        allocator.destroy(pipe);
    }

    {
        // x |> f (pipe right)

        pipe.* = .{
            .pipe_expr = .{
                .value = value,
                .operator = lexer.Token{
                    .kind = lexer.TokenKind{ .operator = .PipeRight },
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

    {
        // f <| x (pipe left)

        pipe.* = .{
            .pipe_expr = .{
                .value = value,
                .operator = lexer.Token{
                    .kind = lexer.TokenKind{ .operator = .PipeLeft },
                    .lexeme = "<|",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 2, .end = 4 },
                        .src = .{ .line = 1, .col = 3 },
                    },
                },
                .func = func,
            },
        };

        // Assertions
        const expr = pipe.pipe_expr;

        // Verify the operator in the expression is a pipe-left operator (<|)
        try testing.expectEqual(lexer.TokenKind{ .operator = .PipeLeft }, expr.operator.kind);

        // Verify the lexeme of the pipe-left operator
        try testing.expectEqualStrings("<|", expr.operator.lexeme);

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
}

test "[TypeAliasNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // type alias UserId = String

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

test "[TypeApplicationNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Map k v

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

test "[VariantTypeNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // type Result e a = | Err e | Ok a

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
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // type Point a = { x: a, y: a }

    // Action
    // Create type parameters
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
    const path = node.module_path;

    // Verify the include path consists of exactly two segments
    try testing.expectEqual(@as(usize, 2), path.segments.items.len);

    // Ensure the first segment of the include path is "Std"
    try testing.expectEqualStrings("Std", path.segments.items[0]);

    // Ensure the second segment of the include path is "List"
    try testing.expectEqualStrings("List", path.segments.items[1]);
}

test "[IncludeNode]" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // include Std.List

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
    const include = node.include;

    // Verify the include path consists of exactly two segments
    try testing.expectEqual(@as(usize, 2), include.path.segments.items.len);

    // Ensure the first segment of the include path is "Std"
    try testing.expectEqualStrings("Std", include.path.segments.items[0]);

    // Ensure the second segment of the include path is "List"
    try testing.expectEqualStrings("List", include.path.segments.items[1]);
}

test "[MatchExprNode] (basic literal/constructor)" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // match opt on | Some x => x | None => 0

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
    const match = node.match_expr;

    // Verify the match expression has exactly two cases
    try testing.expectEqual(@as(usize, 2), match.cases.items.len);

    // Ensure the constructor name for the first case is "Some"
    try testing.expectEqualStrings("Some", match.cases.items[0].pattern.constructor.name);

    // Check the argument for the "Some" constructor is a variable named "x"
    try testing.expectEqualStrings("x", match.cases.items[0].pattern.constructor.args.items[0].variable.name);

    // Ensure the constructor name for the second case is "None"
    try testing.expectEqualStrings("None", match.cases.items[1].pattern.constructor.name);

    // Verify the "None" constructor in the second case has no arguments
    try testing.expectEqual(@as(usize, 0), match.cases.items[1].pattern.constructor.args.items.len);
}

test "[MatchExprNode] (list pattern with cons)" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // match list on | head :: tail => head | [] => 0

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
    empty_pattern.* = .{ .empty_list = .{
        .token = .{
            .kind = .{ .delimiter = .LeftBracket },
            .lexeme = "[]",
            .loc = .{
                .filename = TEST_FILE,
                .span = .{ .start = 0, .end = 2 },
                .src = .{ .line = 1, .col = 1 },
            },
        },
    } };

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

test "[MatchExprNode] (with guards)" {
    // Setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // match x on | n when n > 0 => "positive" | n => "non-positive"

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
