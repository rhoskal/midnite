const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");

/// Represents the possible types in the language.
pub const Type = union(enum) {
    Char,
    Float,
    Int,
    String,
    List: *Type,
    // Function: *FunctionType,
    // Variable: *TypeVariable,
};

/// Represents a variant (sum) type with a name and set of constructors.
/// Each constructor can have zero or more parameter types.
///
/// Examples:
/// - `type Boolean = | True | False`
/// - `type Maybe a = | None | Some a`
const VariantType = struct {
    /// The name of the variant type (e.g. "Boolean", "Maybe").
    name: []const u8,

    /// Maps constructor names to their parameter types
    /// For example, in `Maybe a = None | Some a`:
    /// - "None" maps to an empty list
    /// - "Some" maps to a list containing the type parameter `a`
    constructors: std.StringHashMap(std.ArrayList(Type)),

    /// Memory allocator used for managing constructor data.
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, name: []const u8) VariantType {
        return .{
            .name = name,
            .constructors = std.StringHashMap(std.ArrayList(Type)).init(allocator),
            .allocator = allocator,
        };
    }

};

/// Errors that can occur during type checking.
pub const TypeCheckError = error{
    FloatOperandRequired,
    IntegerOperandRequired,
    ListElementTypeMismatch,
    ListOperandRequired,
    OutOfMemory,
    StringOperandRequired,
    TypeMismatch,
    UndefinedVariable,
    Unimplemented,
};

/// Maintains a mapping of variables to their types during type checking.
/// Acts as a symbol table for type information in the current scope.
const TypeEnvironment = struct {
    /// Maps variable names to their corresponding types
    /// For example: "x" -> Int, "name" -> String
    types: std.StringHashMap(Type),

    /// Memory allocator used for managing the type mapping.
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) TypeEnvironment {
        return .{
            .types = std.StringHashMap(Type).init(allocator),
            .allocator = allocator,
        };
    }

    /// Frees all resources associated with the type environment.
    pub fn deinit(self: *TypeEnvironment) void {
        self.types.deinit();
    }
};

/// Performs static type checking on AST nodes to ensure type safety.
/// Verifies that operations are performed on values of compatible types
/// and maintains type information throughout the program.
const TypeChecker = struct {
    /// The type environment storing variable -> type mappings for the current scope.
    environment: TypeEnvironment,

    /// Memory allocator used for creating types and managing type checker data.
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) TypeChecker {
        return .{
            .environment = TypeEnvironment.init(allocator),
            .allocator = allocator,
        };
    }

    /// Frees all resources associated with the type checker.
    pub fn deinit(self: *TypeChecker) void {
        self.environment.deinit();
    }

    /// Recursively type checks an AST node and returns its type.
    pub fn checkNode(self: *TypeChecker, node: *ast.Node) TypeCheckError!Type {
        return switch (node.*) {
            .char_literal => .Char,
            .float_literal => .Float,
            .int_literal => .Int,
            .str_literal => .String,
            .list => |list| {
                if (list.elements.items.len == 0) {
                    // For empty list, create a new List type with a dummy element type.
                    // This will be unified later during list operations.
                    const element_type = try self.allocator.create(Type);
                    element_type.* = .Int;

                    return Type{ .List = element_type };
                }

                const first_type = try self.checkNode(list.elements.items[0]);

                for (list.elements.items[1..]) |element| {
                    const element_type = try self.checkNode(element);

                    if (!std.meta.eql(element_type, first_type)) {
                        return error.TypeMismatch;
                    }
                }

                const element_type = try self.allocator.create(Type);
                element_type.* = first_type;

                return Type{ .List = element_type };
            },
            .arithmetic_expr => |expr| {
                const left_type = try self.checkNode(expr.left);
                const right_type = try self.checkNode(expr.right);

                switch (expr.operator.kind) {
                    .operator => |op| switch (op) {
                        .IntAdd,
                        .IntDiv,
                        .IntMul,
                        .IntSub,
                        => {
                            if (left_type != .Int) return error.IntegerOperandRequired;
                            if (right_type != .Int) return error.IntegerOperandRequired;

                            return .Int;
                        },
                        .FloatAdd,
                        .FloatDiv,
                        .FloatMul,
                        .FloatSub,
                        => {
                            if (left_type != .Float) return error.FloatOperandRequired;
                            if (right_type != .Float) return error.FloatOperandRequired;

                            return .Float;
                        },
                        else => unreachable,
                    },
                    else => unreachable,
                }
            },
            .list_concat_expr => |expr| {
                const left_type = try self.checkNode(expr.left);
                const right_type = try self.checkNode(expr.right);

                if (left_type != .List) return error.ListOperandRequired;
                if (right_type != .List) return error.ListOperandRequired;

                const result_type = try self.allocator.create(Type);
                errdefer self.allocator.destroy(result_type);

                if (expr.left.list.elements.items.len == 0) {
                    result_type.* = right_type.List.*;

                    self.allocator.destroy(left_type.List);
                    self.allocator.destroy(right_type.List);

                    return Type{ .List = result_type };
                }

                if (expr.right.list.elements.items.len == 0) {
                    result_type.* = left_type.List.*;

                    self.allocator.destroy(left_type.List);
                    self.allocator.destroy(right_type.List);

                    return Type{ .List = result_type };
                }

                if (!std.meta.eql(left_type.List.*, right_type.List.*)) {
                    return error.ListElementTypeMismatch;
                }

                result_type.* = left_type.List.*;

                self.allocator.destroy(left_type.List);
                self.allocator.destroy(right_type.List);

                return Type{ .List = result_type };
            },
            .str_concat_expr => |expr| {
                const left_type = try self.checkNode(expr.left);
                const right_type = try self.checkNode(expr.right);

                if (left_type != .String) return error.StringOperandRequired;
                if (right_type != .String) return error.StringOperandRequired;

                return .String;
            },
            .unary_expr => |expr| {
                const operand_type = try self.checkNode(expr.operand);

                switch (expr.operator.kind) {
                    .operator => |op| switch (op) {
                        .IntSub => {
                            return switch (operand_type) {
                                .Int => .Int,
                                .Float => .Float,
                                else => unreachable,
                            };
                        },
                        else => unreachable,
                    },
                    else => unreachable,
                }
            },
            .logical_expr => |expr| {
                const left_type = try self.checkNode(expr.left);
                const right_type = try self.checkNode(expr.right);

                if (left_type != .Variant or right_type != .Variant) {
                    return error.TypeMismatch;
                }

                if (!std.mem.eql(u8, left_type.Variant.name, right_type.Variant.name)) {
                    return error.TypeMismatch;
                }

                return left_type;
            },
            .variant_type => |vtype| {
                const type_name = vtype.name;

                // Create variant type in environment
                var variant = try self.allocator.create(VariantType);
                std.debug.print("Created VariantType at {*}\n", .{variant});
                std.debug.print("Number of constructors: {d}\n", .{vtype.constructors.items.len});
                variant.* = VariantType.init(self.allocator, type_name);

                // Register each constructor
                for (vtype.constructors.items) |constructor| {
                    if (constructor.params.items.len > 0) {
                        var param_types = try self.allocator.alloc(Type, constructor.params.items.len);

                        for (constructor.params.items, 0..) |param, i| {
                            param_types[i] = try self.checkNode(param);
                        }

                        try variant.addConstructorTypes(constructor.name, param_types);
                    } else {
                        const empty_params = try self.allocator.alloc(Type, 0);
                        try variant.addConstructorTypes(constructor.name, empty_params);
                    }
                }

                const variant_type = Type{ .Variant = variant };
                try self.environment.types.put(type_name, variant_type);

                return variant_type;
            },
            .lower_identifier => |ident| {
                return self.environment.types.get(ident.name) orelse
                    error.UndefinedVariable;
            },
            .upper_identifier => |ident| {
                return (self.environment.types.get(ident.name)) orelse
                    error.UndefinedConstructor;
            },
            else => error.Unimplemented,
        };
    }
};

const testing = std.testing;

const TEST_FILE = "test.mox";

test "[char_literal]" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const node = try allocator.create(ast.Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
        .char_literal = .{
            .value = 'a',
            .token = .{
                .kind = .{ .literal = .Char },
                .lexeme = "'a'",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 3 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    var checker = TypeChecker.init(allocator);
    defer checker.deinit();

    const result_type = try checker.checkNode(node);

    try testing.expectEqual(.Char, result_type);
}

test "[float_literal]" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const node = try allocator.create(ast.Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
        .float_literal = .{
            .value = 42.0,
            .token = .{
                .kind = .{ .literal = .Float },
                .lexeme = "42.0",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 4 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    var checker = TypeChecker.init(allocator);
    defer checker.deinit();

    const result_type = try checker.checkNode(node);

    try testing.expectEqual(.Float, result_type);
}

test "[int_literal]" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const node = try allocator.create(ast.Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
        .int_literal = .{
            .value = 42,
            .token = .{
                .kind = .{ .literal = .Int },
                .lexeme = "42",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 2 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    var checker = TypeChecker.init(allocator);
    defer checker.deinit();

    const result_type = try checker.checkNode(node);

    try testing.expectEqual(.Int, result_type);
}

test "[string_literal]" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const node = try allocator.create(ast.Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
        .str_literal = .{
            .value = try allocator.dupe(u8, "foo"),
            .token = .{
                .kind = .{ .literal = .String },
                .lexeme = "foo",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 5 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    var checker = TypeChecker.init(allocator);
    defer checker.deinit();

    const result_type = try checker.checkNode(node);

    try testing.expectEqual(.String, result_type);
}

test "[arithmetic_expr]" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        // 42 + 24

        const int1 = try allocator.create(ast.Node);
        const int2 = try allocator.create(ast.Node);
        int1.* = .{
            .int_literal = .{
                .value = 42,
                .token = .{
                    .kind = .{ .literal = .Int },
                    .lexeme = "42",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 2 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };
        int2.* = .{
            .int_literal = .{
                .value = 24,
                .token = .{
                    .kind = .{ .literal = .Int },
                    .lexeme = "24",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 5, .end = 7 },
                        .src = .{ .line = 1, .col = 6 },
                    },
                },
            },
        };

        const node = try allocator.create(ast.Node);
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        node.* = .{
            .arithmetic_expr = .{
                .left = int1,
                .operator = .{
                    .kind = .{ .operator = .IntAdd },
                    .lexeme = "+",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 3, .end = 4 },
                        .src = .{ .line = 1, .col = 4 },
                    },
                },
                .right = int2,
            },
        };

        var checker = TypeChecker.init(allocator);
        defer checker.deinit();

        const result_type = try checker.checkNode(node);

        try testing.expectEqual(.Int, result_type);
    }

    {
        // 42.0 +. 24.0

        const float1 = try allocator.create(ast.Node);
        const float2 = try allocator.create(ast.Node);
        float1.* = .{
            .float_literal = .{
                .value = 42.0,
                .token = .{
                    .kind = .{ .literal = .Float },
                    .lexeme = "42.0",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 4 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };
        float2.* = .{
            .float_literal = .{
                .value = 24.0,
                .token = .{
                    .kind = .{ .literal = .Float },
                    .lexeme = "24.0",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 8, .end = 12 },
                        .src = .{ .line = 1, .col = 9 },
                    },
                },
            },
        };

        const node = try allocator.create(ast.Node);
        defer {
            node.deinit(allocator);
            allocator.destroy(node);
        }

        node.* = .{
            .arithmetic_expr = .{
                .left = float1,
                .operator = .{
                    .kind = .{ .operator = .FloatAdd },
                    .lexeme = "+.",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 5, .end = 7 },
                        .src = .{ .line = 1, .col = 6 },
                    },
                },
                .right = float2,
            },
        };

        var checker = TypeChecker.init(allocator);
        defer checker.deinit();

        const result_type = try checker.checkNode(node);

        try testing.expectEqual(.Float, result_type);
    }
}

test "[arithmetic_expr] (error.IntegerOperandRequired)" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // 42.0 + 24.0

    const float1 = try allocator.create(ast.Node);
    const float2 = try allocator.create(ast.Node);
    float1.* = .{
        .float_literal = .{
            .value = 42.0,
            .token = .{
                .kind = .{ .literal = .Float },
                .lexeme = "42.0",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 4 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };
    float2.* = .{
        .float_literal = .{
            .value = 24.0,
            .token = .{
                .kind = .{ .literal = .Float },
                .lexeme = "24.0",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 7, .end = 11 },
                    .src = .{ .line = 1, .col = 8 },
                },
            },
        },
    };

    const node = try allocator.create(ast.Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
        .arithmetic_expr = .{
            .left = float1,
            .operator = .{
                .kind = .{ .operator = .IntAdd },
                .lexeme = "+",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 5, .end = 6 },
                    .src = .{ .line = 1, .col = 6 },
                },
            },
            .right = float2,
        },
    };

    var checker = TypeChecker.init(allocator);
    defer checker.deinit();

    try testing.expectError(error.IntegerOperandRequired, checker.checkNode(node));
}

test "[arithmetic_expr] (error.FloatOperandRequired)" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // 42 +. 24

    const int1 = try allocator.create(ast.Node);
    const int2 = try allocator.create(ast.Node);
    int1.* = .{
        .int_literal = .{
            .value = 42,
            .token = .{
                .kind = .{ .literal = .Int },
                .lexeme = "42",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 2 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };
    int2.* = .{
        .int_literal = .{
            .value = 24,
            .token = .{
                .kind = .{ .literal = .Int },
                .lexeme = "24",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 7, .end = 9 },
                    .src = .{ .line = 1, .col = 8 },
                },
            },
        },
    };

    const node = try allocator.create(ast.Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
        .arithmetic_expr = .{
            .left = int1,
            .operator = .{
                .kind = .{ .operator = .FloatAdd },
                .lexeme = "+.",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 3, .end = 5 },
                    .src = .{ .line = 1, .col = 4 },
                },
            },
            .right = int2,
        },
    };

    var checker = TypeChecker.init(allocator);
    defer checker.deinit();

    try testing.expectError(error.FloatOperandRequired, checker.checkNode(node));
}

test "[list_concat_expr] (non-empty)" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Create [1, 2]
    var left_elements = std.ArrayList(*ast.Node).init(allocator);
    defer {
        for (left_elements.items) |node| {
            allocator.destroy(node);
        }

        left_elements.deinit();
    }

    const int1 = try allocator.create(ast.Node);
    const int2 = try allocator.create(ast.Node);

    int1.* = .{
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
    int2.* = .{
        .int_literal = .{
            .value = 2,
            .token = .{
                .kind = .{ .literal = .Int },
                .lexeme = "2",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 3 },
                },
            },
        },
    };

    try left_elements.append(int1);
    try left_elements.append(int2);

    const left_list = try allocator.create(ast.Node);
    defer allocator.destroy(left_list);

    left_list.* = .{
        .list = .{
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
        },
    };

    // Create [3, 4]
    var right_elements = std.ArrayList(*ast.Node).init(allocator);
    defer {
        for (right_elements.items) |node| {
            allocator.destroy(node);
        }

        right_elements.deinit();
    }

    const int3 = try allocator.create(ast.Node);
    const int4 = try allocator.create(ast.Node);

    int3.* = .{
        .int_literal = .{
            .value = 3,
            .token = .{
                .kind = .{ .literal = .Int },
                .lexeme = "3",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 8 },
                },
            },
        },
    };
    int4.* = .{
        .int_literal = .{
            .value = 4,
            .token = .{
                .kind = .{ .literal = .Int },
                .lexeme = "4",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 10 },
                },
            },
        },
    };

    try right_elements.append(int3);
    try right_elements.append(int4);

    const right_list = try allocator.create(ast.Node);
    defer allocator.destroy(right_list);

    right_list.* = .{
        .list = .{
            .elements = right_elements,
            .token = .{
                .kind = .{ .delimiter = .LeftBracket },
                .lexeme = "[",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 7 },
                },
            },
        },
    };

    var concat_node = ast.Node{
        .list_concat_expr = .{
            .left = left_list,
            .operator = .{
                .kind = .{ .operator = .ListConcat },
                .lexeme = "++",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 2 },
                    .src = .{ .line = 1, .col = 5 },
                },
            },
            .right = right_list,
        },
    };

    var checker = TypeChecker.init(allocator);
    defer checker.deinit();

    const result_type = try checker.checkNode(&concat_node);
    defer if (result_type == .List) allocator.destroy(result_type.List);

    try testing.expect(result_type == .List);
    try testing.expect(result_type.List.* == .Int);
}

test "[list_concat_expr] (empty)" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    {
        var empty_elements = std.ArrayList(*ast.Node).init(allocator);
        defer empty_elements.deinit();

        const empty_list = try allocator.create(ast.Node);
        defer allocator.destroy(empty_list);

        empty_list.* = .{
            .list = .{
                .elements = empty_elements,
                .token = .{
                    .kind = .{ .delimiter = .LeftBracket },
                    .lexeme = "[",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 1 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        var nonempty_elements = std.ArrayList(*ast.Node).init(allocator);
        defer nonempty_elements.deinit();

        const int1 = try allocator.create(ast.Node);
        const int2 = try allocator.create(ast.Node);
        defer {
            allocator.destroy(int1);
            allocator.destroy(int2);
        }

        int1.* = .{
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
        int2.* = .{
            .int_literal = .{
                .value = 2,
                .token = .{
                    .kind = .{ .literal = .Int },
                    .lexeme = "2",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 1 },
                        .src = .{ .line = 1, .col = 3 },
                    },
                },
            },
        };

        try nonempty_elements.append(int1);
        try nonempty_elements.append(int2);

        const nonempty_list = try allocator.create(ast.Node);
        defer allocator.destroy(nonempty_list);

        nonempty_list.* = .{
            .list = .{
                .elements = nonempty_elements,
                .token = .{
                    .kind = .{ .delimiter = .LeftBracket },
                    .lexeme = "[",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 1 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        // Test empty ++ non-empty
        var concat_node = ast.Node{
            .list_concat_expr = .{
                .left = empty_list,
                .operator = .{
                    .kind = .{ .operator = .ListConcat },
                    .lexeme = "++",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 2 },
                        .src = .{ .line = 1, .col = 5 },
                    },
                },
                .right = nonempty_list,
            },
        };

        var checker = TypeChecker.init(allocator);
        defer checker.deinit();

        const result = try checker.checkNode(&concat_node);
        defer if (result == .List) allocator.destroy(result.List);

        try testing.expect(result == .List);
        try testing.expect(result.List.* == .Int);
    }

    {
        var empty_elements = std.ArrayList(*ast.Node).init(allocator);
        defer empty_elements.deinit();

        const empty_list = try allocator.create(ast.Node);
        defer allocator.destroy(empty_list);

        empty_list.* = .{
            .list = .{
                .elements = empty_elements,
                .token = .{
                    .kind = .{ .delimiter = .LeftBracket },
                    .lexeme = "[",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 1 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        var nonempty_elements = std.ArrayList(*ast.Node).init(allocator);
        defer nonempty_elements.deinit();

        const int1 = try allocator.create(ast.Node);
        const int2 = try allocator.create(ast.Node);
        defer {
            allocator.destroy(int1);
            allocator.destroy(int2);
        }

        int1.* = .{
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
        int2.* = .{
            .int_literal = .{
                .value = 2,
                .token = .{
                    .kind = .{ .literal = .Int },
                    .lexeme = "2",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 1 },
                        .src = .{ .line = 1, .col = 3 },
                    },
                },
            },
        };

        try nonempty_elements.append(int1);
        try nonempty_elements.append(int2);

        const nonempty_list = try allocator.create(ast.Node);
        defer allocator.destroy(nonempty_list);

        nonempty_list.* = .{
            .list = .{
                .elements = nonempty_elements,
                .token = .{
                    .kind = .{ .delimiter = .LeftBracket },
                    .lexeme = "[",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 1 },
                        .src = .{ .line = 1, .col = 1 },
                    },
                },
            },
        };

        // Test non-empty ++ empty
        var concat_node = ast.Node{
            .list_concat_expr = .{
                .left = nonempty_list,
                .operator = .{
                    .kind = .{ .operator = .ListConcat },
                    .lexeme = "++",
                    .loc = .{
                        .filename = TEST_FILE,
                        .span = .{ .start = 0, .end = 2 },
                        .src = .{ .line = 1, .col = 5 },
                    },
                },
                .right = empty_list,
            },
        };

        var checker = TypeChecker.init(allocator);
        defer checker.deinit();

        const result = try checker.checkNode(&concat_node);
        defer if (result == .List) allocator.destroy(result.List);

        try testing.expect(result == .List);
        try testing.expect(result.List.* == .Int);
    }
}

test "[str_concat_expr]" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // "foo" <> "bar"

    const str1 = try allocator.create(ast.Node);
    const str2 = try allocator.create(ast.Node);
    str1.* = .{
        .str_literal = .{
            .value = try allocator.dupe(u8, "foo"),
            .token = .{
                .kind = .{ .literal = .String },
                .lexeme = "foo",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 0, .end = 5 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };
    str2.* = .{
        .str_literal = .{
            .value = try allocator.dupe(u8, "bar"),
            .token = .{
                .kind = .{ .literal = .String },
                .lexeme = "bar",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 10, .end = 15 },
                    .src = .{ .line = 1, .col = 11 },
                },
            },
        },
    };

    const node = try allocator.create(ast.Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
        .str_concat_expr = .{
            .left = str1,
            .operator = lexer.Token{
                .kind = .{ .operator = .StrConcat },
                .lexeme = "<>",
                .loc = .{
                    .filename = TEST_FILE,
                    .span = .{ .start = 6, .end = 8 },
                    .src = .{ .line = 1, .col = 7 },
                },
            },
            .right = str2,
        },
    };

    var checker = TypeChecker.init(allocator);
    defer checker.deinit();

    const result_type = try checker.checkNode(node);

    try testing.expectEqual(.String, result_type);
}

test "[unary_expr]" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const operand = try allocator.create(ast.Node);
    errdefer allocator.destroy(operand);

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

    const node = try allocator.create(ast.Node);
    errdefer allocator.destroy(node);

    node.* = .{
        .unary_expr = .{
            .operator = .{
                .kind = .{ .operator = .IntSub },
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

    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    var checker = TypeChecker.init(allocator);
    defer checker.deinit();

    const result_type = try checker.checkNode(node);

    try testing.expectEqual(.Int, result_type);
}
test "[lower_identifier]" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const node = try allocator.create(ast.Node);
    defer {
        node.deinit(allocator);
        allocator.destroy(node);
    }

    node.* = .{
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

    var checker = TypeChecker.init(allocator);
    defer checker.deinit();

    // Test undefined variable
    try testing.expectError(error.UndefinedVariable, checker.checkNode(node));

    // Add variable to environment and test lookup
    try checker.environment.types.put("x", .Int);
    const result_type = try checker.checkNode(node);
    try testing.expectEqual(.Int, result_type);
}

