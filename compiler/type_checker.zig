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
    // Constructor: *TypeConstructor,
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
    // Map variable names to their types
    types: std.StringHashMap(Type),

    pub fn init(allocator: std.mem.Allocator) TypeEnvironment {
        return .{
            .types = std.StringHashMap(Type).init(allocator),
        };
    }

    pub fn deinit(self: *TypeEnvironment) void {
        self.types.deinit();
    }
};

/// Performs static type checking on AST nodes to ensure type safety.
/// Verifies that operations are performed on values of compatible types
/// and maintains type information throughout the program.
const TypeChecker = struct {
    allocator: std.mem.Allocator,
    environment: TypeEnvironment,

    /// Creates a new type checker with an empty type environment.
    pub fn init(allocator: std.mem.Allocator) TypeChecker {
        return .{
            .allocator = allocator,
            .environment = TypeEnvironment.init(allocator),
        };
    }

    /// Frees all resources associated with the type checker.
    pub fn deinit(self: *TypeChecker) void {
        self.environment.deinit();
    }

    /// Recursively type checks an AST node and returns its type.
    pub fn checkNode(self: *TypeChecker, node: *ast.Node) TypeCheckError!Type {
        return switch (node.*) {
            .int_literal => .Int,
            .float_literal => .Float,
            .str_literal => .String,
            .char_literal => .Char,
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
                    .OpIntAdd, .OpIntDiv, .OpIntMul, .OpIntSub => {
                        if (left_type != .Int) return error.IntegerOperandRequired;
                        if (right_type != .Int) return error.IntegerOperandRequired;

                        return .Int;
                    },
                    .OpFloatAdd, .OpFloatDiv, .OpFloatMul, .OpFloatSub => {
                        if (left_type != .Float) return error.FloatOperandRequired;
                        if (right_type != .Float) return error.FloatOperandRequired;

                        return .Float;
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
            .lower_identifier => |ident| {
                return self.environment.types.get(ident.name) orelse
                    error.UndefinedVariable;
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

    var node = ast.Node{
        .char_literal = .{
            .value = 'a',
            .token = .{
                .kind = .LitChar,
                .lexeme = "'a'",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 0, .end = 3 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    var checker = TypeChecker.init(allocator);
    const result_type = try checker.checkNode(&node);

    try std.testing.expectEqual(Type.Char, result_type);
}

test "[float_literal]" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var node = ast.Node{
        .float_literal = .{
            .value = 42.0,
            .token = .{
                .kind = .LitFloat,
                .lexeme = "42.0",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 0, .end = 4 },
                    .src = .{ .line = 1, .col = 1 },
                },
            },
        },
    };

    var checker = TypeChecker.init(allocator);
    const result_type = try checker.checkNode(&node);

    try std.testing.expectEqual(Type.Float, result_type);
}

test "[int_literal]" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var node = ast.Node{
        .int_literal = .{
            .value = 42,
            .token = .{
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

    var checker = TypeChecker.init(allocator);
    const result_type = try checker.checkNode(&node);

    try std.testing.expectEqual(Type.Int, result_type);
}

test "[string_literal]" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const loc = lexer.TokenLoc{
        .filename = TEST_FILE,
        .buf = .{ .start = 0, .end = 5 },
        .src = .{ .line = 1, .col = 1 },
    };

    const token = lexer.Token{
        .kind = .LitString,
        .lexeme = "foo",
        .loc = loc,
    };

    var node = ast.Node{
        .str_literal = .{
            .value = "\"foo\"",
            .token = token,
        },
    };

    var checker = TypeChecker.init(allocator);
    const result_type = try checker.checkNode(&node);

    try std.testing.expectEqual(Type.String, result_type);
}

test "[list concat] (non-empty)" {
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
    int2.* = .{
        .int_literal = .{
            .value = 2,
            .token = .{
                .kind = .LitInt,
                .lexeme = "2",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 0, .end = 1 },
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
                .kind = .DelLBrack,
                .lexeme = "[",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 0, .end = 1 },
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
                .kind = .LitInt,
                .lexeme = "3",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 8 },
                },
            },
        },
    };
    int4.* = .{
        .int_literal = .{
            .value = 4,
            .token = .{
                .kind = .LitInt,
                .lexeme = "4",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 0, .end = 1 },
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
                .kind = .DelLBrack,
                .lexeme = "[",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 0, .end = 1 },
                    .src = .{ .line = 1, .col = 7 },
                },
            },
        },
    };

    var concat_node = ast.Node{
        .list_concat_expr = .{
            .left = left_list,
            .operator = .{
                .kind = .OpListConcat,
                .lexeme = "++",
                .loc = .{
                    .filename = TEST_FILE,
                    .buf = .{ .start = 0, .end = 2 },
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

    try std.testing.expect(result_type == .List);
    try std.testing.expect(result_type.List.* == .Int);
}

test "[list concat] (empty)" {
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
                    .kind = .DelLBrack,
                    .lexeme = "[",
                    .loc = .{
                        .filename = TEST_FILE,
                        .buf = .{ .start = 0, .end = 1 },
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
        int2.* = .{
            .int_literal = .{
                .value = 2,
                .token = .{
                    .kind = .LitInt,
                    .lexeme = "2",
                    .loc = .{
                        .filename = TEST_FILE,
                        .buf = .{ .start = 0, .end = 1 },
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
                    .kind = .DelLBrack,
                    .lexeme = "[",
                    .loc = .{
                        .filename = TEST_FILE,
                        .buf = .{ .start = 0, .end = 1 },
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
                    .kind = .OpListConcat,
                    .lexeme = "++",
                    .loc = .{
                        .filename = TEST_FILE,
                        .buf = .{ .start = 0, .end = 2 },
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

        try std.testing.expect(result == .List);
        try std.testing.expect(result.List.* == .Int);
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
                    .kind = .DelLBrack,
                    .lexeme = "[",
                    .loc = .{
                        .filename = TEST_FILE,
                        .buf = .{ .start = 0, .end = 1 },
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
        int2.* = .{
            .int_literal = .{
                .value = 2,
                .token = .{
                    .kind = .LitInt,
                    .lexeme = "2",
                    .loc = .{
                        .filename = TEST_FILE,
                        .buf = .{ .start = 0, .end = 1 },
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
                    .kind = .DelLBrack,
                    .lexeme = "[",
                    .loc = .{
                        .filename = TEST_FILE,
                        .buf = .{ .start = 0, .end = 1 },
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
                    .kind = .OpListConcat,
                    .lexeme = "++",
                    .loc = .{
                        .filename = TEST_FILE,
                        .buf = .{ .start = 0, .end = 2 },
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

        try std.testing.expect(result == .List);
        try std.testing.expect(result.List.* == .Int);
    }
}
