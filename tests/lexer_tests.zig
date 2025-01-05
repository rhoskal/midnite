const std = @import("std");
const lexer = @import("../src/lexer.zig");

test "basic lexing" {
    const source = "12 + 34 - 5";
    const tokens = try lexer.tokenize(source);

    try std.testing.expect(tokens[0].kind == .Number);
    try std.testing.expect(tokens[1].kind == .Plus);
    try std.testing.expect(tokens[2].kind == .Number);
    try std.testing.expect(tokens[3].kind == .Minus);
    try std.testing.expect(tokens[4].kind == .Number);
}
