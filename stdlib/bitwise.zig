const std = @import("std");

export fn zig_bitwise_and(a: i64, b: i64) i64 {
    return a & b;
}

export fn zig_bitwise_or(a: i64, b: i64) i64 {
    return a | b;
}

export fn zig_bitwise_not(a: i64) i64 {
    return ~a;
}

export fn zig_bitwise_xor(a: i64, b: i64) i64 {
    return a ^ b;
}

export fn zig_bitwise_shl(a: i64, b: i64) i64 {
    return a << @as(u6, @intCast(b & 63));
}

export fn zig_bitwise_shr(a: i64, b: i64) i64 {
    return a >> @as(u6, @intCast(b & 63));
}

export fn zig_bitwise_lshr(a: u64, b: u6) u64 {
    return a >> b;
}

export fn zig_bitwise_rotl(a: u64, b: u6) u64 {
    return std.math.rotl(u64, a, b);
}

export fn zig_bitwise_rotr(a: u64, b: u6) u64 {
    return std.math.rotr(u64, a, b);
}

export fn zig_bitwise_hamming_weight(a: u64) i64 {
    return @as(i64, @intCast(@popCount(a)));
}

export fn zig_bitwise_distance(a: u64, b: u64) i64 {
    return @as(i64, @intCast(@popCount(a ^ b)));
}
