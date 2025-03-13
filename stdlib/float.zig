const std = @import("std");

export fn zig_float_pow(x: f64, y: f64) f64 {
    return std.math.pow(f64, x, y);
}

export fn zig_float_mod(x: f64, y: f64) f64 {
    return @mod(x, y);
}

export fn zig_float_sqrt(x: f64) f64 {
    return std.math.sqrt(x);
}

export fn zig_float_log(base: f64, x: f64) f64 {
    return std.math.log2(x) / std.math.log2(base);
}

export fn zig_float_e() f64 {
    return std.math.e;
}

export fn zig_float_pi() f64 {
    return std.math.pi;
}

export fn zig_float_cos(x: f64) f64 {
    return std.math.cos(x);
}

export fn zig_float_sin(x: f64) f64 {
    return std.math.sin(x);
}

export fn zig_float_tan(x: f64) f64 {
    return std.math.tan(x);
}

export fn zig_float_acos(x: f64) f64 {
    return std.math.acos(x);
}

export fn zig_float_asin(x: f64) f64 {
    return std.math.asin(x);
}

export fn zig_float_atan(x: f64) f64 {
    return std.math.atan(x);
}

export fn zig_float_atan2(y: f64, x: f64) f64 {
    return std.math.atan2(y, x);
}

export fn zig_float_trunc(x: f64) i64 {
    return @intFromFloat(x);
}

export fn zig_float_ceil(x: f64) i64 {
    return @intFromFloat(std.math.ceil(x));
}

export fn zig_float_floor(x: f64) i64 {
    return @intFromFloat(std.math.floor(x));
}

export fn zig_float_round(x: f64) i64 {
    return @intFromFloat(std.math.round(x));
}

export fn zig_float_cosh(x: f64) f64 {
    return std.math.cosh(x);
}

export fn zig_float_sinh(x: f64) f64 {
    return std.math.sinh(x);
}

export fn zig_float_tanh(x: f64) f64 {
    return std.math.tanh(x);
}

export fn zig_float_coth(x: f64) f64 {
    return 1.0 / std.math.tanh(x);
}

export fn zig_float_sech(x: f64) f64 {
    return 1.0 / std.math.cosh(x);
}

export fn zig_float_cosech(x: f64) f64 {
    return 1.0 / std.math.sinh(x);
}

export fn zig_float_acosh(x: f64) f64 {
    return std.math.acosh(x);
}

export fn zig_float_asinh(x: f64) f64 {
    return std.math.asinh(x);
}

export fn zig_float_atanh(x: f64) f64 {
    return std.math.atanh(x);
}

export fn zig_float_acoth(x: f64) f64 {
    return 0.5 * std.math.log((x + 1) / (x - 1));
}

export fn zig_float_asech(x: f64) f64 {
    return std.math.acosh(1 / x);
}

export fn zig_float_acosech(x: f64) f64 {
    return std.math.asinh(1 / x);
}

export fn zig_float_min() f64 {
    return std.math.floatMin(f64);
}

export fn zig_float_max() f64 {
    return std.math.floatMax(f64);
}

export fn zig_float_from_int(x: i64) f64 {
    return @floatFromInt(x);
}
