const std = @import("std");

/// Calculate the number of characters in a string.
export fn zig_string_size(str_ptr: [*]const u8, len: usize) usize {
    // For UTF-8 strings, we need to count code points, not bytes
    var i: usize = 0;
    var count: usize = 0;

    while (i < len) {
        // Skip continuation bytes (10xxxxxx)
        if ((str_ptr[i] & 0xC0) != 0x80) {
            count += 1;
        }

        i += 1;
    }

    return count;
}

/// Append two strings.
export fn zig_string_append(allocator: *anyopaque, str1_ptr: [*]const u8, str1_len: usize, str2_ptr: [*]const u8, str2_len: usize, out_len: *usize) [*]u8 {
    const alloc = @as(*std.mem.Allocator, @ptrCast(allocator));
    const total_len = str1_len + str2_len;

    const result = alloc.alloc(u8, total_len) catch @panic("OOM");

    @memcpy(result[0..str1_len], str1_ptr[0..str1_len]);
    @memcpy(result[str1_len..total_len], str2_ptr[0..str2_len]);

    out_len.* = total_len;

    return result.ptr;
}

/// Take a substring given a start and end index.
export fn zig_string_slice(allocator: *anyopaque, str_ptr: [*]const u8, str_len: usize, start: usize, end: usize, out_len: *usize) [*]u8 {
    const alloc = @as(*std.mem.Allocator, @ptrCast(allocator));

    // Bounds checking
    const real_start = if (start > str_len) str_len else start;
    const real_end = if (end > str_len) str_len else end;

    if (real_start >= real_end) {
        out_len.* = 0;
        return alloc.alloc(u8, 0) catch @panic("OOM");
    }

    const slice_len = real_end - real_start;
    const result = alloc.alloc(u8, slice_len) catch @panic("OOM");

    @memcpy(result, str_ptr[real_start..real_end]);

    out_len.* = slice_len;
    return result.ptr;
}

/// Convert a string to all lower case.
export fn zig_string_to_lower(allocator: *anyopaque, str_ptr: [*]const u8, str_len: usize, out_len: *usize) [*]u8 {
    const alloc = @as(*std.mem.Allocator, @ptrCast(allocator));

    const result = alloc.alloc(u8, str_len) catch @panic("OOM");
    out_len.* = str_len;

    for (0..str_len) |i| {
        const c = str_ptr[i];
        result[i] = if (c >= 'A' and c <= 'Z') c + 32 else c;
    }

    return result.ptr;
}

/// Convert a string to all upper case.
export fn zig_string_to_upper(allocator: *anyopaque, str_ptr: [*]const u8, str_len: usize, out_len: *usize) [*]u8 {
    const alloc = @as(*std.mem.Allocator, @ptrCast(allocator));

    const result = alloc.alloc(u8, str_len) catch @panic("OOM");
    out_len.* = str_len;

    for (0..str_len) |i| {
        const c = str_ptr[i];
        result[i] = if (c >= 'a' and c <= 'z') c - 32 else c;
    }

    return result.ptr;
}

/// Get character at the given position.
export fn zig_string_at(str_ptr: [*]const u8, str_len: usize, position: usize) i32 {
    if (position >= str_len) {
        return -1; // out of bounds
    }

    return @as(i32, str_ptr[position]);
}

/// Test whether a string is empty.
export fn zig_string_empty(str_len: usize) bool {
    return str_len == 0;
}

/// Reverse a string.
export fn zig_string_reverse(allocator: *anyopaque, str_ptr: [*]const u8, str_len: usize, out_len: *usize) [*]u8 {
    const alloc = @as(*std.mem.Allocator, @ptrCast(allocator));

    const result = alloc.alloc(u8, str_len) catch @panic("OOM");
    out_len.* = str_len;

    for (0..str_len) |i| {
        result[i] = str_ptr[str_len - i - 1];
    }

    return result.ptr;
}

/// Concatenate many strings into one.
export fn zig_string_concat(allocator: *anyopaque, strs_ptr: [*][*]const u8, lens_ptr: [*]const usize, count: usize, out_len: *usize) [*]u8 {
    const alloc = @as(*std.mem.Allocator, @ptrCast(allocator));

    // Calculate total length
    var total_len: usize = 0;
    for (0..count) |i| {
        total_len += lens_ptr[i];
    }

    const result = alloc.alloc(u8, total_len) catch @panic("OOM");
    out_len.* = total_len;

    var pos: usize = 0;
    for (0..count) |i| {
        const str = strs_ptr[i];
        const len = lens_ptr[i];
        @memcpy(result[pos .. pos + len], str[0..len]);
        pos += len;
    }

    return result.ptr;
}

/// Returns `True` if string starts with the given pattern.
export fn zig_string_starts_with(str_ptr: [*]const u8, str_len: usize, pattern_ptr: [*]const u8, pattern_len: usize) bool {
    if (pattern_len > str_len) return false;

    for (0..pattern_len) |i| {
        if (str_ptr[i] != pattern_ptr[i]) {
            return false;
        }
    }

    return true;
}

/// Returns `True` if string ends with the given pattern.
export fn zig_string_ends_with(str_ptr: [*]const u8, str_len: usize, pattern_ptr: [*]const u8, pattern_len: usize) bool {
    if (pattern_len > str_len) return false;

    const start = str_len - pattern_len;
    for (0..pattern_len) |i| {
        if (str_ptr[start + i] != pattern_ptr[i]) {
            return false;
        }
    }

    return true;
}
