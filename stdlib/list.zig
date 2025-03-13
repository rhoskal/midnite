const std = @import("std");

pub fn List(comptime T: type) type {
    return struct {
        value: T,
        next: ?*@This(),
    };
}

export fn zig_list_cons(allocator: *std.mem.Allocator, comptime T: type, list: ?*List(T), value: T) ?*List(T) {
    var new_node = allocator.create(List(T)) catch {
        return null;
    };

    new_node.value = value;
    new_node.next = list;

    return new_node;
}

export fn zig_list_size(comptime T: type, list: ?*const List(T)) i64 {
    var count: i64 = 0;
    var current = list;

    while (current) |node| {
        count += 1;
        current = node.next;
    }

    return count;
}

export fn zig_list_range(allocator: *std.mem.Allocator, start: i64, end: i64) ?*List(i64) {
    if (start > end) {
        return null;
    }

    var result: ?*List(i64) = null;

    var i = end;
    while (i >= start) : (i -= 1) {
        var new_node = allocator.create(List(i64)) catch {
            if (result) |r| freeList(i64, r, allocator);
            return null;
        };

        new_node.value = i;
        new_node.next = result;
        result = new_node;
    }

    return result;
}

export fn zig_list_sum(list: ?*const List(i64)) f64 {
    var sum: f64 = 0;
    var current = list;

    while (current) |node| {
        sum += @as(f64, @floatFromInt(node.value));
        current = node.next;
    }

    return sum;
}

fn freeList(allocator: *std.mem.Allocator, comptime T: type, list: ?*List(T)) void {
    var current = list;

    while (current) |node| {
        const to_free = node;
        current = node.next;
        allocator.destroy(to_free);
    }
}
