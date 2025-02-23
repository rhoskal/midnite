const std = @import("std");

const commands = @import("commands.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    // Skip the program name
    _ = args.skip();

    if (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            commands.printUsage();

            return;
        }

        if (std.mem.eql(u8, arg, "-V") or std.mem.eql(u8, arg, "--version")) {
            commands.printVersion();

            return;
        }

        if (std.mem.eql(u8, arg, "build")) {
            const filepath = args.next() orelse {
                std.debug.print("Error: build command requires a filepath\n", .{});
                std.debug.print("Usage: midnite build <filepath>\n", .{});
                std.process.exit(1);
            };

            const cmd = commands.Command{ .build = filepath };
            try commands.executeCommand(allocator, cmd);

            return;
        }

        if (std.mem.eql(u8, arg, "fmt")) {
            const filepath = args.next() orelse {
                std.debug.print("Error: fmt command requires a filepath\n", .{});
                std.debug.print("Usage: midnite fmt <filepath>\n", .{});
                std.process.exit(1);
            };

            const cmd = commands.Command{ .fmt = filepath };
            try commands.executeCommand(allocator, cmd);

            return;
        }

        const cmd = commands.Command.fromString(arg) orelse {
            std.debug.print("Unknown command: {s}\n", .{arg});
            commands.printUsage();

            std.process.exit(1);
        };

        try commands.executeCommand(allocator, cmd);

        return;
    } else {
        commands.printUsage();
    }
}
