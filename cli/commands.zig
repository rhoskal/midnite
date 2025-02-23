const std = @import("std");

const compiler = @import("compiler");
const formatter = @import("formatter");

pub const Command = union(enum) {
    build: []const u8,
    docs,
    fmt: []const u8,
    init,
    lint,
    lsp,
    repl,
    run,
    @"test",

    pub fn fromString(str: []const u8) ?Command {
        if (std.mem.eql(u8, str, "build")) {
            return Command{ .build = "" };
        }

        if (std.mem.eql(u8, str, "fmt")) {
            return Command{ .fmt = "" };
        }

        // Handle all other commands normally
        const command_type = std.meta.stringToEnum(
            std.meta.Tag(Command),
            str,
        ) orelse return null;

        return switch (command_type) {
            .build => unreachable,
            .docs => Command.docs,
            .fmt => unreachable,
            .init => Command.init,
            .lint => Command.lint,
            .lsp => Command.lsp,
            .repl => Command.repl,
            .run => Command.run,
            .@"test" => Command.@"test",
        };
    }
};

// pub const CommandError = error{
//     UnknownCommand,
//     NotImplemented,
// };

pub fn executeCommand(allocator: std.mem.Allocator, command: Command) !void {
    switch (command) {
        .build => |filepath| {
            const stdout = std.io.getStdOut().writer();

            try compiler.compile(allocator, stdout, filepath);
        },
        .docs => {
            std.debug.print("Command 'docs' not implemented yet\n", .{});
        },
        .fmt => |filepath| {
            const formatted = try formatter.format(allocator, filepath);
            defer allocator.free(formatted);

            try std.fs.cwd().writeFile(.{
                .sub_path = filepath,
                .data = formatted,
                .flags = .{},
            });
        },
        .init => {
            std.debug.print("Command 'init' not implemented yet\n", .{});
        },
        .lint => {
            std.debug.print("Command 'lint' not implemented yet\n", .{});
        },
        .lsp => {
            std.debug.print("Command 'lsp' not implemented yet\n", .{});
        },
        .repl => {
            std.debug.print("Command 'repl' not implemented yet\n", .{});
        },
        .run => {
            std.debug.print("Command 'run' not implemented yet\n", .{});
        },
        .@"test" => {
            std.debug.print("Command 'test' not implemented yet\n", .{});
        },
    }
}

pub const version = "0.1.0";

pub fn printVersion() void {
    std.debug.print("midnite v{s}\n", .{version});
}

pub fn printUsage() void {
    std.debug.print(
        \\Usage: midnite <command>
        \\
        \\Options:
        \\    -h, --help       Print help information
        \\    -V, --version    Print version
        \\
        \\Commands:
        \\    build            Build the project
        \\    docs             Generate documentation
        \\    fmt              Format source code
        \\    init             Create a new project
        \\    lint             Lint source code
        \\    lsp              Start language server
        \\    repl             Start interactive shell
        \\    run              Execute program
        \\    test             Run tests
        \\
        \\
    , .{});
}
