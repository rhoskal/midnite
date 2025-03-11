const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "midnite",
        .root_source_file = b.path("cli/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const compiler_module = b.addModule("compiler", .{
        .root_source_file = b.path("compiler/root.zig"),
    });

    const formatter_module = b.addModule("formatter", .{
        .root_source_file = b.path("formatter/root.zig"),
    });
    formatter_module.addImport("compiler", compiler_module);

    exe.root_module.addImport("compiler", compiler_module);
    exe.root_module.addImport("formatter", formatter_module);

    b.installArtifact(exe);

    const run_exe = b.addRunArtifact(exe);

    const run_step = b.step("run", "Run the compiler");
    run_step.dependOn(&run_exe.step);

    const test_step = b.step("test", "Run compiler/frontend tests");

    const frontend_test_files = [_][]const u8{
        "compiler/frontend/ast.zig",
        "compiler/frontend/lexer.zig",
        "compiler/frontend/parser.zig",
    };

    for (frontend_test_files) |file| {
        const test_file = b.addTest(.{
            .root_source_file = b.path(file),
            .target = target,
            .optimize = optimize,
        });

        test_file.root_module.addImport("compiler", compiler_module);
        test_step.dependOn(&test_file.step);
    }
}
