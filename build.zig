const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "mox",
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
}
