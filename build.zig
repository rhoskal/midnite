const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "moxc",
        .root_source_file = .{ .cwd_relative = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);

    const run_step = b.step("run", "Build and run the compiler");
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(&exe.step);
    run_step.dependOn(&run_cmd.step);
}
