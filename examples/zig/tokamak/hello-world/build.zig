const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const dep_opts = .{
        .target = target,
        .optimize = optimize,
    };

    const exe = b.addExecutable(.{
        .name = "hello-world",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const tokamak = b.dependency("tokamak", dep_opts).module("tokamak");
    const datastar = b.dependency("datastar", dep_opts).module("datastar");

    exe.root_module.addImport("tokamak", tokamak);
    exe.root_module.addImport("datastar", datastar);
}
