const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const dep_opts = .{ .target = target, .optimize = optimize };

    const tokamak = b.dependency("tokamak", dep_opts).module("tokamak");
    const datastar = b.dependency("datastar", dep_opts).module("datastar");

    const datastar_tokamak = b.addModule("datastar_tokamak", .{
        .root_source_file = b.path("src/root.zig"),
        .imports = &.{
            .{ .name = "tokamak", .module = tokamak },
            .{ .name = "datastar", .module = datastar },
        },
    });
    _ = datastar_tokamak;

    {
        const tests = b.addTest(.{
            .root_source_file = b.path("src/tests.zig"),
            .target = target,
            .optimize = optimize,
            .test_runner = .{ .path = b.path("../test_runner.zig"), .mode = .simple },
        });

        tests.root_module.addImport("tokamak", tokamak);
        tests.root_module.addImport("datastar", datastar);
        const run_test = b.addRunArtifact(tests);
        run_test.has_side_effects = true;

        const test_step = b.step("test", "Run tests");
        test_step.dependOn(&run_test.step);
    }
}
