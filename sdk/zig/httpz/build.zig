const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const dep_opts = .{ .target = target, .optimize = optimize };

    const httpz = b.dependency("httpz", dep_opts).module("httpz");
    const datastar = b.dependency("datastar", dep_opts).module("datastar");

    const datastar_httpz = b.addModule("datastar_httpz", .{
        .root_source_file = b.path("src/root.zig"),
        .imports = &.{
            .{ .name = "httpz", .module = httpz },
            .{ .name = "datastar", .module = datastar },
        },
    });
    _ = datastar_httpz;

    {
        const tests = b.addTest(.{
            .root_source_file = b.path("src/root.zig"),
            .target = target,
            .optimize = optimize,
            .test_runner = .{ .path = b.path("../test_runner.zig"), .mode = .simple },
        });

        tests.root_module.addImport("httpz", httpz);
        tests.root_module.addImport("datastar", datastar);
        const run_test = b.addRunArtifact(tests);
        run_test.has_side_effects = true;

        const test_step = b.step("test", "Run tests");
        test_step.dependOn(&run_test.step);
    }
}
