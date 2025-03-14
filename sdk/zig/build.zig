const std = @import("std");

pub const Framework = enum {
    httpz,
    tokamak,
    all,
};

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const dep_opts = .{ .target = target, .optimize = optimize };

    const framework = b.option(
        Framework,
        "framework",
        "Decide which framework to use",
    ) orelse .all;

    const options = b.addOptions();
    options.addOption(bool, "http1", true);
    options.addOption(Framework, "framework", framework);

    const httpz = b.dependency("httpz", dep_opts).module("httpz");
    const tokamak = b.dependency("tokamak", dep_opts).module("tokamak");

    const imports: []const std.Build.Module.Import = switch (framework) {
        .httpz => &.{.{ .name = "httpz", .module = httpz }},
        .tokamak => &.{.{ .name = "tokamak", .module = tokamak }},
        .all => &.{
            .{ .name = "httpz", .module = httpz },
            .{ .name = "tokamak", .module = tokamak },
        },
    };

    const datastar = b.addModule(
        "datastar",
        .{
            .root_source_file = b.path("src/root.zig"),
            .imports = imports,
        },
    );

    datastar.addOptions("config", options);

    const tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
        .test_runner = .{
            .path = b.path("test_runner.zig"),
            .mode = .simple,
        },
    });

    tests.root_module.addOptions("config", options);

    for (imports) |import| {
        tests.root_module.addImport(import.name, import.module);
    }

    const run_test = b.addRunArtifact(tests);
    run_test.has_side_effects = true;

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_test.step);
}
