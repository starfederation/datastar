const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    _ = target;
    _ = optimize;
    const datastar = b.addModule("datastar", .{
        .root_source_file = b.path("src/main.zig"),
    });
    _ = datastar;
}
