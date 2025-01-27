const std = @import("std");
const consts = @import("consts.zig");
const tests = @import("tests.zig");
const httpz = @import("httpz");

pub const ServerSentEventGenerator = @import("ServerSentEventGenerator.zig");

test {
    std.testing.refAllDecls(tests);
}

/// `readSignals` is a helper function that reads datastar signals from the request.
pub fn readSignals(comptime T: type, req: *httpz.Request) !T {
    switch (req.method) {
        .GET => {
            const query = try req.query();
            const signals = query.get(consts.datastar_key) orelse return error.MissingDatastarKey;

            return std.json.parseFromSliceLeaky(T, req.arena, signals, .{});
        },
        else => {
            const body = req.body() orelse return error.MissingBody;

            return std.json.parseFromSliceLeaky(T, req.arena, body, .{});
        },
    }
}
