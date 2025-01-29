const root = @import("root");
const std = @import("std");
const httpz = @import("httpz");
const datastar = @import("datastar");
const testing = datastar.testing;

fn sdk(req: *httpz.Request, res: *httpz.Response) !void {
    var sse = try root.ServerSentEventGenerator.init(res);
    const signals = try root.readSignals(
        testing.Signals,
        req,
    );

    try testing.sdk(&sse, signals);
}

test sdk {
    var server = try httpz.Server(void).init(
        std.testing.allocator,
        .{ .port = 5882 },
        {},
    );
    defer {
        server.stop();
        server.deinit();
    }

    var router = server.router(.{});

    router.get("/test", sdk, .{});
    router.post("/test", sdk, .{});

    try server.listen();
}
