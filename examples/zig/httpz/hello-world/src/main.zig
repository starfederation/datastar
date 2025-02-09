const std = @import("std");
const httpz = @import("httpz");
const datastar = @import("datastar");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var server = try httpz.Server(void).init(
        allocator,
        .{ .port = 8080 },
        {},
    );
    defer {
        server.stop();
        server.deinit();
    }

    var router = server.router(.{});

    router.get("/", index, .{});
    router.get("/hello-world", helloWorld, .{});

    try server.listen();
}

fn index(_: *httpz.Request, res: *httpz.Response) !void {
    res.content_type = .HTML;
    res.body = @embedFile("hello-world.html");
}

const Signals = struct {
    delay: u64,
};

const message = "Hello, world!";

fn helloWorld(req: *httpz.Request, res: *httpz.Response) !void {
    var sse = try datastar.ServerSentEventGenerator.init(res);
    const signals = try datastar.readSignals(
        Signals,
        req,
    );

    inline for (message, 0..) |_, i| {
        const fragment = std.fmt.comptimePrint(
            "<div id='message'>{s}</div>",
            .{message[0 .. i + 1]},
        );
        try sse.mergeFragments(fragment, .{});

        std.Thread.sleep(std.time.ns_per_ms * signals.delay);
    }
}
