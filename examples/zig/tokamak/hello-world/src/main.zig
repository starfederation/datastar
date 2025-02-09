const std = @import("std");
const tk = @import("tokamak");
const datastar = @import("datastar");

const App = struct {
    server: *tk.Server,
    routes: []const tk.Route = &.{
        .get("/", tk.static.file("hello-world.html")),
        .group("/", &.{.router(routes)}),
    },
};

pub fn main() !void {
    try tk.app.run(App);
}

const Signals = struct {
    delay: u64,
};

const message = "Hello, world!";

const routes = struct {
    pub fn @"GET /hello-world"(req: *tk.Request, res: *tk.Response) !void {
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
};
