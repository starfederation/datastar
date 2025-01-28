const std = @import("std");
const tk = @import("tokamak");
const testing = @import("datastar").testing;
const root = @import("root");

fn sdk(req: *tk.Request, res: *tk.Response) !void {
    var sse = try root.ServerSentEventGenerator.init(res);
    const datastar = try root.readSignals(
        testing.Signals,
        req,
    );

    try testing.sdk(&sse, datastar);
}

const App = struct {
    server: *tk.Server,
    routes: []const tk.Route = &.{
        .get("/test", sdk),
        .post("/test", sdk),
    },
};

test sdk {
    try tk.app.run(App);
}
