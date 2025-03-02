// const std = @import("std");
// const tk = @import("tokamak");
// const consts = @import("../consts.zig");
// const testing = @import("../testing.zig");

// pub const ServerSentEventGenerator = @import("ServerSentEventGenerator.zig");

// /// `readSignals` is a helper function that reads datastar signals from the request.
// pub fn readSignals(comptime T: type, req: *tk.Request) !T {
//     switch (req.method) {
//         .GET => {
//             const query = try req.query();
//             const signals = query.get(consts.datastar_key) orelse return error.MissingDatastarKey;

//             return std.json.parseFromSliceLeaky(T, req.arena, signals, .{});
//         },
//         else => {
//             const body = req.body() orelse return error.MissingBody;

//             return std.json.parseFromSliceLeaky(T, req.arena, body, .{});
//         },
//     }
// }

// fn sdk(req: *tk.Request, res: *tk.Response) !void {
//     var sse = try ServerSentEventGenerator.init(res);
//     const signals = try readSignals(
//         testing.Signals,
//         req,
//     );

//     try testing.sdk(&sse, signals);
// }

// const App = struct {
//     server: *tk.Server,
//     routes: []const tk.Route = &.{
//         .get("/test", sdk),
//         .post0("/test", sdk),
//     },
// };

// test sdk {
//     try tk.app.run(App);
// }
