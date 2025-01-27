const std = @import("std");
const root = @import("root.zig");
const consts = @import("consts.zig");
const httpz = @import("httpz");
const Logger = @import("Logger.zig");

const App = struct {
    pub fn uncaughtError(_: *App, req: *httpz.Request, res: *httpz.Response, err: anyerror) void {
        std.log.err("500 {} {s} {}", .{ req.method, req.url.path, err });
        res.status = 500;
        res.body = "sorry";
    }
};

const Signals = struct {
    events: []const std.json.Value,
};

const ExecuteScript = struct {
    type: []const u8,
    script: []const u8,
    eventId: ?[]const u8 = null,
    retryDuration: ?u32 = null,
    attributes: ?std.json.Value = null,
    autoRemove: ?bool = null,
};

const MergeFragments = struct {
    type: []const u8,
    fragments: []const u8,
    eventId: ?[]const u8 = null,
    retryDuration: ?u32 = null,
    selector: ?[]const u8 = null,
    mergeMode: ?consts.FragmentMergeMode = null,
    settleDuration: ?u32 = null,
    useViewTransition: ?bool = null,
};

const MergeSignals = struct {
    type: []const u8,
    signals: std.json.Value,
    eventId: ?[]const u8 = null,
    retryDuration: ?u32 = null,
    onlyIfMissing: ?bool = null,
};

const RemoveFragments = struct {
    type: []const u8,
    selector: []const u8,
    eventId: ?[]const u8 = null,
    retryDuration: ?u32 = null,
    settleDuration: ?u32 = null,
    useViewTransition: ?bool = null,
};

const RemoveSignals = struct {
    type: []const u8,
    paths: []const []const u8,
    eventId: ?[]const u8 = null,
    retryDuration: ?u32 = null,
};

fn sdkTest(_: *App, req: *httpz.Request, res: *httpz.Response) !void {
    const datastar = try root.readSignals(
        Signals,
        req,
    );

    var sse = try root.ServerSentEventGenerator.init(res);

    for (datastar.events) |event| {
        const event_type = event.object.get("type").?.string;

        if (std.mem.eql(u8, event_type, "executeScript")) {
            const ev = try std.json.parseFromValueLeaky(
                ExecuteScript,
                res.arena,
                event,
                .{},
            );

            const attrs = blk: {
                if (ev.attributes) |attrs| {
                    var result = std.ArrayList([]const u8).init(res.arena);

                    var iter = attrs.object.iterator();
                    while (iter.next()) |entry| {
                        var value = try std.json.stringifyAlloc(
                            res.arena,
                            entry.value_ptr.*,
                            .{},
                        );

                        switch (entry.value_ptr.*) {
                            .string => {
                                value = value[1 .. value.len - 1];
                            },
                            else => {},
                        }

                        const string = try std.fmt.allocPrint(
                            res.arena,
                            "{s} {s}",
                            .{
                                entry.key_ptr.*,
                                value,
                            },
                        );

                        try result.append(string);
                    }

                    break :blk try result.toOwnedSlice();
                } else {
                    break :blk &[_][]const u8{consts.default_execute_script_attributes};
                }
            };

            try sse.executeScript(
                ev.script,
                .{
                    .event_id = ev.eventId,
                    .retry_duration = ev.retryDuration orelse consts.default_sse_retry_duration,
                    .attributes = attrs,
                    .auto_remove = ev.autoRemove orelse true,
                },
            );
        } else if (std.mem.eql(u8, event_type, "mergeFragments")) {
            const ev = try std.json.parseFromValueLeaky(
                MergeFragments,
                res.arena,
                event,
                .{},
            );

            try sse.mergeFragments(
                ev.fragments,
                .{
                    .event_id = ev.eventId,
                    .retry_duration = ev.retryDuration orelse consts.default_sse_retry_duration,
                    .selector = ev.selector,
                    .merge_mode = ev.mergeMode orelse consts.default_fragment_merge_mode,
                    .settle_duration = ev.settleDuration orelse consts.default_fragments_settle_duration,
                    .use_view_transition = ev.useViewTransition orelse consts.default_fragments_use_view_transitions,
                },
            );
        } else if (std.mem.eql(u8, event_type, "mergeSignals")) {
            const ev = try std.json.parseFromValueLeaky(
                MergeSignals,
                res.arena,
                event,
                .{},
            );

            const json = try std.json.stringifyAlloc(
                res.arena,
                ev.signals,
                .{},
            );

            try sse.mergeSignals(
                json,
                .{
                    .event_id = ev.eventId,
                    .retry_duration = ev.retryDuration orelse consts.default_sse_retry_duration,
                    .only_if_missing = ev.onlyIfMissing orelse consts.default_merge_signals_only_if_missing,
                },
            );
        } else if (std.mem.eql(u8, event_type, "removeFragments")) {
            const ev = try std.json.parseFromValueLeaky(
                RemoveFragments,
                res.arena,
                event,
                .{},
            );

            try sse.removeFragments(
                ev.selector,
                .{
                    .event_id = ev.eventId,
                    .retry_duration = ev.retryDuration orelse consts.default_sse_retry_duration,
                    .settle_duration = ev.settleDuration orelse consts.default_fragments_settle_duration,
                    .use_view_transition = ev.useViewTransition orelse consts.default_fragments_use_view_transitions,
                },
            );
        } else if (std.mem.eql(u8, event_type, "removeSignals")) {
            const ev = try std.json.parseFromValueLeaky(
                RemoveSignals,
                res.arena,
                event,
                .{},
            );

            try sse.removeSignals(
                ev.paths,
                .{
                    .event_id = ev.eventId,
                    .retry_duration = ev.retryDuration orelse consts.default_sse_retry_duration,
                },
            );
        }
    }
}

test "sdk" {
    var app = App{};
    var server = try httpz.Server(*App).init(
        std.testing.allocator,
        .{ .port = 5882 },
        &app,
    );
    defer {
        server.stop();
        server.deinit();
    }

    const logger = try server.middleware(Logger, .{});

    var router = server.router(.{});

    router.middlewares = &.{logger};

    router.get("/test", sdkTest, .{});
    router.post("/test", sdkTest, .{});

    try server.listen();
}
