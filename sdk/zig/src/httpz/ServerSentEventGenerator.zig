const std = @import("std");
const config = @import("config");
const httpz = @import("httpz");
const ServerSentEventGenerator = @import("../ServerSentEventGenerator.zig");

pub fn init(res: *httpz.Response, options: ServerSentEventGenerator.InitOptions) !ServerSentEventGenerator {
    res.content_type = .EVENTS;
    res.header("Cache-Control", "no-cache");

    var data: std.ArrayList(u8) = undefined;
    if (options.encoding) |encoding| {
        res.header("Content-Encoding", @tagName(encoding));

        data = std.ArrayList(u8).init(res.arena);
        errdefer data.deinit();
    }

    if (config.http1) {
        res.header("Connection", "keep-alive");
    }

    try res.write();

    const conn = res.conn;
    conn.handover = .close;

    return .{
        .allocator = res.arena,
        .writer = conn.stream.writer(),
        .encoding = options.encoding,
        .data = data,
    };
}
