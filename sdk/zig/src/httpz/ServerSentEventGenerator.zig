const std = @import("std");
const config = @import("config");
const httpz = @import("httpz");
const ServerSentEventGenerator = @import("../ServerSentEventGenerator.zig");

pub fn init(res: *httpz.Response, options: ServerSentEventGenerator.InitOptions) !ServerSentEventGenerator {
    res.content_type = .EVENTS;
    res.header("Cache-Control", "no-cache");

    if (config.http1) {
        res.header("Connection", "keep-alive");
    }

    if (options.encoding) |_| {} else try res.write();

    const conn = res.conn;
    conn.handover = .close;

    return .{
        .allocator = res.arena,
        .writer = conn.stream.writer(),
        .res = res,
        // .encoding = options.encoding,
        .options = options,
        .data = if (options.encoding) |_| std.ArrayList(u8).init(res.arena) else undefined,
    };
}
