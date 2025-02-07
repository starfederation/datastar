const std = @import("std");
const config = @import("config");
const tk = @import("tokamak");
const ServerSentEventGenerator = @import("../ServerSentEventGenerator.zig");

pub fn init(res: *tk.Response) !ServerSentEventGenerator {
    res.content_type = .EVENTS;
    res.header("Cache-Control", "no-cache");

    if (config.http1) {
        res.header("Connection", "keep-alive");
    }

    try res.write();

    const conn = res.conn;
    conn.handover = .close;

    return .{
        .allocator = res.arena,
        .writer = conn.stream.writer(),
    };
}
