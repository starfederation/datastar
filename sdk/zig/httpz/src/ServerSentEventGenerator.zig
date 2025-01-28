const std = @import("std");
const httpz = @import("httpz");
const ServerSentEventGenerator = @import("datastar").ServerSentEventGenerator;

pub fn init(res: *httpz.Response) !ServerSentEventGenerator {
    res.content_type = .EVENTS;
    res.header("Cache-Control", "no-cache");
    res.header("Connection", "keep-alive");

    try res.write();

    const conn = res.conn;
    conn.handover = .close;

    return .{
        .allocator = res.arena,
        .writer = conn.stream.writer(),
    };
}
