const std = @import("std");
const consts = @import("consts.zig");
const ServerSentEventGenerator = @import("ServerSentEventGenerator.zig");

pub const Signals = struct {
    events: []const std.json.Value,
};

const ExecuteScript = struct {
    script: []const u8,
    eventId: ?[]const u8 = null,
    retryDuration: ?u32 = null,
    attributes: ?std.json.Value = null,
    autoRemove: ?bool = null,
};

const MergeFragments = struct {
    fragments: []const u8,
    eventId: ?[]const u8 = null,
    retryDuration: ?u32 = null,
    selector: ?[]const u8 = null,
    mergeMode: ?consts.FragmentMergeMode = null,
    settleDuration: ?u32 = null,
    useViewTransition: ?bool = null,
};

const MergeSignals = struct {
    signals: std.json.Value,
    eventId: ?[]const u8 = null,
    retryDuration: ?u32 = null,
    onlyIfMissing: ?bool = null,
};

const RemoveFragments = struct {
    selector: []const u8,
    eventId: ?[]const u8 = null,
    retryDuration: ?u32 = null,
    settleDuration: ?u32 = null,
    useViewTransition: ?bool = null,
};

const RemoveSignals = struct {
    paths: []const []const u8,
    eventId: ?[]const u8 = null,
    retryDuration: ?u32 = null,
};

pub fn sdk(sse: *ServerSentEventGenerator, signals: Signals) !void {
    for (signals.events) |event| {
        const event_type = event.object.get("type").?.string;

        if (std.mem.eql(u8, event_type, "executeScript")) {
            const ev = try std.json.parseFromValueLeaky(
                ExecuteScript,
                sse.allocator,
                event,
                .{ .ignore_unknown_fields = true },
            );

            const attrs = blk: {
                if (ev.attributes) |attrs| {
                    var result = std.ArrayList([]const u8).init(sse.allocator);

                    var iter = attrs.object.iterator();
                    while (iter.next()) |entry| {
                        var value = try std.json.stringifyAlloc(
                            sse.allocator,
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
                            sse.allocator,
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
                sse.allocator,
                event,
                .{ .ignore_unknown_fields = true },
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
                sse.allocator,
                event,
                .{ .ignore_unknown_fields = true },
            );

            const json = try std.json.stringifyAlloc(
                sse.allocator,
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
                sse.allocator,
                event,
                .{ .ignore_unknown_fields = true },
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
                sse.allocator,
                event,
                .{ .ignore_unknown_fields = true },
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
