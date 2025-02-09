const std = @import("std");
const consts = @import("consts.zig");

const default_execute_script_attributes: []const []const u8 = &[_][]const u8{consts.default_execute_script_attributes};

allocator: std.mem.Allocator,
writer: std.net.Stream.Writer,
mutex: std.Thread.Mutex = .{},

pub const ExecuteScriptOptions = struct {
    /// `event_id` can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    event_id: ?[]const u8 = null,
    /// `retry_duration` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
    retry_duration: u32 = consts.default_sse_retry_duration,
    /// A list of attributes to add to the script element.
    /// Each item in the array ***must*** be a string in the format `key value`.
    attributes: []const []const u8 = default_execute_script_attributes,
    /// Whether to remove the script after execution.
    auto_remove: bool = consts.default_execute_script_auto_remove,
};

pub const MergeFragmentsOptions = struct {
    /// `event_id` can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    event_id: ?[]const u8 = null,
    /// `retry_duration` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
    retry_duration: u32 = consts.default_sse_retry_duration,
    /// The CSS selector to use to insert the fragments.
    selector: ?[]const u8 = null,
    /// The mode to use when merging the fragment into the DOM.
    merge_mode: consts.FragmentMergeMode = consts.default_fragment_merge_mode,
    /// The amount of time that a fragment should take before removing any CSS related to settling.
    /// `settle_duration` is used to allow for animations in the browser via the Datastar client.
    settle_duration: u32 = consts.default_fragments_settle_duration,
    /// Whether to use view transitions.
    use_view_transition: bool = consts.default_fragments_use_view_transitions,
};

pub const MergeSignalsOptions = struct {
    /// `event_id` can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    event_id: ?[]const u8 = null,
    /// `retry_duration` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
    retry_duration: u32 = consts.default_sse_retry_duration,
    /// Whether to merge the signal only if it does not already exist.
    only_if_missing: bool = consts.default_merge_signals_only_if_missing,
};

pub const RemoveFragmentsOptions = struct {
    /// `event_id` can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    event_id: ?[]const u8 = null,
    /// `retry_duration` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
    retry_duration: u32 = consts.default_sse_retry_duration,
    /// The amount of time that a fragment should take before removing any CSS related to settling.
    /// `settle_duration` is used to allow for animations in the browser via the Datastar client.
    settle_duration: u32 = consts.default_fragments_settle_duration,
    /// Whether to use view transitions.
    use_view_transition: bool = consts.default_fragments_use_view_transitions,
};

pub const RemoveSignalsOptions = struct {
    /// `event_id` can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    event_id: ?[]const u8 = null,
    /// `retry_duration` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
    retry_duration: u32 = consts.default_sse_retry_duration,
};

fn send(
    self: *@This(),
    event: consts.EventType,
    data: []const []const u8,
    options: struct {
        event_id: ?[]const u8 = null,
        retry_duration: u32 = consts.default_sse_retry_duration,
    },
) !void {
    self.mutex.lock();
    defer self.mutex.unlock();

    try self.writer.print("event: {}\n", .{event});

    if (options.event_id) |id| {
        try self.writer.print("id: {s}\n", .{id});
    }

    if (options.retry_duration != consts.default_sse_retry_duration) {
        try self.writer.print("retry: {d}\n", .{options.retry_duration});
    }

    for (data) |line| {
        try self.writer.print("data: {s}\n", .{line});
    }

    try self.writer.writeAll("\n\n");
}

/// `ExecuteScript` executes JavaScript in the browser
///
/// See the [Datastar documentation](https://data-star.dev/reference/sse_events#datastar-execute-script) for more information.
pub fn executeScript(
    self: *@This(),
    /// `script` is a string that represents the JavaScript to be executed by the browser.
    script: []const u8,
    options: ExecuteScriptOptions,
) !void {
    var data = std.ArrayList([]const u8).init(self.allocator);

    if (options.attributes.len != 1 or !std.mem.eql(
        u8,
        default_execute_script_attributes[0],
        options.attributes[0],
    )) {
        for (options.attributes) |attribute| {
            const line = try std.fmt.allocPrint(
                self.allocator,
                "{s} {s}",
                .{
                    consts.attributes_dataline_literal,
                    attribute,
                },
            );

            try data.append(line);
        }
    }

    if (options.auto_remove != consts.default_execute_script_auto_remove) {
        const line = try std.fmt.allocPrint(
            self.allocator,
            "{s} {s}",
            .{
                consts.auto_remove_dataline_literal,
                if (options.auto_remove) "true" else "false",
            },
        );

        try data.append(line);
    }

    var iter = std.mem.splitScalar(u8, script, '\n');
    while (iter.next()) |elem| {
        const line = try std.fmt.allocPrint(
            self.allocator,
            "{s} {s}",
            .{
                consts.script_dataline_literal,
                elem,
            },
        );

        try data.append(line);
    }

    try self.send(
        .execute_script,
        try data.toOwnedSlice(),
        .{
            .event_id = options.event_id,
            .retry_duration = options.retry_duration,
        },
    );
}

/// `MergeFragments` merges one or more fragments into the DOM. By default,
/// Datastar merges fragments using Idiomorph, which matches top level elements based on their ID.
///
/// See the [Datastar documentation](https://data-star.dev/reference/sse_events#datastar-merge-fragments) for more information.
pub fn mergeFragments(
    self: *@This(),
    /// The HTML fragments to merge into the DOM.
    fragments: []const u8,
    options: MergeFragmentsOptions,
) !void {
    var data = std.ArrayList([]const u8).init(self.allocator);

    if (options.selector) |selector| {
        const line = try std.fmt.allocPrint(
            self.allocator,
            "{s} {s}",
            .{
                consts.selector_dataline_literal,
                selector,
            },
        );

        try data.append(line);
    }

    if (options.merge_mode != consts.default_fragment_merge_mode) {
        const line = try std.fmt.allocPrint(
            self.allocator,
            "{s} {}",
            .{
                consts.merge_mode_dataline_literal,
                options.merge_mode,
            },
        );

        try data.append(line);
    }

    if (options.settle_duration != consts.default_fragments_settle_duration) {
        const line = try std.fmt.allocPrint(
            self.allocator,
            "{s} {d}",
            .{
                consts.settle_duration_dataline_literal,
                options.settle_duration,
            },
        );

        try data.append(line);
    }

    if (options.use_view_transition != consts.default_fragments_use_view_transitions) {
        const line = try std.fmt.allocPrint(
            self.allocator,
            "{s} {}",
            .{
                consts.use_view_transition_dataline_literal,
                options.use_view_transition,
            },
        );

        try data.append(line);
    }

    var iter = std.mem.splitScalar(u8, fragments, '\n');
    while (iter.next()) |elem| {
        const line = try std.fmt.allocPrint(
            self.allocator,
            "{s} {s}",
            .{
                consts.fragments_dataline_literal,
                elem,
            },
        );

        try data.append(line);
    }

    try self.send(
        .merge_fragments,
        try data.toOwnedSlice(),
        .{
            .event_id = options.event_id,
            .retry_duration = options.retry_duration,
        },
    );
}

/// `MergeSignals` sends one or more signals to the browser to be merged into the signals.
///
/// See the [Datastar documentation](https://data-star.dev/reference/sse_events#datastar-merge-signals) for more information.
pub fn mergeSignals(
    self: *@This(),
    signals: []const u8,
    options: MergeSignalsOptions,
) !void {
    var data = std.ArrayList([]const u8).init(self.allocator);

    if (options.only_if_missing != consts.default_merge_signals_only_if_missing) {
        const line = try std.fmt.allocPrint(
            self.allocator,
            "{s} {}",
            .{
                consts.only_if_missing_dataline_literal,
                options.only_if_missing,
            },
        );

        try data.append(line);
    }

    const line = try std.fmt.allocPrint(
        self.allocator,
        "{s} {s}",
        .{
            consts.signals_dataline_literal,
            signals,
        },
    );

    try data.append(line);

    try self.send(
        .merge_signals,
        try data.toOwnedSlice(),
        .{
            .event_id = options.event_id,
            .retry_duration = options.retry_duration,
        },
    );
}

/// `RemoveFragments` sends a selector to the browser to remove HTML fragments from the DOM.
///
/// See the [Datastar documentation](https://data-star.dev/reference/sse_events#datastar-remove-fragments) for more information.
pub fn removeFragments(
    self: *@This(),
    selector: []const u8,
    options: RemoveFragmentsOptions,
) !void {
    var data = std.ArrayList([]const u8).init(self.allocator);

    if (options.settle_duration != consts.default_fragments_settle_duration) {
        const line = try std.fmt.allocPrint(
            self.allocator,
            "{s} {d}",
            .{
                consts.settle_duration_dataline_literal,
                options.settle_duration,
            },
        );

        try data.append(line);
    }

    if (options.use_view_transition != consts.default_fragments_use_view_transitions) {
        const line = try std.fmt.allocPrint(
            self.allocator,
            "{s} {}",
            .{
                consts.use_view_transition_dataline_literal,
                options.use_view_transition,
            },
        );

        try data.append(line);
    }

    const line = try std.fmt.allocPrint(
        self.allocator,
        "{s} {s}",
        .{
            consts.selector_dataline_literal,
            selector,
        },
    );

    try data.append(line);

    try self.send(
        .remove_fragments,
        try data.toOwnedSlice(),
        .{
            .event_id = options.event_id,
            .retry_duration = options.retry_duration,
        },
    );
}

/// `RemoveSignals` sends signals to the browser to be removed from the signals.
///
/// See the [Datastar documentation](https://data-star.dev/reference/sse_events#datastar-remove-signals) for more information.
pub fn removeSignals(
    self: *@This(),
    paths: []const []const u8,
    options: RemoveSignalsOptions,
) !void {
    var data = std.ArrayList([]const u8).init(self.allocator);

    for (paths) |path| {
        const line = try std.fmt.allocPrint(
            self.allocator,
            "{s} {s}",
            .{
                consts.paths_dataline_literal,
                path,
            },
        );

        try data.append(line);
    }

    try self.send(
        .remove_signals,
        try data.toOwnedSlice(),
        .{
            .event_id = options.event_id,
            .retry_duration = options.retry_duration,
        },
    );
}
