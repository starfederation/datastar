// This is auto-generated by Datastar. DO NOT EDIT.

const std = @import("std");

pub const datastar_key = "datastar";
pub const version = "1.0.0-beta.10";

// #region Defaults

// #region Default durations

/// The default duration for retrying SSE on connection reset. This is part of the underlying retry mechanism of SSE.
pub const default_sse_retry_duration = 1000;

// #endregion

// #region Default strings

/// The default attributes for <script/> element use when executing scripts. It is a set of key-value pairs delimited by a newline \\n character.
pub const default_execute_script_attributes = "type module";

// #endregion

// #region Datalines

pub const selector_dataline_literal = "selector";
pub const merge_mode_dataline_literal = "mergeMode";
pub const fragments_dataline_literal = "fragments";
pub const use_view_transition_dataline_literal = "useViewTransition";
pub const signals_dataline_literal = "signals";
pub const only_if_missing_dataline_literal = "onlyIfMissing";
pub const paths_dataline_literal = "paths";
pub const script_dataline_literal = "script";
pub const attributes_dataline_literal = "attributes";
pub const auto_remove_dataline_literal = "autoRemove";

// #endregion

// #region Default booleans

/// Should fragments be merged using the ViewTransition API?
pub const default_fragments_use_view_transitions = false;
/// Should a given set of signals merge if they are missing?
pub const default_merge_signals_only_if_missing = false;
/// Should script element remove itself after execution?
pub const default_execute_script_auto_remove = true;

// #endregion

// #region Enums

/// The mode in which a fragment is merged into the DOM.
pub const FragmentMergeMode = enum {
    /// Morphs the fragment into the existing element using idiomorph.
    morph,
    /// Replaces the inner HTML of the existing element.
    inner,
    /// Replaces the outer HTML of the existing element.
    outer,
    /// Prepends the fragment to the existing element.
    prepend,
    /// Appends the fragment to the existing element.
    append,
    /// Inserts the fragment before the existing element.
    before,
    /// Inserts the fragment after the existing element.
    after,
    /// Upserts the attributes of the existing element.
    upsert_attributes,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.writeAll(
            switch (self) {
                .morph => "morph",
                .inner => "inner",
                .outer => "outer",
                .prepend => "prepend",
                .append => "append",
                .before => "before",
                .after => "after",
                .upsert_attributes => "upsertAttributes",
            },
        );
    }
};

pub const default_fragment_merge_mode = FragmentMergeMode.morph;

/// The type protocol on top of SSE which allows for core pushed based communication between the server and the client.
pub const EventType = enum {
    /// An event for merging HTML fragments into the DOM.
    merge_fragments,
    /// An event for merging signals.
    merge_signals,
    /// An event for removing HTML fragments from the DOM.
    remove_fragments,
    /// An event for removing signals.
    remove_signals,
    /// An event for executing <script/> elements in the browser.
    execute_script,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.writeAll(
            switch (self) {
                .merge_fragments => "datastar-merge-fragments",
                .merge_signals => "datastar-merge-signals",
                .remove_fragments => "datastar-remove-fragments",
                .remove_signals => "datastar-remove-signals",
                .execute_script => "datastar-execute-script",
            },
        );
    }
};


// #endregion

// #endregion