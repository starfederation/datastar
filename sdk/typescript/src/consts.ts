// This is auto-generated by Datastar. DO NOT EDIT.

export const DATASTAR = "datastar" as const;
export const DATASTAR_REQUEST = "Datastar-Request";
export const VERSION = "1.0.0-beta.11";

// #region Defaults

// #region Default durations

// The default duration for retrying SSE on connection reset. This is part of the underlying retry mechanism of SSE.
export const DefaultSseRetryDurationMs = 1000;

// #endregion


// #region Default strings

// The default attributes for <script/> element use when executing scripts. It is a set of key-value pairs delimited by a newline \\n character.
export const DefaultExecuteScriptAttributes = "type module";

// #endregion


// #region Default booleans

// Should fragments be merged using the ViewTransition API?
export const DefaultFragmentsUseViewTransitions = false;

// Should a given set of signals merge if they are missing?
export const DefaultMergeSignalsOnlyIfMissing = false;

// Should script element remove itself after execution?
export const DefaultExecuteScriptAutoRemove = true;

// #endregion

// #region Datalines

export const DatastarDatalineSelector = "selector"
export const DatastarDatalineMergeMode = "mergeMode"
export const DatastarDatalineFragments = "fragments"
export const DatastarDatalineUseViewTransition = "useViewTransition"
export const DatastarDatalineSignals = "signals"
export const DatastarDatalineOnlyIfMissing = "onlyIfMissing"
export const DatastarDatalinePaths = "paths"
export const DatastarDatalineScript = "script"
export const DatastarDatalineAttributes = "attributes"
export const DatastarDatalineAutoRemove = "autoRemove"
// #endregion


// #region Enums

// The mode in which a fragment is merged into the DOM.
export const FragmentMergeModes = [
// Morphs the fragment into the existing element using idiomorph.
    "morph",
// Replaces the inner HTML of the existing element.
    "inner",
// Replaces the outer HTML of the existing element.
    "outer",
// Prepends the fragment to the existing element.
    "prepend",
// Appends the fragment to the existing element.
    "append",
// Inserts the fragment before the existing element.
    "before",
// Inserts the fragment after the existing element.
    "after",
// Upserts the attributes of the existing element.
    "upsertAttributes",
] as const;

// Default value for FragmentMergeMode
export const DefaultFragmentMergeMode = "morph";

// The type protocol on top of SSE which allows for core pushed based communication between the server and the client.
export const EventTypes = [
// An event for merging HTML fragments into the DOM.
    "datastar-merge-fragments",
// An event for merging signals.
    "datastar-merge-signals",
// An event for removing HTML fragments from the DOM.
    "datastar-remove-fragments",
// An event for removing signals.
    "datastar-remove-signals",
// An event for executing <script/> elements in the browser.
    "datastar-execute-script",
] as const;
// #endregion

// #endregion