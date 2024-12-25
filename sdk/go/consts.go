// This is auto-generated by Datastar. DO NOT EDIT.

package datastar

import "time"

const (
    DatastarKey = "datastar"
    Version                   = "0.21.4"
    VersionClientByteSize     = 34238
    VersionClientByteSizeGzip = 12642

    //region Default durations

    // The default duration for settling during fragment merges. Allows for CSS transitions to complete.
    DefaultFragmentsSettleDuration = 300 * time.Millisecond
    // The default duration for retrying SSE on connection reset. This is part of the underlying retry mechanism of SSE.
    DefaultSseRetryDuration = 1000 * time.Millisecond

    //endregion Default durations

    //region Default strings

    // The default attributes for <script/> element use when executing scripts. It is a set of of key-value pairs delimited by a newline \\n character.
    DefaultExecuteScriptAttributes = "type module"

    //endregion Default strings

    //region Dataline literals
    SelectorDatalineLiteral = "selector "
    MergeModeDatalineLiteral = "mergeMode "
    SettleDurationDatalineLiteral = "settleDuration "
    FragmentsDatalineLiteral = "fragments "
    UseViewTransitionDatalineLiteral = "useViewTransition "
    SignalsDatalineLiteral = "signals "
    OnlyIfMissingDatalineLiteral = "onlyIfMissing "
    PathsDatalineLiteral = "paths "
    ScriptDatalineLiteral = "script "
    AttributesDatalineLiteral = "attributes "
    AutoRemoveDatalineLiteral = "autoRemove "
    //endregion Dataline literals
)

var (
    //region Default booleans

    // Should fragments be merged using the ViewTransition API?
    DefaultFragmentsUseViewTransitions = false

    // Should a given set of signals merge if they are missing?
    DefaultMergeSignalsOnlyIfMissing = false

    // Should script element remove itself after execution?
    DefaultExecuteScriptAutoRemove = true

    //endregion Default booleans
)

//region Enums

//region The mode in which a fragment is merged into the DOM.
type FragmentMergeMode string

const (
    // Default value for FragmentMergeMode
    // Morphs the fragment into the existing element using idiomorph.
    DefaultFragmentMergeMode = FragmentMergeModeMorph

    // Morphs the fragment into the existing element using idiomorph.
    FragmentMergeModeMorph FragmentMergeMode = "morph"

    // Replaces the inner HTML of the existing element.
    FragmentMergeModeInner FragmentMergeMode = "inner"

    // Replaces the outer HTML of the existing element.
    FragmentMergeModeOuter FragmentMergeMode = "outer"

    // Prepends the fragment to the existing element.
    FragmentMergeModePrepend FragmentMergeMode = "prepend"

    // Appends the fragment to the existing element.
    FragmentMergeModeAppend FragmentMergeMode = "append"

    // Inserts the fragment before the existing element.
    FragmentMergeModeBefore FragmentMergeMode = "before"

    // Inserts the fragment after the existing element.
    FragmentMergeModeAfter FragmentMergeMode = "after"

    // Upserts the attributes of the existing element.
    FragmentMergeModeUpsertAttributes FragmentMergeMode = "upsertAttributes"

)
//endregion FragmentMergeMode

//region The type protocol on top of SSE which allows for core pushed based communication between the server and the client.
type EventType string

const (
    // An event for merging HTML fragments into the DOM.
    EventTypeMergeFragments EventType = "datastar-merge-fragments"

    // An event for merging signals.
    EventTypeMergeSignals EventType = "datastar-merge-signals"

    // An event for removing HTML fragments from the DOM.
    EventTypeRemoveFragments EventType = "datastar-remove-fragments"

    // An event for removing signals.
    EventTypeRemoveSignals EventType = "datastar-remove-signals"

    // An event for executing <script/> elements in the browser.
    EventTypeExecuteScript EventType = "datastar-execute-script"

)
//endregion EventType

//endregion Enums