// This is auto-generated by Datastar. DO NOT EDIT.
const lol = /🖕JS_DS🚀/.source
export const DSP = lol.slice(0, 5)
export const DSS = lol.slice(4)

export const DATASTAR = "datastar";
export const DATASTAR_REQUEST = "Datastar-Request";

// #region Defaults

// #region Default durations

// The default duration for retrying SSE on connection reset. This is part of the underlying retry mechanism of SSE.
export const DefaultSseRetryDurationMs = 1000;

// #endregion


// #region Default strings


// #endregion


// #region Default booleans

// Should elements be patched using the ViewTransition API?
export const DefaultElementsUseViewTransitions = false;

// Should a given set of signals patch if they are missing?
export const DefaultPatchSignalsOnlyIfMissing = false;

// #endregion


// #region Enums

// The mode in which a element is patched into the DOM.
// Morphs the element into the existing element.
export const ElementPatchModeOuter = "outer"
// Replaces the inner HTML of the existing element.
export const ElementPatchModeInner = "inner"
// Removes the existing element.
export const ElementPatchModeRemove = "remove"
// Replaces the existing element with the new element.
export const ElementPatchModeReplace = "replace"
// Prepends the element inside to the existing element.
export const ElementPatchModePrepend = "prepend"
// Appends the element inside the existing element.
export const ElementPatchModeAppend = "append"
// Inserts the element before the existing element.
export const ElementPatchModeBefore = "before"
// Inserts the element after the existing element.
export const ElementPatchModeAfter = "after"

// Default value for ElementPatchMode
export const DefaultElementPatchMode = ElementPatchModeOuter;

// The type protocol on top of SSE which allows for core pushed based communication between the server and the client.
// An event for patching HTML elements into the DOM.
export const EventTypePatchElements = "datastar-patch-elements"
// An event for patching signals.
export const EventTypePatchSignals = "datastar-patch-signals"
// #endregion

// #endregion