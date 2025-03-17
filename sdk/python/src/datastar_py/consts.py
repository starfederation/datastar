# This is auto-generated by Datastar. DO NOT EDIT.
from enum import StrEnum

# region The mode in which a fragment is merged into the DOM.
class FragmentMergeMode(StrEnum):
    # Morphs the fragment into the existing element using idiomorph.
    MORPH = "morph"

    # Replaces the inner HTML of the existing element.
    INNER = "inner"

    # Replaces the outer HTML of the existing element.
    OUTER = "outer"

    # Prepends the fragment to the existing element.
    PREPEND = "prepend"

    # Appends the fragment to the existing element.
    APPEND = "append"

    # Inserts the fragment before the existing element.
    BEFORE = "before"

    # Inserts the fragment after the existing element.
    AFTER = "after"

    # Upserts the attributes of the existing element.
    UPSERT_ATTRIBUTES = "upsertAttributes"

# endregion FragmentMergeMode

# region The type protocol on top of SSE which allows for core pushed based communication between the server and the client.
class EventType(StrEnum):
    # An event for merging HTML fragments into the DOM.
    MERGE_FRAGMENTS = "datastar-merge-fragments"

    # An event for merging signals.
    MERGE_SIGNALS = "datastar-merge-signals"

    # An event for removing HTML fragments from the DOM.
    REMOVE_FRAGMENTS = "datastar-remove-fragments"

    # An event for removing signals.
    REMOVE_SIGNALS = "datastar-remove-signals"

    # An event for executing <script/> elements in the browser.
    EXECUTE_SCRIPT = "datastar-execute-script"

# endregion EventType

# endregion Enums

DATASTAR_KEY = "datastar"
VERSION = "1.0.0-beta.9"

# region Default durations

# The default duration for retrying SSE on connection reset. This is part of the underlying retry mechanism of SSE.
DEFAULT_SSE_RETRY_DURATION = 1000

# endregion Default durations

# region Default strings

# The default attributes for <script/> element use when executing scripts. It is a set of key-value pairs delimited by a newline \\n character.
DEFAULT_EXECUTE_SCRIPT_ATTRIBUTES = "type module"

# endregion Default strings

# region Dataline literals
SELECTOR_DATALINE_LITERAL = "selector"
MERGE_MODE_DATALINE_LITERAL = "mergeMode"
FRAGMENTS_DATALINE_LITERAL = "fragments"
USE_VIEW_TRANSITION_DATALINE_LITERAL = "useViewTransition"
SIGNALS_DATALINE_LITERAL = "signals"
ONLY_IF_MISSING_DATALINE_LITERAL = "onlyIfMissing"
PATHS_DATALINE_LITERAL = "paths"
SCRIPT_DATALINE_LITERAL = "script"
ATTRIBUTES_DATALINE_LITERAL = "attributes"
AUTO_REMOVE_DATALINE_LITERAL = "autoRemove"
# endregion Dataline literals

# region Default booleans

# Should fragments be merged using the ViewTransition API?
DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS = False

# Should a given set of signals merge if they are missing?
DEFAULT_MERGE_SIGNALS_ONLY_IF_MISSING = False

# Should script element remove itself after execution?
DEFAULT_EXECUTE_SCRIPT_AUTO_REMOVE = True

# endregion Default booleans

# region Enums