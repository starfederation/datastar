package starfederation.datastar;

import starfederation.datastar.enums.FragmentMergeMode;

/**
 * This is auto-generated by Datastar. DO NOT EDIT.
 */
public final class Consts {
    public static final String DATASTAR_KEY = "datastar";
    public static final String VERSION = "1.0.0-beta.7";

    // The default duration for settling during fragment merges. Allows for CSS transitions to complete.
    public static final int DEFAULT_FRAGMENTS_SETTLE_DURATION = 300;

    // The default duration for retrying SSE on connection reset. This is part of the underlying retry mechanism of SSE.
    public static final int DEFAULT_SSE_RETRY_DURATION = 1000;

    // Should fragments be merged using the ViewTransition API?
    public static final boolean DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS = false;

    // Should a given set of signals merge if they are missing?
    public static final boolean DEFAULT_MERGE_SIGNALS_ONLY_IF_MISSING = false;

    // Should script element remove itself after execution?
    public static final boolean DEFAULT_EXECUTE_SCRIPT_AUTO_REMOVE = true;

    // The default attributes for <script/> element use when executing scripts. It is a set of key-value pairs delimited by a newline \\n character.
    public static final String DEFAULT_EXECUTE_SCRIPT_ATTRIBUTES = "type module";

    // The mode in which a fragment is merged into the DOM.
    public static final FragmentMergeMode DEFAULT_FRAGMENT_MERGE_MODE = FragmentMergeMode.Morph;

    // Dataline literals.
    public static final String SELECTOR_DATALINE_LITERAL = "selector ";
    public static final String MERGE_MODE_DATALINE_LITERAL = "mergeMode ";
    public static final String SETTLE_DURATION_DATALINE_LITERAL = "settleDuration ";
    public static final String FRAGMENTS_DATALINE_LITERAL = "fragments ";
    public static final String USE_VIEW_TRANSITION_DATALINE_LITERAL = "useViewTransition ";
    public static final String SIGNALS_DATALINE_LITERAL = "signals ";
    public static final String ONLY_IF_MISSING_DATALINE_LITERAL = "onlyIfMissing ";
    public static final String PATHS_DATALINE_LITERAL = "paths ";
    public static final String SCRIPT_DATALINE_LITERAL = "script ";
    public static final String ATTRIBUTES_DATALINE_LITERAL = "attributes ";
    public static final String AUTO_REMOVE_DATALINE_LITERAL = "autoRemove ";
}