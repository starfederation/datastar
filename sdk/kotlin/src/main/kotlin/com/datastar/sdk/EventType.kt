package com.datastar.sdk

object MergeMode {
    const val MORPH = "morph"             // Use idiomorph to merge the fragment into the DOM
    const val INNER = "inner"             // Replace the innerHTML of the selector with the fragment
    const val OUTER = "outer"             // Replace the outerHTML of the selector with the fragment
    const val PREPEND = "prepend"         // Prepend the fragment to the selector
    const val APPEND = "append"           // Append the fragment to the selector
    const val BEFORE = "before"           // Insert the fragment before the selector
    const val AFTER = "after"             // Insert the fragment after the selector
    const val UPSERT_ATTRIBUTES = "upsertAttributes" // Update the attributes of the selector with the fragment
}
data class MergeFragmentsOptions(

    val fragments: String,
    val selector: String? = null,
    val mergeMode: String? = null,
    val settleDuration: Long? = null,
    val useViewTransition: Boolean? = null,
    val id : String? = null
)

data class DataStarEvent(
    val eventType: String,
    val dataLines: List<String>,
    val id : String? = null
)


data class RemoveFragmentsOptions(
    val selector: String,
    val settleDuration: Long? = null,
    val useViewTransition: Boolean? = null,
    val id : String? = null
)


data class MergeSignalsOptions(
    val signals: String,
    val onlyIfMissing: Boolean = false,
    val id : String? = null
)

data class RemoveSignalsOptions(
    val paths: List<String>,
    val id : String? = null
)

data class ExecuteScriptOptions(
    val script: String,
    val attributes: String? = null,
    val autoRemove: Boolean? = null,
    val id : String? = null
)

fun ExecuteScriptOptions.toDataStarEvent(): DataStarEvent {
    val dataLines = mutableListOf<String>()

    // Add autoRemove if false (default is true)
    if (autoRemove != Consts.DEFAULT_EXECUTE_SCRIPT_AUTO_REMOVE) {
        dataLines.add(Consts.AUTO_REMOVE_DATALINE_LITERAL + autoRemove)
    }

    // Add attributes if not default
    if (attributes != null && attributes != Consts.DEFAULT_EXECUTE_SCRIPT_ATTRIBUTES) {
        dataLines.add(Consts.ATTRIBUTES_DATALINE_LITERAL + attributes)
    }

    // Add script
    dataLines.add(Consts.SCRIPT_DATALINE_LITERAL + script)

    return DataStarEvent(eventType = "datastar-execute-script", dataLines = dataLines, id = id)
}

fun MergeSignalsOptions.toDataStarEvent(): DataStarEvent {
    val dataLines = mutableListOf<String>()
    
    // Only add onlyIfMissing if different from default
    if (onlyIfMissing != Consts.DEFAULT_MERGE_SIGNALS_ONLY_IF_MISSING) {
        dataLines.add(Consts.ONLY_IF_MISSING_DATALINE_LITERAL + onlyIfMissing)
    }
    
    // Add signals data
    dataLines.add(Consts.SIGNALS_DATALINE_LITERAL + signals.trim())
    
    return DataStarEvent(eventType = "datastar-merge-signals", dataLines = dataLines, id = id)
}


fun RemoveFragmentsOptions.toDataStarEvent(): DataStarEvent {
    val dataLines = mutableListOf<String>()
    dataLines.add(Consts.SELECTOR_DATALINE_LITERAL + selector)
    dataLines.add(Consts.SETTLE_DURATION_DATALINE_LITERAL + settleDuration)
    dataLines.add(Consts.USE_VIEW_TRANSITION_DATALINE_LITERAL + useViewTransition)
    return DataStarEvent(eventType = "datastar-remove-fragments", dataLines = dataLines, id = id)
}

fun RemoveSignalsOptions.toDataStarEvent(): DataStarEvent {
    val dataLines = mutableListOf<String>()
    paths.forEach { path -> 
        dataLines.add(Consts.PATHS_DATALINE_LITERAL + path)
    }
    return DataStarEvent(eventType = "datastar-remove-signals", dataLines = dataLines, id = id)
}


fun MergeFragmentsOptions.toDataStarEvent(): DataStarEvent {
    val dataLines = mutableListOf<String>()
    dataLines.add(Consts.FRAGMENTS_DATALINE_LITERAL + fragments)
    if (selector != null) {
        dataLines.add(Consts.SELECTOR_DATALINE_LITERAL + selector)
    }
    if (mergeMode != null) {
        dataLines.add(Consts.MERGE_MODE_DATALINE_LITERAL + mergeMode)
    }
    if (settleDuration != null) {
        dataLines.add(Consts.SETTLE_DURATION_DATALINE_LITERAL + settleDuration)
    }
    if (useViewTransition != null) {
        dataLines.add(Consts.USE_VIEW_TRANSITION_DATALINE_LITERAL + useViewTransition)
    }

    fragments.split("\n")
        .map { it.trim() }
        .filter { it.isNotEmpty() }
        .forEach { dataLines.add(Consts.FRAGMENTS_DATALINE_LITERAL + it) }
    return DataStarEvent(eventType = "datastar-merge-fragments", dataLines = dataLines, id = id)
}

object Consts {
    const val DATASTAR_KEY = "datastar"
    const val VERSION = "1.0.0-beta.1"

    // The default duration for settling during fragment merges. Allows for CSS transitions to complete.
    const val DEFAULT_FRAGMENTS_SETTLE_DURATION = 300

    // The default duration for retrying SSE on connection reset. This is part of the underlying retry mechanism of SSE.
    const val DEFAULT_SSE_RETRY_DURATION = 1000

    // Should fragments be merged using the ViewTransition API?
    const val DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS = false

    // Should a given set of signals merge if they are missing?
    const val DEFAULT_MERGE_SIGNALS_ONLY_IF_MISSING = false

    // Should script element remove itself after execution?
    const val DEFAULT_EXECUTE_SCRIPT_AUTO_REMOVE = true

    // The default attributes for <script/> element use when executing scripts. It is a set of of key-value pairs delimited by a newline \n character.
    const val DEFAULT_EXECUTE_SCRIPT_ATTRIBUTES = "type module"

    // The mode in which a fragment is merged into the DOM.
    val DEFAULT_FRAGMENT_MERGE_MODE = MergeMode.MORPH

    // Dataline literals.
    const val SELECTOR_DATALINE_LITERAL = "selector "
    const val MERGE_MODE_DATALINE_LITERAL = "mergeMode "
    const val SETTLE_DURATION_DATALINE_LITERAL = "settleDuration "
    const val FRAGMENTS_DATALINE_LITERAL = "fragments "
    const val USE_VIEW_TRANSITION_DATALINE_LITERAL = "useViewTransition "
    const val SIGNALS_DATALINE_LITERAL = "signals "
    const val ONLY_IF_MISSING_DATALINE_LITERAL = "onlyIfMissing "
    const val PATHS_DATALINE_LITERAL = "paths "
    const val SCRIPT_DATALINE_LITERAL = "script "
    const val ATTRIBUTES_DATALINE_LITERAL = "attributes "
    const val AUTO_REMOVE_DATALINE_LITERAL = "autoRemove "
}

