# frozen_string_literal: true

# This is auto-generated by Datastar. DO NOT EDIT.
module Datastar
  module Consts
    DATASTAR_KEY = 'datastar'
    VERSION = '1.0.0-beta.5'

    # The default duration for settling during fragment merges. Allows for CSS transitions to complete.
    DEFAULT_FRAGMENTS_SETTLE_DURATION = 300

    # The default duration for retrying SSE on connection reset. This is part of the underlying retry mechanism of SSE.
    DEFAULT_SSE_RETRY_DURATION = 1000

    # Should fragments be merged using the ViewTransition API?
    DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS = false

    # Should a given set of signals merge if they are missing?
    DEFAULT_MERGE_SIGNALS_ONLY_IF_MISSING = false

    # Should script element remove itself after execution?
    DEFAULT_EXECUTE_SCRIPT_AUTO_REMOVE = true

    # The default attributes for <script/> element use when executing scripts. It is a set of key-value pairs delimited by a newline \\n character.}
    DEFAULT_EXECUTE_SCRIPT_ATTRIBUTES = 'type module'

    module FragmentMergeMode

      # Morphs the fragment into the existing element using idiomorph.
      MORPH = 'morph'

      # Replaces the inner HTML of the existing element.
      INNER = 'inner'

      # Replaces the outer HTML of the existing element.
      OUTER = 'outer'

      # Prepends the fragment to the existing element.
      PREPEND = 'prepend'

      # Appends the fragment to the existing element.
      APPEND = 'append'

      # Inserts the fragment before the existing element.
      BEFORE = 'before'

      # Inserts the fragment after the existing element.
      AFTER = 'after'

      # Upserts the attributes of the existing element.
      UPSERT_ATTRIBUTES = 'upsertAttributes'
    end

    # The mode in which a fragment is merged into the DOM.
    DEFAULT_FRAGMENT_MERGE_MODE = FragmentMergeMode::MORPH

    # Dataline literals.
    SELECTOR_DATALINE_LITERAL = 'selector'
    MERGE_MODE_DATALINE_LITERAL = 'mergeMode'
    SETTLE_DURATION_DATALINE_LITERAL = 'settleDuration'
    FRAGMENTS_DATALINE_LITERAL = 'fragments'
    USE_VIEW_TRANSITION_DATALINE_LITERAL = 'useViewTransition'
    SIGNALS_DATALINE_LITERAL = 'signals'
    ONLY_IF_MISSING_DATALINE_LITERAL = 'onlyIfMissing'
    PATHS_DATALINE_LITERAL = 'paths'
    SCRIPT_DATALINE_LITERAL = 'script'
    ATTRIBUTES_DATALINE_LITERAL = 'attributes'
    AUTO_REMOVE_DATALINE_LITERAL = 'autoRemove'
  end
end