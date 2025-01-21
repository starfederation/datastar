package starfederation.datastar.events;

import starfederation.datastar.enums.EventType;
import starfederation.datastar.enums.FragmentMergeMode;

import java.util.ArrayList;
import java.util.List;

import static starfederation.datastar.Consts.*;

public final class MergeFragments extends AbstractDatastarEvent {

    private MergeFragments(EventType eventType, List<String> dataLines) {
        super(eventType, dataLines);
    }


    @Override
    public EventType getEventType() {
        return EventType.MergeFragments;
    }

    public static Builder builder() {
        return new Builder();
    }


    public static final class Builder extends AbstractBuilder<MergeFragments> {
        private String selector;
        private FragmentMergeMode mergeMode = DEFAULT_FRAGMENT_MERGE_MODE; // Default
        private int settleDuration = DEFAULT_FRAGMENTS_SETTLE_DURATION; // Default
        private boolean useViewTransition = DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS; // Default
        private String rawData;

        private Builder() {
        }

        public Builder selector(String selector) {
            this.selector = selector;
            return this;
        }

        public Builder mergeMode(FragmentMergeMode mergeMode) {
            this.mergeMode = mergeMode;
            return this;
        }

        public Builder settleDuration(int settleDuration) {
            this.settleDuration = settleDuration;
            return this;
        }

        public Builder useViewTransition(boolean useViewTransition) {
            this.useViewTransition = useViewTransition;
            return this;
        }

        public Builder data(String rawData) {
            this.rawData = rawData;
            return this;
        }

        @Override
        public MergeFragments build() {
            if (rawData == null || rawData.isBlank()) {
                throw new IllegalArgumentException("Data cannot be null or empty");
            }

            List<String> dataLines = new ArrayList<>();

            // Add selector
            if (selector != null && !selector.isEmpty()) {
                dataLines.add(SELECTOR_DATALINE_LITERAL + selector.trim());
            }

            // Add mergeMode if not default
            if (mergeMode != DEFAULT_FRAGMENT_MERGE_MODE) {
                dataLines.add(MERGE_MODE_DATALINE_LITERAL + mergeMode);
            }

            // Add settleDuration if not default
            if (settleDuration != DEFAULT_FRAGMENTS_SETTLE_DURATION) {
                dataLines.add(SETTLE_DURATION_DATALINE_LITERAL + settleDuration);
            }

            // Add useViewTransition if true
            if (useViewTransition != DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS) {
                dataLines.add(USE_VIEW_TRANSITION_DATALINE_LITERAL + useViewTransition);
            }

            // Add raw data as fragments
            rawData.lines()
                    .filter(line -> !line.isBlank())
                    .map(String::trim)
                    .forEach(line -> dataLines.add(FRAGMENTS_DATALINE_LITERAL + line));

            return new MergeFragments(EventType.MergeFragments, dataLines);
        }
    }
}
