package starfederation.datastar.events;

import starfederation.datastar.enums.EventType;

import java.util.ArrayList;
import java.util.List;

import static starfederation.datastar.Consts.*;

public final class RemoveFragments extends AbstractDatastarEvent {

    private RemoveFragments(EventType eventType, List<String> dataLines) {
        super(eventType, dataLines);
    }

    public static Builder builder() {
        return new Builder();
    }


    public static final class Builder extends AbstractBuilder<RemoveFragments> {
        private String selector;
        private int settleDuration = DEFAULT_FRAGMENTS_SETTLE_DURATION; // Default
        private boolean useViewTransition = DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS; // Default

        public Builder selector(String selector) {
            this.selector = selector;
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

        @Override
        public RemoveFragments build() {
            if (selector == null || selector.isBlank()) {
                throw new IllegalArgumentException("Selector cannot be null or empty");
            }

            List<String> dataLines = new ArrayList<>();

            // Add selector
            dataLines.add(SELECTOR_DATALINE_LITERAL + selector);

            // Add settleDuration if not default
            if (settleDuration != DEFAULT_FRAGMENTS_SETTLE_DURATION) {
                dataLines.add(SETTLE_DURATION_DATALINE_LITERAL + settleDuration);
            }

            // Add useViewTransition if true
            if (useViewTransition != DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS) {
                dataLines.add(USE_VIEW_TRANSITION_DATALINE_LITERAL + useViewTransition);
            }

            return new RemoveFragments(EventType.RemoveFragments, dataLines);
        }
    }
}
