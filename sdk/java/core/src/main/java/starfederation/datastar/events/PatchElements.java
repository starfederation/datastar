package starfederation.datastar.events;

import starfederation.datastar.enums.ElementPatchMode;
import starfederation.datastar.enums.EventType;

import java.util.ArrayList;
import java.util.List;

import static starfederation.datastar.Consts.*;

public final class PatchElements extends AbstractDatastarEvent {

    private PatchElements(EventType eventType, List<String> dataLines) {
        super(eventType, dataLines);
    }

    @Override
    public EventType getEventType() {
        return EventType.PatchElements;
    }

    public static Builder builder() {
        return new Builder();
    }

    public static final class Builder extends AbstractBuilder<PatchElements> {
        private String selector;
        private ElementPatchMode mode = DEFAULT_ELEMENT_PATCH_MODE; // Default
        private boolean useViewTransition = DEFAULT_ELEMENTS_USE_VIEW_TRANSITIONS; // Default
        private String rawData;

        private Builder() {
        }

        public Builder selector(String selector) {
            this.selector = selector;
            return this;
        }

        public Builder mode(ElementPatchMode mode) {
            this.mode = mode;
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
        public PatchElements build() {
            if (mode != ElementPatchMode.Remove && (rawData == null || rawData.isBlank())) {
                throw new IllegalArgumentException("Data cannot be null or empty");
            }

            List<String> dataLines = new ArrayList<>();

            // Add selector
            if (selector != null && !selector.isEmpty()) {
                dataLines.add(SELECTOR_DATALINE_LITERAL + selector.trim());
            }

            // Add mergeMode if not default
            if (mode != DEFAULT_ELEMENT_PATCH_MODE) {
                dataLines.add(MODE_DATALINE_LITERAL + mode);
            }

            // Add useViewTransition if true
            if (useViewTransition != DEFAULT_ELEMENTS_USE_VIEW_TRANSITIONS) {
                dataLines.add(USE_VIEW_TRANSITION_DATALINE_LITERAL + useViewTransition);
            }

            // Add raw data as fragments
            if (rawData!= null)
                rawData.lines()
                        .filter(line -> !line.isBlank())
                        .map(String::trim)
                        .forEach(line -> dataLines.add(ELEMENTS_DATALINE_LITERAL + line));

            return new PatchElements(EventType.PatchElements, dataLines);
        }
    }
}
