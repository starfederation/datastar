package starfederation.datastar.events;

import starfederation.datastar.enums.EventType;

import java.util.ArrayList;
import java.util.List;

import static starfederation.datastar.Consts.PATHS_DATALINE_LITERAL;

public final class RemoveSignals extends AbstractDatastarEvent {

    private RemoveSignals(EventType eventType, List<String> dataLines) {
        super(eventType, dataLines);
    }

    public static Builder builder() {
        return new Builder();
    }

    public static final class Builder extends AbstractBuilder<RemoveSignals> {
        private final List<String> paths = new ArrayList<>();

        public Builder addPath(String path) {
            if (path == null || path.isBlank()) {
                throw new IllegalArgumentException("Path cannot be null or empty");
            }
            paths.add(path.trim());
            return this;
        }

        @Override
        public RemoveSignals build() {
            if (paths.isEmpty()) {
                throw new IllegalArgumentException("At least one path must be provided");
            }

            List<String> dataLines = new ArrayList<>();

            // Add paths
            paths.forEach(path -> dataLines.add(PATHS_DATALINE_LITERAL + path));

            return new RemoveSignals(EventType.RemoveSignals, dataLines);
        }
    }
}
