package starfederation.datastar.events;

import starfederation.datastar.enums.EventType;

import java.util.ArrayList;
import java.util.List;

import static starfederation.datastar.Consts.*;

public final class PatchSignals extends AbstractDatastarEvent {

    private PatchSignals(EventType eventType, List<String> dataLines) {
        super(eventType, dataLines);
    }

    @Override
    public EventType getEventType() {
        return EventType.PatchSignals;
    }

    public static Builder builder() {
        return new Builder();
    }

    public static final class Builder extends AbstractBuilder<PatchSignals> {
        private String data;
        private boolean onlyIfMissing = DEFAULT_PATCH_SIGNALS_ONLY_IF_MISSING; // Default

        public Builder data(String data) {
            this.data = data;
            return this;
        }

        public Builder onlyIfMissing(boolean onlyIfMissing) {
            this.onlyIfMissing = onlyIfMissing;
            return this;
        }

        @Override
        public PatchSignals build() {
            if (data == null || data.isBlank()) {
                throw new IllegalArgumentException("Data cannot be null or empty");
            }

            List<String> dataLines = new ArrayList<>();

            // Add onlyIfMissing if true
            if (onlyIfMissing != DEFAULT_PATCH_SIGNALS_ONLY_IF_MISSING) {
                dataLines.add(ONLY_IF_MISSING_DATALINE_LITERAL + onlyIfMissing);
            }

            // Add signals data
            dataLines.add(SIGNALS_DATALINE_LITERAL + data);

            return new PatchSignals(EventType.PatchSignals, dataLines);
        }
    }
}
