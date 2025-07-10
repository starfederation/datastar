package starfederation.datastar.events;

import starfederation.datastar.enums.EventType;

import java.util.List;
import java.util.Objects;

sealed public abstract class AbstractDatastarEvent implements DatastarEvent permits PatchElements, PatchSignals,
        ExecuteScript, CustomEvent {

    private final String[] dataLines;

    protected AbstractDatastarEvent(EventType eventType, List<String> dataLines) {
        Objects.requireNonNull(eventType, "Event type cannot be null");
        Objects.requireNonNull(dataLines, "Data lines cannot be null");
        if (dataLines.isEmpty()) {
            throw new IllegalArgumentException("Data lines cannot be empty");
        }
        this.dataLines = dataLines.toArray(String[]::new);
    }

    @Override
    public String[] getDataLines() {
        return dataLines;
    }

    @Override
    public String toString() {
        return "event: %s\n%s".formatted(getEventType(), String.join("\n", dataLines));
    }
}
