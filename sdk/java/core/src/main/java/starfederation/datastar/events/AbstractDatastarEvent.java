package starfederation.datastar.events;

import starfederation.datastar.enums.EventType;

import java.util.List;
import java.util.Objects;

sealed public abstract class AbstractDatastarEvent implements DatastarEvent permits MergeFragments, MergeSignals, RemoveFragments,
        RemoveSignals, ExecuteScript, CustomEvent {

    private final EventType eventType;
    private final String[] dataLines;

    protected AbstractDatastarEvent(EventType eventType, List<String> dataLines) {
        Objects.requireNonNull(eventType, "Event type cannot be null");
        Objects.requireNonNull(dataLines, "Data lines cannot be null");
        if (dataLines.isEmpty()) {
            throw new IllegalArgumentException("Data lines cannot be empty");
        }
        this.eventType = eventType;
        this.dataLines = dataLines.toArray(String[]::new);
    }


    @Override
    public EventType getEventType() {
        return eventType;
    }

    @Override
    public String[] getDataLines() {
        return dataLines;
    }


    @Override
    public String toString() {
        return String.join("\n", dataLines);
    }

}
