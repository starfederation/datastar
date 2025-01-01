package starfederation.datastar.events;

import starfederation.datastar.enums.EventType;

import java.util.List;

public abstract non-sealed class CustomEvent extends AbstractDatastarEvent {

    protected CustomEvent(EventType eventType, List<String> dataLines) {
        super(eventType, dataLines);
    }

}
