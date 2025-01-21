package starfederation.datastar.events;

import starfederation.datastar.enums.EventType;

sealed interface DatastarEvent permits AbstractDatastarEvent {

    /**
     * Returns the event type.
     */
    EventType getEventType();

    /**
     * Returns the data lines for the event.
     */
    String[] getDataLines();

    /**
     * Returns the builder for the event.
     */

    static AbstractBuilder<AbstractDatastarEvent> builder() {
        throw new IllegalStateException("the builder method should be overridden to use the appropriate builder");
    }

}

