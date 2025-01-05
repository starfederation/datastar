package starfederation.datastar.events;

public abstract class AbstractBuilder<T extends AbstractDatastarEvent> {
    abstract T build();
}
