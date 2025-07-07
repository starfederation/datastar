package starfederation.datastar.events;

import starfederation.datastar.enums.EventType;

import java.util.ArrayList;
import java.util.List;

import static starfederation.datastar.Consts.*;

public final class ExecuteScript extends AbstractDatastarEvent {

    private ExecuteScript(EventType eventType, List<String> dataLines) {
        super(eventType, dataLines);
    }

    @Override
    public EventType getEventType() {
        return EventType.PatchElements;
    }

    public static Builder builder() {
        return new Builder();
    }

    public static final class Builder extends AbstractBuilder<ExecuteScript> {
        private String script;
        private boolean autoRemove = true; // Default
        private String attributes = ""; // Default

        /**
         * JavaScript to execute on the client. Do not wrap in HTML-script tags.
         * @param script valid JavaScript
         * @return a builder for fluent configuration
         */
        public Builder script(String script) {
            if (script == null || script.isBlank()) {
                throw new IllegalArgumentException("Script cannot be null or empty");
            }
            this.script = script;
            return this;
        }
        private Builder() {
        }

        public Builder autoRemove(boolean autoRemove) {
            this.autoRemove = autoRemove;
            return this;
        }

        public Builder attributes(String attributes) {
            if (attributes != null && !attributes.isBlank()) {
                this.attributes = attributes.trim();
            }
            return this;
        }

        @Override
        public ExecuteScript build() {
            if (script == null || script.isBlank()) {
                throw new IllegalArgumentException("Script cannot be null or empty");
            }

            var wrappedScript = new StringBuilder("<script");
            List<String> dataLines = new ArrayList<>();
            
            // Add attributes if not default
            if (attributes != null && !attributes.isBlank()) {
                wrappedScript.append(' ').append(attributes);
            }

            // Add autoRemove
            if (autoRemove) {
                wrappedScript.append(" data-effect=\"el.remove()\"");
            }

            wrappedScript.append(">").append(script).append("</script>");

            // Add script
            wrappedScript.toString().lines()
                    .filter(line -> !line.isBlank())
                    .forEach(line -> dataLines.add(ELEMENTS_DATALINE_LITERAL + line));

            return new ExecuteScript(EventType.PatchElements, dataLines);
        }
    }
}
