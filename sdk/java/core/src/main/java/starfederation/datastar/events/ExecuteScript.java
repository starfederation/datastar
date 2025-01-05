package starfederation.datastar.events;

import starfederation.datastar.enums.EventType;

import java.util.ArrayList;
import java.util.List;

import static starfederation.datastar.Consts.*;

public final class ExecuteScript extends AbstractDatastarEvent {

    private ExecuteScript(EventType eventType, List<String> dataLines) {
        super(eventType, dataLines);
    }

    public static Builder builder() {
        return new Builder();
    }

    public static final class Builder extends AbstractBuilder<ExecuteScript> {
        private String script;
        private boolean autoRemove = DEFAULT_EXECUTE_SCRIPT_AUTO_REMOVE; // Default
        private String attributes = DEFAULT_EXECUTE_SCRIPT_ATTRIBUTES; // Default

        public Builder script(String script) {
            if (script == null || script.isBlank()) {
                throw new IllegalArgumentException("Script cannot be null or empty");
            }
            this.script = script;
            return this;
        }

        public Builder autoRemove(boolean autoRemove) {
            this.autoRemove = autoRemove;
            return this;
        }

        public Builder attributes(String attributes) {
            if (attributes != null && !attributes.trim().isEmpty()) {
                this.attributes = attributes;
            }
            return this;
        }

        @Override
        public ExecuteScript build() {
            if (script == null || script.isBlank()) {
                throw new IllegalArgumentException("Script cannot be null or empty");
            }

            List<String> dataLines = new ArrayList<>();

            // Add autoRemove if false (default is true)
            if (autoRemove != DEFAULT_EXECUTE_SCRIPT_AUTO_REMOVE) {
                dataLines.add(AUTO_REMOVE_DATALINE_LITERAL + autoRemove);
            }

            // Add attributes if not default
            if (attributes != null && !attributes.equals(DEFAULT_EXECUTE_SCRIPT_ATTRIBUTES)) {
                dataLines.add(ATTRIBUTES_DATALINE_LITERAL + attributes);
            }

            // Add script
            dataLines.add(SCRIPT_DATALINE_LITERAL + script);

            return new ExecuteScript(EventType.ExecuteScript, dataLines);
        }
    }
}
