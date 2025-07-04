package starfederation.datastar.unit;

import org.junit.jupiter.api.Test;
import starfederation.datastar.enums.EventType;
import starfederation.datastar.events.PatchSignals;

import static org.junit.jupiter.api.Assertions.*;

class RemoveSignalsTest {

    @Test
    void builderShouldGenerateCorrectEvent() {
        PatchSignals event = PatchSignals.builder()
                .data("{\"user\": {\"name\": null, \"email\": null}}")
                .build();

        String[] expectedDataLines = {
                "signals {\"user\": {\"name\": null, \"email\": null}}"
        };

        assertArrayEquals(expectedDataLines, event.getDataLines());
        assertEquals(EventType.PatchSignals, event.getEventType());
    }

    @Test
    void builderShouldThrowExceptionForEmptyPaths() {
        assertThrows(IllegalArgumentException.class, () -> PatchSignals.builder().build());
    }
}
