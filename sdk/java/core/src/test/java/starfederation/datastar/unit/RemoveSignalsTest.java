package starfederation.datastar.unit;

import org.junit.jupiter.api.Test;
import starfederation.datastar.enums.EventType;
import starfederation.datastar.events.RemoveSignals;

import static org.junit.jupiter.api.Assertions.*;

class RemoveSignalsTest {

    @Test
    void builderShouldGenerateCorrectEvent() {
        RemoveSignals event = RemoveSignals.builder()
                .addPath("user.name")
                .addPath("user.email")
                .build();

        String[] expectedDataLines = {
                "paths user.name",
                "paths user.email"
        };

        assertArrayEquals(expectedDataLines, event.getDataLines());
        assertEquals(EventType.RemoveSignals.toString(), event.getEventType().toString());
    }

    @Test
    void builderShouldThrowExceptionForEmptyPaths() {
        assertThrows(IllegalArgumentException.class, () -> RemoveSignals.builder().build());
    }
}
