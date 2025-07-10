package starfederation.datastar.unit;

import org.junit.jupiter.api.Test;
import starfederation.datastar.enums.EventType;
import starfederation.datastar.events.PatchSignals;

import static org.junit.jupiter.api.Assertions.*;

class MergeSignalsTest {

    @Test
    void builderShouldGenerateCorrectEvent() {
        PatchSignals event = PatchSignals.builder()
                .data("{\"key\": \"value\"}")
                .onlyIfMissing(true)
                .build();

        String[] expectedDataLines = {
                "onlyIfMissing true",
                "signals {\"key\": \"value\"}"
        };

        assertArrayEquals(expectedDataLines, event.getDataLines());
        assertEquals(EventType.PatchSignals, event.getEventType());
    }

    @Test
    void builderShouldExcludeDefaultValues() {
        PatchSignals event = PatchSignals.builder()
                .data("{\"key\": \"value\"}")
                .build();

        String[] expectedDataLines = {
                "signals {\"key\": \"value\"}"
        };

        assertArrayEquals(expectedDataLines, event.getDataLines());
    }

    @Test
    void builderShouldThrowExceptionForNullData() {
        assertThrows(IllegalArgumentException.class, () -> PatchSignals.builder().build());
    }
}
