package starfederation.datastar.unit;

import org.junit.jupiter.api.Test;
import starfederation.datastar.enums.EventType;
import starfederation.datastar.events.MergeSignals;

import static org.junit.jupiter.api.Assertions.*;

class MergeSignalsTest {

    @Test
    void builderShouldGenerateCorrectEvent() {
        MergeSignals event = MergeSignals.builder()
                .data("{\"key\": \"value\"}")
                .onlyIfMissing(true)
                .build();

        String[] expectedDataLines = {
                "onlyIfMissing true",
                "signals {\"key\": \"value\"}"
        };

        assertArrayEquals(expectedDataLines, event.getDataLines());
        assertEquals(EventType.MergeSignals.toString(), event.getEventType().toString());
    }

    @Test
    void builderShouldExcludeDefaultValues() {
        MergeSignals event = MergeSignals.builder()
                .data("{\"key\": \"value\"}")
                .build();

        String[] expectedDataLines = {
                "signals {\"key\": \"value\"}"
        };

        assertArrayEquals(expectedDataLines, event.getDataLines());
    }

    @Test
    void builderShouldThrowExceptionForNullData() {
        assertThrows(IllegalArgumentException.class, () -> MergeSignals.builder().build());
    }
}
