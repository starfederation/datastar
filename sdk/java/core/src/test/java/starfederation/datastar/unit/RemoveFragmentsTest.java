package starfederation.datastar.unit;

import org.junit.jupiter.api.Test;
import starfederation.datastar.enums.EventType;
import starfederation.datastar.events.RemoveFragments;

import static org.junit.jupiter.api.Assertions.*;

class RemoveFragmentsTest {

    @Test
    void builderShouldGenerateCorrectEvent() {
        RemoveFragments event = RemoveFragments.builder()
                .selector("#feed")
                .useViewTransition(true)
                .build();

        String[] expectedDataLines = {
                "selector #feed",
                "useViewTransition true"
        };

        assertArrayEquals(expectedDataLines, event.getDataLines());
        assertEquals(EventType.RemoveFragments.toString(), event.getEventType().toString());
    }

    @Test
    void builderShouldExcludeDefaultValues() {
        RemoveFragments event = RemoveFragments.builder()
                .selector("#feed")
                .build();

        String[] expectedDataLines = {
                "selector #feed"
        };

        assertArrayEquals(expectedDataLines, event.getDataLines());
    }

    @Test
    void builderShouldThrowExceptionForNullSelector() {
        assertThrows(IllegalArgumentException.class, () -> RemoveFragments.builder().build());
    }
}
