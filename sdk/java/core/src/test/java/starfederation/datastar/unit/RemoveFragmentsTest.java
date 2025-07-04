package starfederation.datastar.unit;

import org.junit.jupiter.api.Test;

import starfederation.datastar.enums.ElementPatchMode;
import starfederation.datastar.enums.EventType;
import starfederation.datastar.events.PatchElements;

import static org.junit.jupiter.api.Assertions.*;

class RemoveFragmentsTest {

    @Test
    void builderShouldGenerateCorrectEvent() {
        PatchElements event = PatchElements.builder()
                .selector("#feed")
                .mode(ElementPatchMode.Remove)
                .useViewTransition(true)
                .build();

        String[] expectedDataLines = {
                "selector #feed",
                "mode remove",
                "useViewTransition true"
        };

        assertArrayEquals(expectedDataLines, event.getDataLines());
        assertEquals(EventType.PatchElements, event.getEventType());
    }

    @Test
    void builderShouldExcludeDefaultValues() {
        PatchElements event = PatchElements.builder()
                .selector("#feed")
                .mode(ElementPatchMode.Remove)
                .build();

        String[] expectedDataLines = {
                "selector #feed",
                "mode remove"
        };

        assertArrayEquals(expectedDataLines, event.getDataLines());
    }

    @Test
    void builderShouldThrowExceptionForNullSelector() {
        assertThrows(IllegalArgumentException.class, () -> PatchElements.builder().build());
    }
}
