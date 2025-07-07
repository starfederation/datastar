package starfederation.datastar.unit;

import org.junit.jupiter.api.Test;
import starfederation.datastar.enums.EventType;
import starfederation.datastar.enums.ElementPatchMode;
import starfederation.datastar.events.PatchElements;

import static org.junit.jupiter.api.Assertions.*;

class PatchElementsTest {

    @Test
    void builderShouldGenerateCorrectEvent() {
        PatchElements event = PatchElements.builder()
                .selector("#feed")
                .mode(ElementPatchMode.Append)
                .useViewTransition(true)
                .data("<div id=\"feed\">\n<span>1</span>\n</div>")
                .build();

        String[] expectedDataLines = {
                "selector #feed",
                "mode append",
                "useViewTransition true",
                "elements <div id=\"feed\">",
                "elements <span>1</span>",
                "elements </div>"
        };

        assertArrayEquals(expectedDataLines, event.getDataLines());
        assertEquals(EventType.PatchElements, event.getEventType());
    }

    @Test
    void builderShouldExcludeDefaultValues() {
        PatchElements event = PatchElements.builder()
                .data("<div id=\"feed\">\n<span>1</span>\n</div>")
                .build();

        String[] expectedDataLines = {
                "elements <div id=\"feed\">",
                "elements <span>1</span>",
                "elements </div>"
        };

        assertArrayEquals(expectedDataLines, event.getDataLines());
    }

    @Test
    void builderShouldThrowExceptionForNullData() {
        assertThrows(IllegalArgumentException.class, () -> PatchElements.builder().build());
    }
}
