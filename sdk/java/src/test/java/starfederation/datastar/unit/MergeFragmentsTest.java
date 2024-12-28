package starfederation.datastar.unit;

import org.junit.jupiter.api.Test;
import starfederation.datastar.enums.EventType;
import starfederation.datastar.enums.FragmentMergeMode;
import starfederation.datastar.events.MergeFragments;

import static org.junit.jupiter.api.Assertions.*;

class MergeFragmentsTest {

    @Test
    void builderShouldGenerateCorrectEvent() {
        MergeFragments event = MergeFragments.builder()
                .selector("#feed")
                .mergeMode(FragmentMergeMode.Append)
                .settleDuration(500)
                .useViewTransition(true)
                .data("<div id=\"feed\">\n<span>1</span>\n</div>")
                .build();

        String[] expectedDataLines = {
                "selector #feed",
                "mergeMode append",
                "settleDuration 500",
                "useViewTransition true",
                "fragments <div id=\"feed\">",
                "fragments <span>1</span>",
                "fragments </div>"
        };

        assertArrayEquals(expectedDataLines, event.getDataLines());
        assertEquals(EventType.MergeFragments.toString(), event.getEventType().toString());
    }

    @Test
    void builderShouldExcludeDefaultValues() {
        MergeFragments event = MergeFragments.builder()
                .data("<div id=\"feed\">\n<span>1</span>\n</div>")
                .build();

        String[] expectedDataLines = {
                "fragments <div id=\"feed\">",
                "fragments <span>1</span>",
                "fragments </div>"
        };

        assertArrayEquals(expectedDataLines, event.getDataLines());
    }

    @Test
    void builderShouldThrowExceptionForNullData() {
        assertThrows(IllegalArgumentException.class, () -> MergeFragments.builder().build());
    }
}
