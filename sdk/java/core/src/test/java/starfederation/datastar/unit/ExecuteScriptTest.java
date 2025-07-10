package starfederation.datastar.unit;

import org.junit.jupiter.api.Test;
import starfederation.datastar.enums.EventType;
import starfederation.datastar.events.ExecuteScript;

import static org.junit.jupiter.api.Assertions.*;

class ExecuteScriptTest {

    @Test
    void builderShouldGenerateCorrectEvent() {
        ExecuteScript event = ExecuteScript.builder()
                .script("console.log('Hello World');")
                .autoRemove(false)
                .attributes("type='module'")
                .build();

        String[] expectedDataLines = {
                "elements <script type='module'>console.log('Hello World');</script>"
        };

        assertArrayEquals(expectedDataLines, event.getDataLines());
        assertEquals(EventType.PatchElements, event.getEventType());
    }

    @Test
    void builderShouldExcludeDefaultValues() {
        ExecuteScript event = ExecuteScript.builder()
                .script("console.log('Hello World');")
                .build();

        String[] expectedDataLines = {
                "elements <script data-effect=\"el.remove()\">console.log('Hello World');</script>"
        };

        assertArrayEquals(expectedDataLines, event.getDataLines());
    }

    @Test
    void builderShouldThrowExceptionForNullScript() {
        assertThrows(IllegalArgumentException.class, () -> ExecuteScript.builder().build());
    }
}
