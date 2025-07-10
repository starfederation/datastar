package starfederation.datastar.unit;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.opentest4j.AssertionFailedError;
import starfederation.datastar.adapters.response.AbstractResponseAdapter;
import starfederation.datastar.events.PatchElements;
import starfederation.datastar.utils.ServerSentEventGenerator;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

class ServerSentEventGeneratorTest {
    private static final AbstractResponseAdapter mockResponse;
    private static final StringWriter stringWriter;
    private static final ServerSentEventGenerator generator;

    static {
        stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter);
        mockResponse = Mockito.mock(AbstractResponseAdapter.class);
        try {
            when(mockResponse.getWriter()).thenReturn(printWriter);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        generator = new ServerSentEventGenerator(mockResponse);
    }

    @AfterAll
    static void end() {
        generator.close();
    }

    @AfterEach
    void tearDown() throws Exception {
        resetCounter();
        stringWriter.getBuffer().setLength(0);

    }
    private void resetCounter() throws Exception {
        Field counterField = ServerSentEventGenerator.class.getDeclaredField("COUNTER");
        counterField.setAccessible(true);
        AtomicLong counter = (AtomicLong) counterField.get(null);
        counter.set(-1);
        counterField.setAccessible(false);
    }

    @Test
    void sendShouldWriteValidEvent() {
        PatchElements event = PatchElements.builder()
                .selector("#test")
                .data("<div>test</div>")
                .build();

        generator.send(event);

        String expectedOutput = """
                event: datastar-patch-elements
                id: 0
                data: selector #test
                data: elements <div>test</div>
                
                """;

        assertEquals(expectedOutput, stringWriter.toString());
    }

    @Test
    void sendWithIdShouldWriteValidEvent() {
        PatchElements event = PatchElements.builder()
                .selector("#test")
                .data("<div>test</div>")
                .build();

        generator.send(event, "custom-id");

        String expectedOutput = """
                event: datastar-patch-elements
                id: custom-id
                data: selector #test
                data: elements <div>test</div>
                
                """;

        assertEquals(expectedOutput, stringWriter.toString());
    }

    @Test
    void sendShouldGenerateIncrementingIds() {
        PatchElements event1 = PatchElements.builder()
                .selector("#test1")
                .data("<div>test1</div>")
                .build();

        PatchElements event2 = PatchElements.builder()
                .selector("#test2")
                .data("<div>test2</div>")
                .build();

        generator.send(event1);
        generator.send(event2);

        String expectedOutput = """
                event: datastar-patch-elements
                id: 0
                data: selector #test1
                data: elements <div>test1</div>
                
                event: datastar-patch-elements
                id: 1
                data: selector #test2
                data: elements <div>test2</div>
                
                """;

        assertEquals(expectedOutput, stringWriter.toString());
    }

    @Test
    void sendShouldThrowExceptionForNullEvent() {
        assertThrows(IllegalArgumentException.class, () -> generator.send(null));
    }

    @Test
    void constructorShouldThrowExceptionForNullResponse() {
        assertThrows(IllegalArgumentException.class, () -> {
            try (ServerSentEventGenerator ignored = new ServerSentEventGenerator(null)) {
                throw new AssertionFailedError();
            }
        });
    }
}
