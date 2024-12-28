package starfederation.datastar.unit;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import starfederation.datastar.adapters.response.AbstractResponseAdapter;
import starfederation.datastar.events.MergeFragments;
import starfederation.datastar.utils.ServerSentEventGenerator;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

class ServerSentEventGeneratorTest {

    @Mock
    private AbstractResponseAdapter mockResponse;

    private ServerSentEventGenerator generator;
    private StringWriter stringWriter;
    private AutoCloseable closeable;

    @BeforeEach
    void setUp() throws Exception {
        closeable = MockitoAnnotations.openMocks(this);
        stringWriter = new StringWriter();
        when(mockResponse.getWriter()).thenReturn(new PrintWriter(stringWriter));

        generator = new ServerSentEventGenerator(mockResponse);
    }

    @AfterEach
    void tearDown() throws Exception {
        resetCounter();
        generator.close();
        closeable.close();
    }

    private void resetCounter() throws Exception {
        Field counterField = ServerSentEventGenerator.class.getDeclaredField("counter");
        counterField.setAccessible(true);
        AtomicLong counter = (AtomicLong) counterField.get(null);
        counter.set(-1);
        counterField.setAccessible(false);
    }

    @Test
    void sendShouldWriteValidEvent() {
        MergeFragments event = MergeFragments.builder()
                .selector("#test")
                .data("<div>test</div>")
                .build();

        generator.send(event);

        String expectedOutput = """
                event: datastar-merge-fragments
                id: 0
                data: selector #test
                data: fragments <div>test</div>
                
                """;

        assertEquals(expectedOutput, stringWriter.toString());
    }

    @Test
    void sendWithIdShouldWriteValidEvent() {
        MergeFragments event = MergeFragments.builder()
                .selector("#test")
                .data("<div>test</div>")
                .build();

        generator.send(event, "custom-id");

        String expectedOutput = """
                event: datastar-merge-fragments
                id: custom-id
                data: selector #test
                data: fragments <div>test</div>
                
                """;

        assertEquals(expectedOutput, stringWriter.toString());
    }

    @Test
    void sendShouldGenerateIncrementingIds() {
        MergeFragments event1 = MergeFragments.builder()
                .selector("#test1")
                .data("<div>test1</div>")
                .build();

        MergeFragments event2 = MergeFragments.builder()
                .selector("#test2")
                .data("<div>test2</div>")
                .build();

        generator.send(event1);
        generator.send(event2);

        String expectedOutput = """
                event: datastar-merge-fragments
                id: 0
                data: selector #test1
                data: fragments <div>test1</div>
                
                event: datastar-merge-fragments
                id: 1
                data: selector #test2
                data: fragments <div>test2</div>
                
                """;

        assertEquals(expectedOutput, stringWriter.toString());
    }

    @Test
    void sendShouldThrowExceptionForNullEvent() {
        assertThrows(IllegalArgumentException.class, () -> generator.send(null));
    }

    @Test
    void constructorShouldThrowExceptionForNullResponse() {
        assertThrows(IllegalArgumentException.class, () -> new ServerSentEventGenerator(null));
    }
}
