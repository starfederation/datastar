package starfederation.datastar.unit;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import starfederation.datastar.adapters.request.RequestAdapter;
import starfederation.datastar.utils.SignalReader;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

class SignalReaderTest {

    @Mock
    private RequestAdapter mockRequest;

    private ConcurrentMap<String, Object> store;
    private AutoCloseable closeable;

    @BeforeEach
    void setUp() {
        closeable = MockitoAnnotations.openMocks(this);
        store = new ConcurrentHashMap<>();
    }

    @AfterEach
    void tearDown() throws Exception {
        closeable.close();
    }
    @Test
    void readSignalsShouldParseGetRequest() throws Exception {
        when(mockRequest.getMethod()).thenReturn("GET");
        when(mockRequest.getParameter("datastar")).thenReturn("{\"key1\":\"value1\",\"key2\":42}");

        SignalReader.readSignals(mockRequest, store);

        assertEquals(2, store.size());
        assertEquals("value1", store.get("key1"));
        assertEquals(42, store.get("key2"));
    }

    @Test
    void readSignalsShouldParsePostRequest() throws Exception {
        when(mockRequest.getMethod()).thenReturn("POST");
        when(mockRequest.getReader()).thenReturn(new BufferedReader(new StringReader("{\"key1\":\"value1\",\"key2\":42}")));

        SignalReader.readSignals(mockRequest, store);

        assertEquals(2, store.size());
        assertEquals("value1", store.get("key1"));
        assertEquals(42, store.get("key2"));
    }

    @Test
    void readSignalsShouldThrowExceptionForNullStore() {
        assertThrows(NullPointerException.class, () -> SignalReader.readSignals(mockRequest, null));
    }

    @Test
    void readSignalsShouldThrowExceptionForNullRequestAdapter() {
        assertThrows(NullPointerException.class, () -> SignalReader.readSignals(null, store));
    }

    @Test
    void readSignalsShouldThrowExceptionForMissingQueryParameter() throws Exception {
        when(mockRequest.getMethod()).thenReturn("GET");
        when(mockRequest.getParameter("datastar")).thenReturn(null);

        assertThrows(IllegalArgumentException.class, () -> SignalReader.readSignals(mockRequest, store));
    }

    @Test
    void readSignalsShouldThrowExceptionForEmptyPostBody() throws Exception {
        when(mockRequest.getMethod()).thenReturn("POST");
        when(mockRequest.getReader()).thenReturn(new BufferedReader(new StringReader("")));

        assertThrows(IllegalArgumentException.class, () -> SignalReader.readSignals(mockRequest, store));
    }

    @Test
    void readSignalsShouldThrowExceptionForInvalidJson() throws Exception {
        when(mockRequest.getMethod()).thenReturn("POST");
        when(mockRequest.getReader()).thenReturn(new BufferedReader(new StringReader("{invalid json}")));

        assertThrows(IOException.class, () -> SignalReader.readSignals(mockRequest, store));
    }
}
