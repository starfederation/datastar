package starfederation.datastar.unit;

import jakarta.ws.rs.container.ContainerRequestContext;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.UriInfo;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import starfederation.datastar.adapters.request.JaxRsRequestAdapter;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;

class JaxRsRequestAdapterTest {

    @Mock
    private ContainerRequestContext mockRequestContext;

    @Mock
    private UriInfo mockUriInfo;

    private JaxRsRequestAdapter adapter;
    private AutoCloseable closeable;

    @BeforeEach
    void setUp() {
        closeable = MockitoAnnotations.openMocks(this);

        adapter = new JaxRsRequestAdapter();
        adapter.requestContext = mockRequestContext;

        // Mock URI Info
        when(mockRequestContext.getUriInfo()).thenReturn(mockUriInfo);
    }

    @AfterEach
    void tearDown() throws Exception {
        closeable.close();
    }

    @Test
    void getMethodShouldReturnHttpMethod() {
        when(mockRequestContext.getMethod()).thenReturn("POST");

        String method = adapter.getMethod();

        assertEquals("POST", method);
    }

    @Test
    void getReaderShouldReturnBufferedReaderWithUtf8EncodingWhenNoCharsetSpecified() throws Exception {
        when(mockRequestContext.getHeaderString(HttpHeaders.CONTENT_TYPE)).thenReturn("application/json");
        InputStream bodyStream = new ByteArrayInputStream("{\"key\":\"value\"}".getBytes(StandardCharsets.UTF_8));
        when(mockRequestContext.getEntityStream()).thenReturn(bodyStream);

        assertNotNull(adapter.getReader());
        assertEquals("{\"key\":\"value\"}", adapter.getReader().readLine());
    }
}
