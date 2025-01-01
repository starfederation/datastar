package starfederation.datastar.adapters.request;

import jakarta.ws.rs.container.ContainerRequestContext;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.charset.UnsupportedCharsetException;
import java.util.Objects;

/**
 * Adapter to abstract HTTP request handling in JAX-RS.
 */
public class JaxRsRequestAdapter extends AbstractRequestAdapter {

    @Context
    public ContainerRequestContext requestContext;

    @Override
    public String getMethod() {
        return requestContext.getMethod();
    }

    @Override
    public String getParameter(String name) {
        Objects.requireNonNull(name, "Parameter name cannot be null");
        return requestContext.getUriInfo().getQueryParameters().getFirst(name);
    }

    @Override
    public BufferedReader getReader() {
        return new BufferedReader(new InputStreamReader(requestContext.getEntityStream(), getCharacterEncoding()));
    }


    /**
     * Determines the character encoding from the Content-Type header.
     *
     * @return The determined or default character encoding.
     */
    private Charset getCharacterEncoding() {
        String contentType = requestContext.getHeaderString(HttpHeaders.CONTENT_TYPE);
        String charsetFieldName = "charset=";
        if (contentType != null && contentType.contains(charsetFieldName)) {
            String charset = contentType.substring(contentType.indexOf(charsetFieldName) + charsetFieldName.length()).trim();
            try {
                return Charset.forName(charset);
            } catch (UnsupportedCharsetException ignored) {
            }

        }
        return StandardCharsets.UTF_8;

    }
}
