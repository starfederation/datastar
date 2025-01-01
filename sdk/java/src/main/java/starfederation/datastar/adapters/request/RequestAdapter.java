package starfederation.datastar.adapters.request;

import java.io.BufferedReader;
import java.io.IOException;

public interface RequestAdapter {
    /**
     * Gets the HTTP method (e.g., GET, POST) of the request.
     *
     * @return The HTTP method as a string.
     */
    String getMethod();

    /**
     * Retrieves a query parameter by name.
     *
     * @param name The name of the query parameter.
     * @return The value of the query parameter, or null if not present.
     */
    String getParameter(String name);

    /**
     * Retrieves a BufferedReader to read the request body.
     *
     * @return A BufferedReader for the request body.
     * @throws IOException If an error occurs during reading.
     */
    BufferedReader getReader() throws IOException;
}
