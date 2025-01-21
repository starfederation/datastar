package starfederation.datastar.adapters.response;

import java.io.PrintWriter;

public interface ResponseAdapter {
    /**
     * Provides a `PrintWriter` for writing content to the response.
     *
     * @return The `PrintWriter` for the response.
     * @throws Exception if the writer cannot be retrieved.
     */
    PrintWriter getWriter() throws Exception;

    /**
     * Sets a header on the response.
     *
     * @param name  The name of the header.
     * @param value The value of the header.
     */
    void setHeader(String name, String value);

    /**
     * Sets the content type of the response.
     *
     * @param contentType The MIME type of the response content.
     */
    void setContentType(String contentType);

    /**
     * Sets the character encoding for the response.
     *
     * @param encoding The character encoding to use.
     */
    void setCharacterEncoding(String encoding);

    /**
     * Sets the HTTP status code for the response.
     *
     * @param status The HTTP status code.
     */
    void setStatus(int status);
}
