package starfederation.datastar.utils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import starfederation.datastar.adapters.request.RequestAdapter;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentMap;

import static starfederation.datastar.Consts.DATASTAR_KEY;

public class SignalReader {

    private static final ObjectMapper objectMapper = new ObjectMapper();

    /**
     * Parses incoming data from the browser and unmarshals it into the given store object.
     *
     * @param requestAdapter The request adapter wrapping the incoming request.
     * @param store          A Map that will hold the parsed data.
     * @throws IOException If an error occurs during reading or parsing.
     */
    public static void readSignals(RequestAdapter requestAdapter, ConcurrentMap<String, Object> store) throws IOException {
        Objects.requireNonNull(store, "store cannot be null");
        Objects.requireNonNull(requestAdapter, "requestAdapter cannot be null");

        String data;

        if ("GET".equalsIgnoreCase(requestAdapter.getMethod())) {
            // Handle GET requests by parsing the `datastar` query parameter
            data = requestAdapter.getParameter(DATASTAR_KEY);
            if (data == null || data.isBlank()) {
                throw new IllegalArgumentException("Missing 'datastar' query parameter in GET request.");
            }
        } else {
            // Handle other methods by reading the request body
            StringBuilder requestBody = new StringBuilder();
            try (BufferedReader reader = requestAdapter.getReader()) {
                String line;
                while ((line = reader.readLine()) != null) {
                    requestBody.append(line);
                }
            }

            data = requestBody.toString();
            if (data.isEmpty()) {
                throw new IllegalArgumentException("Request body cannot be empty.");
            }
        }

        // Parse the JSON data into the store
        try {
            Map<String, Object> parsedData = objectMapper.readValue(data, new TypeReference<>() {
            });
            store.putAll(parsedData);
        } catch (IOException e) {
            throw new IOException("Failed to parse JSON data.", e);
        }
    }
}
