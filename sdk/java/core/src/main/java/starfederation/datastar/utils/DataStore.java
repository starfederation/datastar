package starfederation.datastar.utils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * A simple data store for managing key-value pairs with serialization/deserialization support.
 */
public class DataStore {

    private final ConcurrentMap<String, Object> store = new ConcurrentHashMap<>();
    private final ObjectMapper objectMapper = new ObjectMapper();

    /**
     * Adds a key-value pair to the store.
     *
     * @param key   The key (must be non-null and non-blank).
     * @param value The value (can be null).
     */
    public void put(String key, Object value) {
        if (key == null) {
            throw new NullPointerException("Key cannot be null");
        }
        if (key.trim().isEmpty()) {
            throw new IllegalArgumentException("Key cannot be blank");
        }
        store.put(key, value);
    }

    /**
     * Retrieves the value associated with the given key, cast to the specified type.
     *
     * @param key   The key.
     * @param clazz The expected type of the value.
     * @return The value associated with the key, or null if not found.
     * @throws IllegalArgumentException if the value is not of the expected type.
     */
    public <T> T get(String key, Class<T> clazz) {
        Object value = store.get(key);
        if (value == null) {
            return null;
        }
        if (!clazz.isInstance(value)) {
            throw new IllegalArgumentException("Value for key '" + key + "' is not of type " + clazz.getSimpleName());
        }
        return clazz.cast(value);
    }

    /**
     * Adds all entries from the given map to the store.
     *
     * @param data The map of key-value pairs.
     */
    public void putAll(Map<String, Object> data) {
        if (data == null) {
            throw new NullPointerException("Data map cannot be null");
        }
        store.putAll(data);
    }

    /**
     * Serializes the store to a JSON string.
     *
     * @return The JSON representation of the store.
     */
    public String toJson() {
        try {
            return objectMapper.writeValueAsString(store);
        } catch (JsonProcessingException e) {
            throw new RuntimeException("Failed to serialize DataStore to JSON", e);
        }
    }

    /**
     * Deserializes the given JSON string into the store.
     *
     * @param json The JSON string.
     * @throws IllegalArgumentException if the JSON is invalid.
     */
    public void fromJson(String json) {
        try {
            Map<String, Object> data = objectMapper.readValue(json, new TypeReference<>() {});
            store.clear();
            store.putAll(data);
        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Invalid JSON: " + json, e);
        }
    }

    @Override
    public String toString() {
        return "DataStore{store=" + store + '}';
    }
}
