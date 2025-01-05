package starfederation.datastar.unit;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import starfederation.datastar.utils.DataStore;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class DataStoreTest {

    private DataStore dataStore;

    @BeforeEach
    void setUp() {
        dataStore = new DataStore();
    }

    @Test
    void putShouldAddKeyValuePair() {
        dataStore.put("key1", "value1");
        assertEquals("value1", dataStore.get("key1", String.class));
    }

    @Test
    void putShouldThrowExceptionForNullKey() {
        assertThrows(NullPointerException.class, () -> dataStore.put(null, "value"));
    }

    @Test
    void putShouldThrowExceptionForBlankKey() {
        assertThrows(IllegalArgumentException.class, () -> dataStore.put("   ", "value"));
    }

    @Test
    void getShouldReturnNullForNonExistentKey() {
        assertNull(dataStore.get("nonexistent", String.class));
    }

    @Test
    void getShouldThrowExceptionForTypeMismatch() {
        dataStore.put("key1", "stringValue");
        assertThrows(IllegalArgumentException.class, () -> dataStore.get("key1", Integer.class));
    }

    @Test
    void putAllShouldAddAllEntries() {
        Map<String, Object> data = new HashMap<>();
        data.put("key1", "value1");
        data.put("key2", 123);
        data.put("key3", true);

        dataStore.putAll(data);

        assertEquals("value1", dataStore.get("key1", String.class));
        assertEquals(123, dataStore.get("key2", Integer.class));
        assertEquals(true, dataStore.get("key3", Boolean.class));
    }

    @Test
    void toJsonShouldSerializeStoreToJson() {
        dataStore.put("key1", "value1");
        dataStore.put("key2", 42);
        dataStore.put("key3", true);

        String expectedJson = "{\"key1\":\"value1\",\"key2\":42,\"key3\":true}";
        String actualJson = dataStore.toJson();

        assertEquals(expectedJson, actualJson);
    }

    @Test
    void fromJsonShouldDeserializeJsonToStore() {
        String json = "{\"key1\":\"value1\",\"key2\":42,\"key3\":true}";

        dataStore.fromJson(json);

        assertEquals("value1", dataStore.get("key1", String.class));
        assertEquals(42, dataStore.get("key2", Integer.class));
        assertEquals(true, dataStore.get("key3", Boolean.class));
    }

    @Test
    void fromJsonShouldThrowExceptionForInvalidJson() {
        String invalidJson = "{invalid json}";

        assertThrows(IllegalArgumentException.class, () -> dataStore.fromJson(invalidJson));
    }

    @Test
    void toStringShouldReturnCorrectRepresentation() {
        dataStore.put("key1", "value1");
        dataStore.put("key2", 42);

        String expectedString = "DataStore{store={key1=value1, key2=42}}";
        assertEquals(expectedString, dataStore.toString());
    }
}
