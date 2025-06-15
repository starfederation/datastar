using System.Diagnostics;
using System.Text.Json;
// ReSharper disable PossibleMultipleEnumeration

namespace StarFederation.Datastar;

internal static class Utilities
{
    public static JsonElement? Get(this JsonElement element, string name) =>
        element.ValueKind != JsonValueKind.Null && element.ValueKind != JsonValueKind.Undefined && element.TryGetProperty(name, out JsonElement value)
            ? value
            : null;

    public static JsonElement? Get(this JsonElement element, int index) =>
        element.ValueKind == JsonValueKind.Null || element.ValueKind == JsonValueKind.Undefined ? null :
        index < element.GetArrayLength() ? element[index] : null;

    /// <summary>
    /// Given a dot-separated path into a JSON structure, will return the JsonElement or null
    /// </summary>
    /// <param name="jsonElement">the head of the tree</param>
    /// <param name="path">dot-separated path</param>
    /// <returns>JsonElement if found; null if the path cannot be followed</returns>
    public static JsonElement? GetFromPath(this JsonElement jsonElement, string path)
    {
        JsonElement? GetFromPathCore(JsonElement jElement, IEnumerable<string> pathElements)
        {
            if (!pathElements.Any())
            {
                return null;
            }

            while (true)
            {
                if (pathElements.Count() == 1)
                {
                    return jElement.Get(pathElements.First());
                }
                if (jElement.Get(pathElements.First()) is not { } childElement)
                {
                    return null;
                }
                jElement = childElement;
                pathElements = pathElements.Skip(1);
            }
        }
        return GetFromPathCore(jsonElement, path.Split('.'));
    }

    /// <summary>
    /// Given a dot-separated path into a JSON structure, will return the value or null
    /// </summary>
    /// <param name="jsonElement">the head of the tree</param>
    /// <param name="path">dot-separated path</param>
    /// <param name="jsonElementType">the type to convert the value into</param>
    /// <param name="jsonSerializerOptions">options to the serializer</param>
    /// <returns>value if found; null if the path cannot be followed</returns>
    public static object? GetValueFromPath(this JsonElement jsonElement, string path, Type jsonElementType, JsonSerializerOptions jsonSerializerOptions)
    {
        JsonElement? childJsonElement = GetFromPath(jsonElement, path);
        return childJsonElement?.Deserialize(jsonElementType, jsonSerializerOptions);
    }

    public static Tuple<TKey, TValue> AsTuple<TKey, TValue>(this KeyValuePair<TKey, TValue> keyValuePair) => new(keyValuePair.Key, keyValuePair.Value);
}