using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;
using System.Web;
using Microsoft.AspNetCore.Http;

namespace StarFederation.Datastar.Extensions;

public static class SignalReaderExtensions
{
    /// <summary>
    /// Reads the signals from the request and returns them as a string.
    /// </summary>
    /// <param name="httpRequest"></param>
    /// <returns></returns>
    public static async Task<string?> ReadSignalsAsStringAsync(this HttpRequest httpRequest)
    {
        if (httpRequest.Method == "GET" && httpRequest.Query.TryGetValue("dataStar", out var dataStarQuery))
        {
            var dataStarSignalJson = HttpUtility.HtmlDecode(dataStarQuery);
            if (string.IsNullOrWhiteSpace(dataStarSignalJson))
            {
                return null;
            }
            
            return dataStarSignalJson;
        }

        //TODO: Is this needed?
        httpRequest.EnableBuffering();
        // Read the request body
        httpRequest.Body.Position = 0;

        using var reader = new StreamReader(httpRequest.Body, Encoding.UTF8);
        var body = await reader.ReadToEndAsync();

        if (string.IsNullOrWhiteSpace(body))
        {
            return null;
        }

        return body;
    }
    
    /// <summary>
    /// Reads the signals from the request and deserializes them into a JsonNode.
    /// </summary>
    /// <param name="httpRequest"></param>
    /// <returns></returns>
    public static async Task<JsonNode?> ReadSignalsAsJsonNodeAsync(this HttpRequest httpRequest)
    {
        var jsonString = await httpRequest.ReadSignalsAsStringAsync();
        if (string.IsNullOrWhiteSpace(jsonString))
        {
            return null;
        }

        var jsonNode = JsonNode.Parse(jsonString);
        return jsonNode;
    }

    /// <summary>
    /// Reads the signals from the request and deserializes them into an object of type T.
    /// </summary>
    /// <param name="httpRequest"></param>
    /// <param name="jsonSerializerOptions"> The JSON serializer options to use for deserialization. If null, the default options will be used.</param>
    /// <typeparam name="T"></typeparam>
    /// <returns> The deserialized object of type T, or null if the request body is empty or invalid.</returns>
    public static async Task<T?> ReadSignalsAsync<T>(this HttpRequest httpRequest, JsonSerializerOptions? jsonSerializerOptions = null)
    {
        var jsonString = await httpRequest.ReadSignalsAsStringAsync();
        if (string.IsNullOrWhiteSpace(jsonString))
        {
            return default;
        }

        var obj = JsonSerializer.Deserialize<T>(jsonString, jsonSerializerOptions ?? JsonSerializerOptions.Default);
        return obj;
    }
}