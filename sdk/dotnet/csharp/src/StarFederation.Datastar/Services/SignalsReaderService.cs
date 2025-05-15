using System.Text;
using System.Text.Json;
using Microsoft.AspNetCore.Http;
using StarFederation.Datastar.Types;

namespace StarFederation.Datastar.Services;

/// <summary>
///     Implementation of IDatastarSignalsReaderService.
/// </summary>
public class SignalsReaderService : IDatastarSignalsReaderService
{
    private HttpRequest HttpRequest { get; }
    
    /// <summary>
    ///     Initializes a new instance of the SignalsHttpHandlers class.
    /// </summary>
    /// <param name="httpRequest">The HTTP request.</param>
    public SignalsReaderService(HttpRequest httpRequest)
    {
        HttpRequest = httpRequest;
        HttpRequest.EnableBuffering();
    }
    

    /// <inheritdoc />
    public async Task<DatastarSignals?> ReadSignalsAsync()
    {
        var signals = await Handler.ReadSignalsAsync();
        return signals;
    }

    /// <inheritdoc />
    public async Task<T?> ReadSignalsAsync<T>() where T : class
    {
        var signals = await Handler.ReadSignalsAsync<T>();
        return signals;
    }
    
    

    /// <summary>
    ///     Reads the signals from the request.
    /// </summary>
    /// <returns>A task that represents the asynchronous read operation. The result contains the signals if present.</returns>
    public async Task<DatastarSignals?> ReadSignalsAsync()
    {
        var rawSignals = await ReadRawSignalsAsync();
        return rawSignals != null
            ? new DatastarSignals(rawSignals)
            : null;
    }

    /// <summary>
    ///     Reads the signals from the request and deserializes them to the specified type.
    /// </summary>
    /// <typeparam name="T">The type to deserialize to.</typeparam>
    /// <returns>A task that represents the asynchronous read operation. The result contains the deserialized data if present.</returns>
    public Task<T?> ReadSignalsAsync<T>() where T : class
    {
        return ReadSignalsAsync<T>(JsonSerializerOptions.Default);
    }

    /// <summary>
    ///     Reads the signals from the request and deserializes them to the specified type using the provided options.
    /// </summary>
    /// <typeparam name="T">The type to deserialize to.</typeparam>
    /// <param name="jsonSerializerOptions">The JSON serializer options to use.</param>
    /// <returns>A task that represents the asynchronous read operation. The result contains the deserialized data if present.</returns>
    public async Task<T?> ReadSignalsAsync<T>(JsonSerializerOptions jsonSerializerOptions) where T : class
    {
        var signals = await ReadRawSignalsAsync();

        if (signals == null)
        {
            return null;
        }

        try
        {
            var deserialized = JsonSerializer.Deserialize<T>(signals, jsonSerializerOptions);
            return deserialized;
        }
        catch
        {
            return null;
        }
    }

    private async Task<string?> ReadRawSignalsAsync()
    {
        if (HttpRequest.Method == "GET")
        {
            // Handle GET request (if implemented)
            return null;
        }

        // Read the request body
        HttpRequest.Body.Position = 0;

        using var reader = new StreamReader(HttpRequest.Body, Encoding.UTF8, leaveOpen: true);
        var body = await reader.ReadToEndAsync();

        if (string.IsNullOrWhiteSpace(body))
        {
            return null;
        }

        return body;
    }
}