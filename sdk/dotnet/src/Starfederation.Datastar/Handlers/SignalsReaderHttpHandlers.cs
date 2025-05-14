using System.Text;
using System.Text.Json;
using Microsoft.AspNetCore.Http;
using Starfederation.Datastar.Types;

namespace Starfederation.Datastar.Handlers;

/// <summary>
///     Implementation of IReadSignals, for reading the Signals from the HttpRequest
/// </summary>
public class SignalsReaderHttpHandlers : ISignalsReader
{
    /// <summary>
    ///     Initializes a new instance of the SignalsHttpHandlers class.
    /// </summary>
    /// <param name="httpRequest">The HTTP request.</param>
    public SignalsReaderHttpHandlers(HttpRequest httpRequest)
    {
        HttpRequest = httpRequest;
        HttpRequest.EnableBuffering();
    }

    /// <summary>
    ///     Gets the HTTP request.
    /// </summary>
    public HttpRequest HttpRequest { get; }

    /// <summary>
    ///     Reads the signals from the request.
    /// </summary>
    /// <returns>A task that represents the asynchronous read operation. The result contains the signals if present.</returns>
    public async Task<Optional<DatastarSignals>> ReadSignalsAsync()
    {
        var rawSignals = await ReadRawSignalsAsync();
        return rawSignals.HasValue
            ? Optional<DatastarSignals>.Some(new DatastarSignals(rawSignals.Value))
            : Optional<DatastarSignals>.None;
    }

    /// <summary>
    ///     Reads the signals from the request and deserializes them to the specified type.
    /// </summary>
    /// <typeparam name="T">The type to deserialize to.</typeparam>
    /// <returns>A task that represents the asynchronous read operation. The result contains the deserialized data if present.</returns>
    public Task<Optional<T>> ReadSignalsAsync<T>()
    {
        return ReadSignalsAsync<T>(JsonSerializerOptions.Default);
    }

    /// <summary>
    ///     Reads the signals from the request and deserializes them to the specified type using the provided options.
    /// </summary>
    /// <typeparam name="T">The type to deserialize to.</typeparam>
    /// <param name="jsonSerializerOptions">The JSON serializer options to use.</param>
    /// <returns>A task that represents the asynchronous read operation. The result contains the deserialized data if present.</returns>
    public async Task<Optional<T>> ReadSignalsAsync<T>(JsonSerializerOptions jsonSerializerOptions)
    {
        var signals = await ReadRawSignalsAsync();

        if (!signals.HasValue)
        {
            return Optional<T>.None;
        }

        try
        {
            var deserialized = JsonSerializer.Deserialize<T>(signals.Value, jsonSerializerOptions);
            return deserialized == null
                ? Optional<T>.None
                : Optional<T>.Some(deserialized);
        }
        catch
        {
            return Optional<T>.None;
        }
    }

    private async Task<Optional<string>> ReadRawSignalsAsync()
    {
        if (HttpRequest.Method == "GET")
        {
            // Handle GET request (if implemented)
            return Optional<string>.None;
        }

        // Read the request body
        HttpRequest.Body.Position = 0;

        using var reader = new StreamReader(HttpRequest.Body, Encoding.UTF8, leaveOpen: true);
        var body = await reader.ReadToEndAsync();

        if (string.IsNullOrWhiteSpace(body))
        {
            return Optional<string>.None;
        }

        return Optional<string>.Some(body);
    }
}