using System.Text.Json;

namespace Starfederation.Datastar.Types;

/// <summary>
///     Interface for reading signals from a request.
/// </summary>
public interface ISignalsReader
{
    /// <summary>
    ///     Reads the signals from the request.
    /// </summary>
    /// <returns>A task that represents the asynchronous read operation. The result contains the signals if present.</returns>
    Task<Optional<DatastarSignals>> ReadSignalsAsync();

    /// <summary>
    ///     Reads the signals from the request and deserializes them to the specified type.
    /// </summary>
    /// <typeparam name="T">The type to deserialize to.</typeparam>
    /// <returns>A task that represents the asynchronous read operation. The result contains the deserialized data if present.</returns>
    Task<Optional<T>> ReadSignalsAsync<T>();

    /// <summary>
    ///     Reads the signals from the request and deserializes them to the specified type using the provided options.
    /// </summary>
    /// <typeparam name="T">The type to deserialize to.</typeparam>
    /// <param name="jsonSerializerOptions">The JSON serializer options to use.</param>
    /// <returns>A task that represents the asynchronous read operation. The result contains the deserialized data if present.</returns>
    Task<Optional<T>> ReadSignalsAsync<T>(JsonSerializerOptions jsonSerializerOptions);
}