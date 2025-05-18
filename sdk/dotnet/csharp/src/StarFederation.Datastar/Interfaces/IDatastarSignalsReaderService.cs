using System.Text.Json;
using System.Text.Json.Nodes;
using StarFederation.Datastar.Types;

namespace StarFederation.Datastar.Interfaces;

/// <summary>
///     Interface for Datastar signals reader service.
/// </summary>
public interface IDatastarSignalsReaderService
{
    /// <summary>
    ///    Reads the signals from the request and returns as a serialized string.
    /// </summary>
    /// <returns> A task that represents the asynchronous read operation. The result contains the signals if present.</returns>
    Task<string?> ReadSignalsAsStringAsync();
    
    /// <summary>
    ///     Reads the signals and returns as a serialized string.
    /// </summary>
    /// <returns> A task that represents the asynchronous read operation. The result contains the signals if present.</returns>
    Task<JsonNode?> ReadSignalsAsJsonNodeAsync();

    /// <summary>
    ///     Reads the signals and deserializes as a T.
    /// </summary>
    /// <typeparam name="T">The type to deserialize to.</typeparam>
    /// <returns>
    ///     A task that represents the asynchronous read and deserialize operation. The result contains the deserialized
    ///     data.
    /// </returns>
    Task<T?> ReadSignalsAsync<T>(JsonSerializerOptions? jsonSerializerOptions = null) where T : class;
}