using Starfederation.Datastar.Types;

namespace Starfederation.Datastar.Services;

/// <summary>
///     Interface for Datastar signals reader service.
/// </summary>
public interface IDatastarSignalsReaderService
{
    /// <summary>
    ///     Gets the underlying signals reader handler.
    /// </summary>
    IReadSignals Handler { get; }

    /// <summary>
    ///     Reads the signals and returns as a serialized string.
    /// </summary>
    /// <returns>A task that represents the asynchronous read operation. The result contains the serialized signals.</returns>
    Task<DatastarSignals?> ReadSignalsAsync();

    /// <summary>
    ///     Reads the signals and deserializes as a T.
    /// </summary>
    /// <typeparam name="T">The type to deserialize to.</typeparam>
    /// <returns>
    ///     A task that represents the asynchronous read and deserialize operation. The result contains the deserialized
    ///     data.
    /// </returns>
    Task<T?> ReadSignalsAsync<T>() where T : class;
}