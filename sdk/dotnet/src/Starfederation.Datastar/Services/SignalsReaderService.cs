using Starfederation.Datastar.Types;

namespace Starfederation.Datastar.Services;

/// <summary>
///     Implementation of IDatastarSignalsReaderService.
/// </summary>
public class SignalsReaderService : IDatastarSignalsReaderService
{
    /// <summary>
    ///     Initializes a new instance of the SignalsReaderService class.
    /// </summary>
    /// <param name="handler">The underlying signals reader handler.</param>
    public SignalsReaderService(ISignalsReader handler)
    {
        Handler = handler;
    }

    /// <summary>
    ///     Gets the underlying signals reader handler.
    /// </summary>
    public ISignalsReader Handler { get; }

    /// <inheritdoc />
    public async Task<DatastarSignals?> ReadSignalsAsync()
    {
        var signals = await Handler.ReadSignalsAsync();
        return signals.HasValue ? signals.Value : DatastarSignals.Empty;
    }

    /// <inheritdoc />
    public async Task<T?> ReadSignalsAsync<T>() where T : class
    {
        var signals = await Handler.ReadSignalsAsync<T>();
        return signals.HasValue ? signals.Value : null;
    }
}