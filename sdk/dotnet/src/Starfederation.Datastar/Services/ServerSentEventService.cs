using Starfederation.Datastar.Types;
using Starfederation.Datastar.Types.Options;

namespace Starfederation.Datastar.Services;

/// <summary>
///     Implementation of IDatastarServerSentEventService.
/// </summary>
public class ServerSentEventService : SignalsReaderService, IDatastarServerSentEventService
{
    /// <summary>
    ///     Initializes a new instance of the ServerSentEventService class.
    /// </summary>
    /// <param name="handler">The underlying server-sent event handler.</param>
    public ServerSentEventService(IServerEventSender handler, ISignalsReader signalsReaderReader) : base(signalsReaderReader)
    {
        ServerEventSenderSender = handler;
    }

    /// <summary>
    ///     Gets the underlying server-sent event handler.
    /// </summary>
    public IServerEventSender ServerEventSenderSender { get; }

    /// <inheritdoc />
    public Task MergeFragmentsAsync(string fragment)
    {
        return ServerSentEventGenerator.MergeFragments(ServerEventSenderSender, fragment);
    }

    /// <inheritdoc />
    public Task MergeFragmentsAsync(string fragment, ServerSentEventMergeFragmentsOptions options)
    {
        var mergeOptions = new MergeFragmentsOptions
        {
            Selector = string.IsNullOrEmpty(options.Selector)
                ? Optional<Selector>.None
                : Optional<Selector>.Some(new Selector(options.Selector)),
            MergeMode = options.MergeMode,
            Retry = options.Retry
        };

        return ServerSentEventGenerator.MergeFragments(ServerEventSenderSender, fragment, mergeOptions);
    }

    /// <inheritdoc />
    public Task RemoveFragmentsAsync(Selector selector)
    {
        return ServerSentEventGenerator.RemoveFragments(ServerEventSenderSender, selector);
    }

    /// <inheritdoc />
    public Task RemoveFragmentsAsync(Selector selector, ServerSentEventRemoveFragmentsOptions options)
    {
        var removeOptions = new RemoveFragmentsOptions
        {
            UseViewTransition = options.UseViewTransition,
            EventId = string.IsNullOrEmpty(options.EventId)
                ? Optional<string>.None
                : Optional<string>.Some(options.EventId),
            Retry = options.Retry
        };

        return ServerSentEventGenerator.RemoveFragments(ServerEventSenderSender, selector, removeOptions);
    }

    /// <inheritdoc />
    public Task MergeSignalsAsync(DatastarSignals dataDatastarSignals)
    {
        return ServerSentEventGenerator.MergeSignals(ServerEventSenderSender, dataDatastarSignals);
    }

    /// <inheritdoc />
    public Task MergeSignalsAsync(DatastarSignals dataDatastarSignals, ServerSentEventMergeSignalsOptions options)
    {
        var mergeOptions = new MergeSignalsOptions
        {
            OnlyIfMissing = options.OnlyIfMissing,
            EventId = string.IsNullOrEmpty(options.EventId)
                ? Optional<string>.None
                : Optional<string>.Some(options.EventId),
            Retry = options.Retry
        };

        return ServerSentEventGenerator.MergeSignals(ServerEventSenderSender, dataDatastarSignals, mergeOptions);
    }

    /// <inheritdoc />
    public Task RemoveSignalsAsync(IEnumerable<SignalPath> paths)
    {
        return ServerSentEventGenerator.RemoveSignals(ServerEventSenderSender, paths);
    }

    /// <inheritdoc />
    public Task RemoveSignalsAsync(IEnumerable<SignalPath> paths, ServerSentEventOptions options)
    {
        var eventOptions = new EventOptions
        {
            EventId = string.IsNullOrEmpty(options.EventId)
                ? Optional<string>.None
                : Optional<string>.Some(options.EventId),
            Retry = options.Retry
        };

        return ServerSentEventGenerator.RemoveSignals(ServerEventSenderSender, paths, eventOptions);
    }

    /// <inheritdoc />
    public Task ExecuteScriptAsync(string script)
    {
        return ServerSentEventGenerator.ExecuteScript(ServerEventSenderSender, script);
    }

    /// <inheritdoc />
    public Task ExecuteScriptAsync(string script, ServerSentEventExecuteScriptOptions options)
    {
        var scriptOptions = new ExecuteScriptOptions
        {
            AutoRemove = options.AutoRemove,
            Attributes = options.Attributes?.Split(' ') ?? ["type", "module"],
            Retry = options.Retry
        };

        return ServerSentEventGenerator.ExecuteScript(ServerEventSenderSender, script, scriptOptions);
    }
}