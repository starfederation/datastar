using Starfederation.Datastar.Enumerations;
using Starfederation.Datastar.Types;
using Starfederation.Datastar.Types.Options;

namespace Starfederation.Datastar;

/// <summary>
///     Converts requests into SSEs, serializes, and sends to ISendServerEvent handlers
/// </summary>
public static class ServerSentEventGenerator
{
    /// <summary>
    ///     Sends a server-sent event.
    /// </summary>
    /// <param name="sseHandler">The handler to send the event to.</param>
    /// <param name="sse">The server-sent event to send.</param>
    /// <returns>A task that represents the asynchronous send operation.</returns>
    public static Task Send(IServerEventSender sseHandler, ServerSentEvent sse)
    {
        return sseHandler.SendServerEventAsync(sse);
    }

    /// <summary>
    ///     Sends a merge fragments event.
    /// </summary>
    /// <param name="sseHandler">The handler to send the event to.</param>
    /// <param name="fragments">The fragments to merge.</param>
    /// <param name="options">The options for the merge fragments event.</param>
    /// <returns>A task that represents the asynchronous send operation.</returns>
    public static Task MergeFragments(IServerEventSender sseHandler, string fragments, MergeFragmentsOptions? options = null)
    {
        options ??= MergeFragmentsOptions.Defaults;

        var dataLines = new List<string>();

        if (options.Selector != null)
        {
            dataLines.Add($"{Consts.DatastarDatalineSelector} {options.Selector.Value}");
        }

        var mergeMode = Consts.FragmentMergeModeHelper.ToString(options.MergeMode);
        if (mergeMode != Consts.FragmentMergeModeHelper.ToString(Consts.DefaultFragmentMergeMode))
        {
            dataLines.Add($"{Consts.DatastarDatalineMergeMode} {mergeMode}");
        }

        dataLines.Add($"{Consts.DatastarDatalineFragments} {fragments}");

        var sse = new ServerSentEvent
        {
            EventType = EventType.MergeFragments,
            Id = options.Selector,
            Retry = options.Retry,
            DataLines = dataLines.ToArray()
        };

        return Send(sseHandler, sse);
    }

    /// <summary>
    ///     Sends a remove fragments event.
    /// </summary>
    /// <param name="sseHandler">The handler to send the event to.</param>
    /// <param name="selector">The selector for the fragments to remove.</param>
    /// <param name="options">The options for the remove fragments event.</param>
    /// <returns>A task that represents the asynchronous send operation.</returns>
    public static Task RemoveFragments(IServerEventSender sseHandler, Selector selector, RemoveFragmentsOptions? options = null)
    {
        options ??= RemoveFragmentsOptions.Defaults;

        var dataLines = new List<string>
        {
            $"{Consts.DatastarDatalineSelector} {selector.Value}"
        };

        if (options.UseViewTransition != Consts.DefaultFragmentsUseViewTransitions)
        {
            dataLines.Add($"{Consts.DatastarDatalineUseViewTransition} {options.UseViewTransition.ToString().ToLower()}");
        }

        var sse = new ServerSentEvent
        {
            EventType = EventType.RemoveFragments,
            Id = options.EventId,
            Retry = options.Retry,
            DataLines = dataLines.ToArray()
        };

        return Send(sseHandler, sse);
    }

    /// <summary>
    ///     Sends a merge signals event.
    /// </summary>
    /// <param name="sseHandler">The handler to send the event to.</param>
    /// <param name="mergeDatastarSignals">The signals to merge.</param>
    /// <param name="options">The options for the merge signals event.</param>
    /// <returns>A task that represents the asynchronous send operation.</returns>
    public static Task MergeSignals(IServerEventSender sseHandler, DatastarSignals mergeDatastarSignals, MergeSignalsOptions? options = null)
    {
        options ??= MergeSignalsOptions.Defaults;

        var dataLines = new List<string>
        {
            $"{Consts.DatastarDatalineSignals} {mergeDatastarSignals.Value}"
        };

        if (options.OnlyIfMissing != Consts.DefaultMergeSignalsOnlyIfMissing)
        {
            dataLines.Add($"{Consts.DatastarDatalineOnlyIfMissing} {options.OnlyIfMissing.ToString().ToLower()}");
        }

        var sse = new ServerSentEvent
        {
            EventType = EventType.MergeSignals,
            Id = options.EventId,
            Retry = options.Retry,
            DataLines = dataLines.ToArray()
        };

        return Send(sseHandler, sse);
    }

    /// <summary>
    ///     Sends a remove signals event.
    /// </summary>
    /// <param name="sseHandler">The handler to send the event to.</param>
    /// <param name="paths">The signal paths to remove.</param>
    /// <param name="options">The options for the remove signals event.</param>
    /// <returns>A task that represents the asynchronous send operation.</returns>
    public static Task RemoveSignals(IServerEventSender sseHandler, IEnumerable<SignalPath> paths, EventOptions? options = null)
    {
        options ??= EventOptions.Defaults;

        var pathsString = string.Join(" ", paths.Select(p => p.Value));

        var sse = new ServerSentEvent
        {
            EventType = EventType.RemoveSignals,
            Id = options.EventId,
            Retry = options.Retry,
            DataLines = [$"{Consts.DatastarDatalinePaths} {pathsString}"]
        };

        return Send(sseHandler, sse);
    }

    /// <summary>
    ///     Sends an execute script event.
    /// </summary>
    /// <param name="sseHandler">The handler to send the event to.</param>
    /// <param name="script">The script to execute.</param>
    /// <param name="options">The options for the execute script event.</param>
    /// <returns>A task that represents the asynchronous send operation.</returns>
    public static Task ExecuteScript(IServerEventSender sseHandler, string script, ExecuteScriptOptions? options = null)
    {
        options ??= ExecuteScriptOptions.Defaults;

        var dataLines = new List<string>
        {
            $"{Consts.DatastarDatalineScript} {script}"
        };

        var attributes = string.Join(" ", options.Attributes);
        if (attributes != Consts.DefaultExecuteScriptAttributes)
        {
            dataLines.Add($"{Consts.DatastarDatalineAttributes} {attributes}");
        }

        if (options.AutoRemove != Consts.DefaultExecuteScriptAutoRemove)
        {
            dataLines.Add($"{Consts.DatastarDatalineAutoRemove} {options.AutoRemove.ToString().ToLower()}");
        }

        var sse = new ServerSentEvent
        {
            EventType = EventType.ExecuteScript,
            Id = script,
            Retry = options.Retry,
            DataLines = dataLines.ToArray()
        };

        return Send(sseHandler, sse);
    }
}