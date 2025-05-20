using System.Text.Json;
using Microsoft.AspNetCore.Http;
using StarFederation.Datastar.Enumerations;
using StarFederation.Datastar.Types;
using StarFederation.Datastar.Types.Options;

namespace StarFederation.Datastar;

public static class ServerSentEventGenerator
{
    #region Basic SSE

    /// <summary>
    /// Starts a server-sent event (SSE) response.
    /// </summary>
    /// <param name="httpResponse"></param>
    /// <returns></returns>
    public static Task StartSseResponse(this HttpResponse httpResponse)
    {
        httpResponse.Headers.ContentType = DatastarConstants.DatastarSseContentType;
        httpResponse.Headers.CacheControl = "no-cache";
        
        if (httpResponse.HttpContext.Request.Protocol == "HTTP/1.1")
        {
            httpResponse.Headers.Connection = "keep-alive";
        }

        // Write the initial response headers
        return httpResponse.Body.FlushAsync();
    }

    /// <summary>
    /// Sends a server-sent event to the client.
    /// </summary>
    /// <param name="httpResponse"></param>
    /// <param name="serverSentEvent"></param>
    internal static async Task SendEvent(this HttpResponse httpResponse, ServerSentEvent serverSentEvent)
    {
        // Ensure the response is started
        if (!httpResponse.HasStarted)
        {
            await httpResponse.StartSseResponse();
        }

        var serializedEvent = serverSentEvent.Serialize();
        var eventString = serverSentEvent.ToString();

        await httpResponse.Body.WriteAsync(serializedEvent);
        //await httpResponse.Body.FlushAsync();
    }

    /// <summary>
    /// Ends the server-sent event (SSE) response.
    /// </summary>
    /// <param name="httpResponse"></param>
    /// <returns></returns>
    public static Task EndSseResponse(this HttpResponse httpResponse)
    {
        // Ensure the response is started
        if (!httpResponse.HasStarted)
        {
            return Task.CompletedTask;
        }

        httpResponse.Headers.Remove("Content-Type");
        httpResponse.Headers.Remove("Cache-Control");
        httpResponse.Headers.Remove("Connection");

        return httpResponse.Body.FlushAsync();
    }

    #endregion

    #region Datastar Fragments

    public static Task MergeFragments(this HttpResponse httpResponse, string fragments, ServerSentEventMergeFragmentsOptions? options = null)
    {
        options ??= new ServerSentEventMergeFragmentsOptions();

        var serverSideEvent = new ServerSentEvent
        {
            EventType = EventType.MergeFragments,
            Id = options.Selector,
            Retry = options.Retry
        };

        if (options.Selector != null)
        {
            serverSideEvent.AddDataLine($"{DatastarConstants.DatastarDatalineSelector} {options.Selector.Value}");
        }

        var mergeMode = options.MergeMode.ToStringFast(true);
        // If the merge mode is the default, we don't need to send it
        if (mergeMode != DatastarConstants.DefaultFragmentMergeMode.ToStringFast(true))
        {
            serverSideEvent.AddDataLine($"{DatastarConstants.DatastarDatalineMergeMode} {mergeMode}");
        }

        serverSideEvent.AddDataLine($"{DatastarConstants.DatastarDatalineFragments} {fragments}");

        return httpResponse.SendEvent(serverSideEvent);
    }

    public static Task RemoveFragments(this HttpResponse httpResponse, DatastarSelector datastarSelector, ServerSentEventRemoveFragmentsOptions? options = null)
    {
        options ??= new ServerSentEventRemoveFragmentsOptions();

        var serverSideEvent = new ServerSentEvent
        {
            EventType = EventType.RemoveFragments,
            Id = datastarSelector.ToString(),
            Retry = options.Retry
        };

        serverSideEvent.AddDataLine($"{DatastarConstants.DatastarDatalineSelector} {datastarSelector.Value}");

        if (options.UseViewTransition != DatastarConstants.DefaultFragmentsUseViewTransitions)
        {
            serverSideEvent.AddDataLine($"{DatastarConstants.DatastarDatalineUseViewTransition} {options.UseViewTransition.ToString().ToLower()}");
        }

        return httpResponse.SendEvent(serverSideEvent);
    }

    #endregion

    #region Datastar Signals

    public static Task MergeSignals<T>(this HttpResponse httpResponse, T? dataDatastarSignals, ServerSentEventMergeSignalsOptions? options = null)
    {
        if (dataDatastarSignals == null)
        {
            return Task.CompletedTask;
        }

        options ??= new ServerSentEventMergeSignalsOptions();

        var serverSideEvent = new ServerSentEvent
        {
            EventType = EventType.MergeSignals,
            Id = string.IsNullOrEmpty(options.EventId) ? null : options.EventId,
            Retry = options.Retry
        };

        if (dataDatastarSignals is string stringSignals)
        {
            serverSideEvent.AddDataLine($"{DatastarConstants.DatastarDatalineSignals} {stringSignals}");
        }
        else
        {
            var jsonValue = JsonSerializer.Serialize(dataDatastarSignals);
            serverSideEvent.AddDataLine($"{DatastarConstants.DatastarDatalineSignals} {jsonValue}");
        }

        if (options.OnlyIfMissing != DatastarConstants.DefaultMergeSignalsOnlyIfMissing)
        {
            serverSideEvent.AddDataLine($"{DatastarConstants.DatastarDatalineOnlyIfMissing} {options.OnlyIfMissing.ToString().ToLower()}");
        }

        return httpResponse.SendEvent(serverSideEvent);
    }

    public static Task RemoveSignals(this HttpResponse httpResponse, IEnumerable<DatastarSignalPath> paths, ServerSentEventOptions? options = null)
    {
        options ??= new ServerSentEventOptions();

        var pathsString = string.Join(" ", paths.Select(p => p.Value));

        var serverSideEvent = new ServerSentEvent
        {
            EventType = EventType.RemoveSignals,
            Id = options.EventId,
            Retry = options.Retry
        };

        serverSideEvent.AddDataLine($"{DatastarConstants.DatastarDatalinePaths} {pathsString}");

        return httpResponse.SendEvent(serverSideEvent);
    }

    #endregion

    #region Datastar Script

    public static async Task<string?> ExecuteJavascriptAsync(this HttpResponse httpResponse, string script, ServerSentEventExecuteScriptOptions? options = null)
    {
        options ??= new ServerSentEventExecuteScriptOptions();

        var serverSideEvent = new ServerSentEvent
        {
            EventType = EventType.ExecuteScript,
            Id = script,
            Retry = options.Retry,
        };

        var attributes = string.Join(" ", options.Attributes);
        if (attributes != DatastarConstants.DefaultExecuteScriptAttributes)
        {
            serverSideEvent.AddDataLine($"{DatastarConstants.DatastarDatalineAttributes} {attributes}");
        }

        if (options.AutoRemove != DatastarConstants.DefaultExecuteScriptAutoRemove)
        {
            serverSideEvent.AddDataLine($"{DatastarConstants.DatastarDatalineAutoRemove} {options.AutoRemove.ToString().ToLower()}");
        }

        await httpResponse.SendEvent(serverSideEvent);

        //TODO: Handle the result of the script execution
        return "//TODO return the result of the script execution";
    }

    /// <summary>
    ///     Redirects the browser to the specified URL.
    /// </summary>
    /// <param name="httpResponse"></param>
    /// <param name="url">The URL to redirect to.</param>
    /// <returns>A task that represents the asynchronous redirect operation.</returns>
    public static Task RedirectAsync(this HttpResponse httpResponse, string url)
    {
        var script = $"window.location.href = '{url}';";
        return httpResponse.ExecuteJavascriptAsync(script);
    }

    /// <summary>
    ///     Executes a browser console action.
    /// </summary>
    /// <param name="httpResponse">The server-sent event service.</param>
    /// <param name="action">The action to execute.</param>
    /// <param name="message">The message for Log and Error actions.</param>
    /// <returns>A task that represents the asynchronous execution operation.</returns>
    public static Task ExecuteConsoleActionAsync(this HttpResponse httpResponse, BrowserConsoleAction action, string? message = null)
    {
        var script = action.ToJavaScript(message);
        return httpResponse.ExecuteJavascriptAsync(script);
    }

    private static string EscapeMessage(string str)
    {
        return str.Replace("'", @"\'");
    }

    /// <summary>
    ///     Converts a BrowserConsoleAction to its JavaScript representation.
    /// </summary>
    /// <param name="action">The action.</param>
    /// <param name="message">The message for Log and Error actions.</param>
    /// <returns>The JavaScript representation of the action.</returns>
    public static string ToJavaScript(this BrowserConsoleAction action, string? message = null)
    {
        return action switch
        {
            BrowserConsoleAction.Clear => "console.clear()",
            BrowserConsoleAction.Log   => $"console.log('{EscapeMessage(message   ?? "")}')",
            BrowserConsoleAction.Error => $"console.error('{EscapeMessage(message ?? "")}')",
            _                          => string.Empty
        };
    }

    #endregion
}