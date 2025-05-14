using Starfederation.Datastar.Enumerations;
using Starfederation.Datastar.Services;

namespace Starfederation.Datastar.Extensions;

/// <summary>
///     Helper class for working with BrowserConsoleAction.
/// </summary>
public static class BrowserConsoleActionExtensions
{
    /// <summary>
    ///     Executes a browser console action.
    /// </summary>
    /// <param name="service">The server-sent event service.</param>
    /// <param name="action">The action to execute.</param>
    /// <param name="message">The message for Log and Error actions.</param>
    /// <returns>A task that represents the asynchronous execution operation.</returns>
    public static Task ExecuteConsoleActionAsync(this IDatastarServerSentEventService service, BrowserConsoleAction action, string? message = null)
    {
        var script = action.ToJavaScript(message);
        return service.ExecuteScriptAsync(script);
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
}