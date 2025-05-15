using StarFederation.Datastar.Enumerations;
using StarFederation.Datastar.Services;

namespace StarFederation.Datastar.Extensions;

/// <summary>
///     Extension methods for IDatastarServerSentEventService.
/// </summary>
public static class ServerSentEventScriptExtensions
{
    /// <summary>
    ///     Redirects the browser to the specified URL.
    /// </summary>
    /// <param name="service">The server-sent event service.</param>
    /// <param name="url">The URL to redirect to.</param>
    /// <returns>A task that represents the asynchronous redirect operation.</returns>
    public static Task RedirectAsync(this IDatastarServerSentEventService service, string url)
    {
        return RedirectExtensions.RedirectAsync(service, url);
    }

    /// <summary>
    ///     Executes a browser console action.
    /// </summary>
    /// <param name="service">The server-sent event service.</param>
    /// <param name="action">The action to execute.</param>
    /// <param name="message">The message for Log and Error actions.</param>
    /// <returns>A task that represents the asynchronous execution operation.</returns>
    public static Task BrowserConsoleActionAsync(this IDatastarServerSentEventService service, BrowserConsoleAction action, string? message = null)
    {
        return service.ExecuteConsoleActionAsync(action, message);
    }
}