using StarFederation.Datastar.Services;

namespace StarFederation.Datastar.Extensions;

/// <summary>
///     Browser redirect utility methods.
/// </summary>
public static class RedirectExtensions
{
    /// <summary>
    ///     Redirects the browser to the specified URL.
    /// </summary>
    /// <param name="service">The server-sent event service.</param>
    /// <param name="url">The URL to redirect to.</param>
    /// <returns>A task that represents the asynchronous redirect operation.</returns>
    public static Task RedirectAsync(this IDatastarServerSentEventService service, string url)
    {
        return service.ExecuteScriptAsync($"window.location.href = '{url}';");
    }
}