namespace Starfederation.Datastar.DependencyInjection
{
    /// <summary>
    /// Represents the action to perform in the browser console.
    /// </summary>
    public enum BrowserConsoleAction
    {
        /// <summary>
        /// Clears the browser console.
        /// </summary>
        Clear,
        
        /// <summary>
        /// Logs a message to the browser console.
        /// </summary>
        Log,
        
        /// <summary>
        /// Logs an error to the browser console.
        /// </summary>
        Error
    }

    /// <summary>
    /// Helper class for working with BrowserConsoleAction.
    /// </summary>
    public static class BrowserConsoleActionExtensions
    {
        private static string EscapeMessage(string str)
        {
            return str.Replace("'", @"\'");
        }

        /// <summary>
        /// Converts a BrowserConsoleAction to its JavaScript representation.
        /// </summary>
        /// <param name="action">The action.</param>
        /// <param name="message">The message for Log and Error actions.</param>
        /// <returns>The JavaScript representation of the action.</returns>
        public static string ToJavaScript(this BrowserConsoleAction action, string? message = null)
        {
            return action switch
            {
                BrowserConsoleAction.Clear => "console.clear()",
                BrowserConsoleAction.Log => $"console.log('{EscapeMessage(message ?? "")}')",
                BrowserConsoleAction.Error => $"console.error('{EscapeMessage(message ?? "")}')",
                _ => string.Empty
            };
        }
    }

    /// <summary>
    /// Browser console utility methods.
    /// </summary>
    public static class BrowserConsole
    {
        /// <summary>
        /// Executes a browser console action.
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
    }

    /// <summary>
    /// Browser redirect utility methods.
    /// </summary>
    public static class Redirect
    {
        /// <summary>
        /// Redirects the browser to the specified URL.
        /// </summary>
        /// <param name="service">The server-sent event service.</param>
        /// <param name="url">The URL to redirect to.</param>
        /// <returns>A task that represents the asynchronous redirect operation.</returns>
        public static Task RedirectAsync(this IDatastarServerSentEventService service, string url)
        {
            return service.ExecuteScriptAsync($"window.location.href = '{url}';");
        }
    }

    /// <summary>
    /// Extension methods for IDatastarServerSentEventService.
    /// </summary>
    public static class ServerSentEventScriptExtensions
    {
        /// <summary>
        /// Redirects the browser to the specified URL.
        /// </summary>
        /// <param name="service">The server-sent event service.</param>
        /// <param name="url">The URL to redirect to.</param>
        /// <returns>A task that represents the asynchronous redirect operation.</returns>
        public static Task RedirectAsync(this IDatastarServerSentEventService service, string url)
        {
            return Redirect.RedirectAsync(service, url);
        }

        /// <summary>
        /// Executes a browser console action.
        /// </summary>
        /// <param name="service">The server-sent event service.</param>
        /// <param name="action">The action to execute.</param>
        /// <param name="message">The message for Log and Error actions.</param>
        /// <returns>A task that represents the asynchronous execution operation.</returns>
        public static Task BrowserConsoleActionAsync(this IDatastarServerSentEventService service, BrowserConsoleAction action, string? message = null)
        {
            return BrowserConsole.ExecuteConsoleActionAsync(service, action, message);
        }
    }
}
