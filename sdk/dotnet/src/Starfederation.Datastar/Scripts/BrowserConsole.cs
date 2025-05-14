using Starfederation.Datastar.DependencyInjection;

namespace Starfederation.Datastar.Scripts
{
    /// <summary>
    /// Utility class for browser console operations.
    /// </summary>
    public static class BrowserConsoleUtility
    {
        /// <summary>
        /// Executes a browser console action through a server-sent event.
        /// </summary>
        /// <param name="handler">The server-sent event handler.</param>
        /// <param name="action">The browser console action to execute.</param>
        /// <param name="message">The message for Log and Error actions.</param>
        /// <param name="options">The event options.</param>
        /// <returns>A task that represents the asynchronous execution operation.</returns>
        public static Task BrowserConsoleActionAsync(ISendServerEvent handler, BrowserConsoleAction action, string? message = null, EventOptions? options = null)
        {
            options ??= EventOptions.Defaults;
            
            var scriptOptions = new ExecuteScriptOptions
            {
                AutoRemove = ExecuteScriptOptions.Defaults.AutoRemove,
                Attributes = ExecuteScriptOptions.Defaults.Attributes,
                Retry = options.Retry
            };
            
            return ServerSentEventGenerator.ExecuteScript(handler, action.ToJavaScript(message), scriptOptions);
        }
    }
}
