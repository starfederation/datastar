namespace Starfederation.Datastar.Scripts
{
    /// <summary>
    /// Utility class for browser redirection.
    /// </summary>
    public static class RedirectUtility
    {
        /// <summary>
        /// Redirects the browser to the specified URL through a server-sent event.
        /// </summary>
        /// <param name="handler">The server-sent event handler.</param>
        /// <param name="url">The URL to redirect to.</param>
        /// <param name="options">The event options.</param>
        /// <returns>A task that represents the asynchronous redirect operation.</returns>
        public static Task RedirectAsync(ISendServerEvent handler, string url, EventOptions? options = null)
        {
            options ??= EventOptions.Defaults;
            
            var scriptOptions = new ExecuteScriptOptions
            {
                AutoRemove = ExecuteScriptOptions.Defaults.AutoRemove,
                Attributes = ExecuteScriptOptions.Defaults.Attributes,
                Retry = options.Retry
            };
            
            return ServerSentEventGenerator.ExecuteScript(handler, $"window.location.href = '{url}';", scriptOptions);
        }
    }
}
