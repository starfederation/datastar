namespace Starfederation.Datastar.DependencyInjection
{
    /// <summary>
    /// Interface for Datastar server-sent event service.
    /// </summary>
    public interface IDatastarServerSentEventService
    {
        /// <summary>
        /// Gets the underlying server-sent event handler.
        /// </summary>
        ISendServerEvent Handler { get; }

        /// <summary>
        /// Merges fragments into the DOM.
        /// </summary>
        /// <param name="fragment">The fragment to merge.</param>
        /// <returns>A task that represents the asynchronous merge operation.</returns>
        Task MergeFragmentsAsync(string fragment);

        /// <summary>
        /// Merges fragments into the DOM with the specified options.
        /// </summary>
        /// <param name="fragment">The fragment to merge.</param>
        /// <param name="options">The merge options.</param>
        /// <returns>A task that represents the asynchronous merge operation.</returns>
        Task MergeFragmentsAsync(string fragment, ServerSentEventMergeFragmentsOptions options);

        /// <summary>
        /// Removes fragments from the DOM.
        /// </summary>
        /// <param name="selector">The selector for the fragments to remove.</param>
        /// <returns>A task that represents the asynchronous remove operation.</returns>
        Task RemoveFragmentsAsync(Selector selector);

        /// <summary>
        /// Removes fragments from the DOM with the specified options.
        /// </summary>
        /// <param name="selector">The selector for the fragments to remove.</param>
        /// <param name="options">The remove options.</param>
        /// <returns>A task that represents the asynchronous remove operation.</returns>
        Task RemoveFragmentsAsync(Selector selector, ServerSentEventRemoveFragmentsOptions options);

        /// <summary>
        /// Merges signals.
        /// </summary>
        /// <param name="dataSignals">The signals to merge.</param>
        /// <returns>A task that represents the asynchronous merge operation.</returns>
        Task MergeSignalsAsync(Signals dataSignals);

        /// <summary>
        /// Merges signals with the specified options.
        /// </summary>
        /// <param name="dataSignals">The signals to merge.</param>
        /// <param name="options">The merge options.</param>
        /// <returns>A task that represents the asynchronous merge operation.</returns>
        Task MergeSignalsAsync(Signals dataSignals, ServerSentEventMergeSignalsOptions options);

        /// <summary>
        /// Removes signals.
        /// </summary>
        /// <param name="paths">The paths for the signals to remove.</param>
        /// <returns>A task that represents the asynchronous remove operation.</returns>
        Task RemoveSignalsAsync(System.Collections.Generic.IEnumerable<SignalPath> paths);

        /// <summary>
        /// Removes signals with the specified options.
        /// </summary>
        /// <param name="paths">The paths for the signals to remove.</param>
        /// <param name="options">The remove options.</param>
        /// <returns>A task that represents the asynchronous remove operation.</returns>
        Task RemoveSignalsAsync(System.Collections.Generic.IEnumerable<SignalPath> paths, ServerSentEventOptions options);

        /// <summary>
        /// Executes a script in the browser.
        /// </summary>
        /// <param name="script">The script to execute.</param>
        /// <returns>A task that represents the asynchronous execution operation.</returns>
        Task ExecuteScriptAsync(string script);

        /// <summary>
        /// Executes a script in the browser with the specified options.
        /// </summary>
        /// <param name="script">The script to execute.</param>
        /// <param name="options">The execution options.</param>
        /// <returns>A task that represents the asynchronous execution operation.</returns>
        Task ExecuteScriptAsync(string script, ServerSentEventExecuteScriptOptions options);
    }

    /// <summary>
    /// Interface for Datastar signals reader service.
    /// </summary>
    public interface IDatastarSignalsReaderService
    {
        /// <summary>
        /// Gets the underlying signals reader handler.
        /// </summary>
        IReadSignals Handler { get; }

        /// <summary>
        /// Reads the signals and returns as a serialized string.
        /// </summary>
        /// <returns>A task that represents the asynchronous read operation. The result contains the serialized signals.</returns>
        Task<Signals> ReadSignalsAsync();

        /// <summary>
        /// Reads the signals and deserializes as a T.
        /// </summary>
        /// <typeparam name="T">The type to deserialize to.</typeparam>
        /// <returns>A task that represents the asynchronous read and deserialize operation. The result contains the deserialized data.</returns>
        Task<T?> ReadSignalsAsync<T>() where T : class;
    }

    /// <summary>
    /// Implementation of IDatastarServerSentEventService.
    /// </summary>
    public class ServerSentEventService : IDatastarServerSentEventService
    {
        private readonly ISendServerEvent _handler;

        /// <summary>
        /// Initializes a new instance of the ServerSentEventService class.
        /// </summary>
        /// <param name="handler">The underlying server-sent event handler.</param>
        public ServerSentEventService(ISendServerEvent handler)
        {
            _handler = handler;
        }

        /// <summary>
        /// Gets the underlying server-sent event handler.
        /// </summary>
        public ISendServerEvent Handler => _handler;

        /// <inheritdoc/>
        public Task MergeFragmentsAsync(string fragment)
        {
            return ServerSentEventGenerator.MergeFragments(_handler, fragment);
        }

        /// <inheritdoc/>
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
            
            return ServerSentEventGenerator.MergeFragments(_handler, fragment, mergeOptions);
        }

        /// <inheritdoc/>
        public Task RemoveFragmentsAsync(Selector selector)
        {
            return ServerSentEventGenerator.RemoveFragments(_handler, selector);
        }

        /// <inheritdoc/>
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
            
            return ServerSentEventGenerator.RemoveFragments(_handler, selector, removeOptions);
        }

        /// <inheritdoc/>
        public Task MergeSignalsAsync(Signals dataSignals)
        {
            return ServerSentEventGenerator.MergeSignals(_handler, dataSignals);
        }

        /// <inheritdoc/>
        public Task MergeSignalsAsync(Signals dataSignals, ServerSentEventMergeSignalsOptions options)
        {
            var mergeOptions = new MergeSignalsOptions
            {
                OnlyIfMissing = options.OnlyIfMissing,
                EventId = string.IsNullOrEmpty(options.EventId) 
                    ? Optional<string>.None 
                    : Optional<string>.Some(options.EventId),
                Retry = options.Retry
            };
            
            return ServerSentEventGenerator.MergeSignals(_handler, dataSignals, mergeOptions);
        }

        /// <inheritdoc/>
        public Task RemoveSignalsAsync(System.Collections.Generic.IEnumerable<SignalPath> paths)
        {
            return ServerSentEventGenerator.RemoveSignals(_handler, paths);
        }

        /// <inheritdoc/>
        public Task RemoveSignalsAsync(System.Collections.Generic.IEnumerable<SignalPath> paths, ServerSentEventOptions options)
        {
            var eventOptions = new EventOptions
            {
                EventId = string.IsNullOrEmpty(options.EventId) 
                    ? Optional<string>.None 
                    : Optional<string>.Some(options.EventId),
                Retry = options.Retry
            };
            
            return ServerSentEventGenerator.RemoveSignals(_handler, paths, eventOptions);
        }

        /// <inheritdoc/>
        public Task ExecuteScriptAsync(string script)
        {
            return ServerSentEventGenerator.ExecuteScript(_handler, script);
        }

        /// <inheritdoc/>
        public Task ExecuteScriptAsync(string script, ServerSentEventExecuteScriptOptions options)
        {
            var scriptOptions = new ExecuteScriptOptions
            {
                AutoRemove = options.AutoRemove,
                Attributes = options.Attributes?.Split(' ') ?? new[] { "type", "module" },
                Retry = options.Retry
            };
            
            return ServerSentEventGenerator.ExecuteScript(_handler, script, scriptOptions);
        }
    }

    /// <summary>
    /// Implementation of IDatastarSignalsReaderService.
    /// </summary>
    public class SignalsReaderService : IDatastarSignalsReaderService
    {
        private readonly IReadSignals _handler;

        /// <summary>
        /// Initializes a new instance of the SignalsReaderService class.
        /// </summary>
        /// <param name="handler">The underlying signals reader handler.</param>
        public SignalsReaderService(IReadSignals handler)
        {
            _handler = handler;
        }

        /// <summary>
        /// Gets the underlying signals reader handler.
        /// </summary>
        public IReadSignals Handler => _handler;

        /// <inheritdoc/>
        public async Task<Signals> ReadSignalsAsync()
        {
            var signals = await _handler.ReadSignalsAsync();
            return signals.HasValue ? signals.Value : Signals.Empty;
        }

        /// <inheritdoc/>
        public async Task<T?> ReadSignalsAsync<T>() where T : class
        {
            var signals = await _handler.ReadSignalsAsync<T>();
            return signals.HasValue ? signals.Value : null;
        }
    }

    /// <summary>
    /// Options for merging fragments in server-sent events.
    /// </summary>
    public class ServerSentEventMergeFragmentsOptions
    {
        /// <summary>
        /// Gets or sets the event ID.
        /// </summary>
        public string? EventId { get; set; }

        /// <summary>
        /// Gets or sets the selector.
        /// </summary>
        public string? Selector { get; set; }

        /// <summary>
        /// Gets or sets the merge mode.
        /// </summary>
        public FragmentMergeMode MergeMode { get; set; } = Consts.DefaultFragmentMergeMode;

        /// <summary>
        /// Gets or sets the retry duration.
        /// </summary>
        public System.TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;
    }

    /// <summary>
    /// Options for merging signals in server-sent events.
    /// </summary>
    public class ServerSentEventMergeSignalsOptions
    {
        /// <summary>
        /// Gets or sets whether to only merge if missing.
        /// </summary>
        public bool OnlyIfMissing { get; set; } = Consts.DefaultMergeSignalsOnlyIfMissing;

        /// <summary>
        /// Gets or sets the event ID.
        /// </summary>
        public string? EventId { get; set; }

        /// <summary>
        /// Gets or sets the retry duration.
        /// </summary>
        public System.TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;
    }

    /// <summary>
    /// Options for removing fragments in server-sent events.
    /// </summary>
    public class ServerSentEventRemoveFragmentsOptions
    {
        /// <summary>
        /// Gets or sets the event ID.
        /// </summary>
        public string? EventId { get; set; }

        /// <summary>
        /// Gets or sets whether to use view transition.
        /// </summary>
        public bool UseViewTransition { get; set; } = Consts.DefaultFragmentsUseViewTransitions;

        /// <summary>
        /// Gets or sets the retry duration.
        /// </summary>
        public System.TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;
    }

    /// <summary>
    /// Common options for server-sent events.
    /// </summary>
    public class ServerSentEventOptions
    {
        /// <summary>
        /// Gets or sets the event ID.
        /// </summary>
        public string? EventId { get; set; }

        /// <summary>
        /// Gets or sets the retry duration.
        /// </summary>
        public System.TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;
    }

    /// <summary>
    /// Options for executing scripts in server-sent events.
    /// </summary>
    public class ServerSentEventExecuteScriptOptions
    {
        /// <summary>
        /// Gets or sets whether to auto remove the script.
        /// </summary>
        public bool AutoRemove { get; set; } = Consts.DefaultExecuteScriptAutoRemove;

        /// <summary>
        /// Gets or sets the script attributes.
        /// </summary>
        public string? Attributes { get; set; } = Consts.DefaultExecuteScriptAttributes;

        /// <summary>
        /// Gets or sets the retry duration.
        /// </summary>
        public System.TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;
    }
}
