using System.Text.Json;
using System.Text.Json.Nodes;
using System.Text.RegularExpressions;

namespace Starfederation.Datastar
{
    /// <summary>
    /// Represents a Server Sent Event.
    /// </summary>
    public class ServerSentEvent
    {
        /// <summary>
        /// Gets the type of the event.
        /// </summary>
        public EventType EventType { get; init; }

        /// <summary>
        /// Gets the ID of the event.
        /// </summary>
        public Optional<string> Id { get; init; }

        /// <summary>
        /// Gets the retry duration for the event.
        /// </summary>
        public TimeSpan Retry { get; init; } = Consts.DefaultSseRetryDuration;

        /// <summary>
        /// Gets the data lines for the event.
        /// </summary>
        public string[] DataLines { get; init; } = Array.Empty<string>();

        /// <summary>
        /// Serializes the ServerSentEvent to a string.
        /// </summary>
        /// <returns>The serialized ServerSentEvent.</returns>
        public string Serialize()
        {
            var lines = new List<string>();

            lines.Add($"event: {Consts.EventType.ToString(EventType)}");

            if (Id.HasValue)
            {
                lines.Add($"id: {Id.Value}");
            }

            if (Retry != TimeSpan.Zero)
            {
                lines.Add($"retry: {(int)Retry.TotalMilliseconds}");
            }

            foreach (var dataLine in DataLines)
            {
                lines.Add($"data: {dataLine}");
            }

            // Add an extra newline at the end to complete the event
            lines.Add("");
            
            return string.Join("\n", lines);
        }
    }

    /// <summary>
    /// Represents signals read to and from Datastar on the front end.
    /// </summary>
    public class Signals
    {
        private readonly string _signalsString;

        /// <summary>
        /// Initializes a new instance of the Signals class.
        /// </summary>
        /// <param name="signalsString">The signals string.</param>
        public Signals(string signalsString)
        {
            _signalsString = signalsString ?? "{ }";
        }

        /// <summary>
        /// Gets the string value of the signals.
        /// </summary>
        /// <returns>The string value of the signals.</returns>
        public string Value => _signalsString;

        /// <summary>
        /// Returns the string representation of the signals.
        /// </summary>
        /// <returns>The string representation of the signals.</returns>
        public override string ToString() => _signalsString;

        /// <summary>
        /// Creates a new signals instance.
        /// </summary>
        /// <param name="signalsString">The signals string.</param>
        /// <returns>A new Signals instance.</returns>
        public static Signals Create(string signalsString) => new Signals(signalsString);

        /// <summary>
        /// Attempts to create a new signals instance.
        /// </summary>
        /// <param name="signalsString">The signals string.</param>
        /// <returns>An Optional containing the Signals if valid, None otherwise.</returns>
        public static Optional<Signals> TryCreate(string signalsString)
        {
            try
            {
                // Validate JSON
                var _ = JsonNode.Parse(signalsString);
                return Optional<Signals>.Some(new Signals(signalsString));
            }
            catch
            {
                return Optional<Signals>.None;
            }
        }

        /// <summary>
        /// Gets an empty signals instance.
        /// </summary>
        public static Signals Empty => new Signals("{ }");
    }

    /// <summary>
    /// Represents a dotted path into Signals to access a key/value pair.
    /// </summary>
    public class SignalPath
    {
        private readonly string _path;

        /// <summary>
        /// Initializes a new instance of the SignalPath class.
        /// </summary>
        /// <param name="path">The signal path.</param>
        public SignalPath(string path)
        {
            _path = path ?? string.Empty;
        }

        /// <summary>
        /// Gets the string value of the signal path.
        /// </summary>
        /// <returns>The string value of the signal path.</returns>
        public string Value => _path;

        /// <summary>
        /// Returns the string representation of the signal path.
        /// </summary>
        /// <returns>The string representation of the signal path.</returns>
        public override string ToString() => _path;

        /// <summary>
        /// Gets the kebab case value of the signal path.
        /// </summary>
        /// <returns>The kebab case value of the signal path.</returns>
        public string KebabValue => Utils.Strings.ToKebabCase(_path);

        /// <summary>
        /// Checks if a key is valid for a signal path.
        /// </summary>
        /// <param name="key">The key to check.</param>
        /// <returns>True if the key is valid, false otherwise.</returns>
        public static bool IsValidKey(string key)
        {
            // Implement validation rules here
            return !string.IsNullOrEmpty(key);
        }
    }

    /// <summary>
    /// Represents an HTML selector name.
    /// </summary>
    public class Selector
    {
        private static readonly Regex SelectorRegex = new Regex(@"[#.][-_]?[_a-zA-Z]+(?:\w|\\.)*|(?<=\s+|^)(?:\w+|\*)|\[[^\s""'=<>`]+?(?<![~|^$*])([~|^$*]?=(?:['""].*['""]|[^\s""'=<>`]+))?\]|:[\w-]+(?:\(.*\))?", 
            RegexOptions.Compiled);
            
        private readonly string _selector;

        /// <summary>
        /// Initializes a new instance of the Selector class.
        /// </summary>
        /// <param name="selector">The selector string.</param>
        public Selector(string selector)
        {
            _selector = selector ?? string.Empty;
        }

        /// <summary>
        /// Gets the string value of the selector.
        /// </summary>
        /// <returns>The string value of the selector.</returns>
        public string Value => _selector;

        /// <summary>
        /// Returns the string representation of the selector.
        /// </summary>
        /// <returns>The string representation of the selector.</returns>
        public override string ToString() => _selector;

        /// <summary>
        /// Creates a new selector instance.
        /// </summary>
        /// <param name="selector">The selector string.</param>
        /// <returns>A new Selector instance.</returns>
        public static Selector Create(string selector) => new Selector(selector);
    }

    /// <summary>
    /// Options for merging fragments.
    /// </summary>
    public class MergeFragmentsOptions
    {
        /// <summary>
        /// Gets or sets the selector.
        /// </summary>
        public Optional<Selector> Selector { get; set; } = Optional<Selector>.None;

        /// <summary>
        /// Gets or sets the merge mode.
        /// </summary>
        public FragmentMergeMode MergeMode { get; set; } = Consts.DefaultFragmentMergeMode;

        /// <summary>
        /// Gets or sets the retry duration.
        /// </summary>
        public TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;

        /// <summary>
        /// Gets the default options.
        /// </summary>
        public static MergeFragmentsOptions Defaults => new MergeFragmentsOptions();
    }

    /// <summary>
    /// Options for merging signals.
    /// </summary>
    public class MergeSignalsOptions
    {
        /// <summary>
        /// Gets or sets whether to only merge if missing.
        /// </summary>
        public bool OnlyIfMissing { get; set; } = Consts.DefaultMergeSignalsOnlyIfMissing;

        /// <summary>
        /// Gets or sets the event ID.
        /// </summary>
        public Optional<string> EventId { get; set; } = Optional<string>.None;

        /// <summary>
        /// Gets or sets the retry duration.
        /// </summary>
        public TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;

        /// <summary>
        /// Gets the default options.
        /// </summary>
        public static MergeSignalsOptions Defaults => new MergeSignalsOptions();
    }

    /// <summary>
    /// Options for removing fragments.
    /// </summary>
    public class RemoveFragmentsOptions
    {
        /// <summary>
        /// Gets or sets whether to use view transition.
        /// </summary>
        public bool UseViewTransition { get; set; } = Consts.DefaultFragmentsUseViewTransitions;

        /// <summary>
        /// Gets or sets the event ID.
        /// </summary>
        public Optional<string> EventId { get; set; } = Optional<string>.None;

        /// <summary>
        /// Gets or sets the retry duration.
        /// </summary>
        public TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;

        /// <summary>
        /// Gets the default options.
        /// </summary>
        public static RemoveFragmentsOptions Defaults => new RemoveFragmentsOptions();
    }

    /// <summary>
    /// Options for executing scripts.
    /// </summary>
    public class ExecuteScriptOptions
    {
        /// <summary>
        /// Gets or sets whether to auto remove the script.
        /// </summary>
        public bool AutoRemove { get; set; } = Consts.DefaultExecuteScriptAutoRemove;

        /// <summary>
        /// Gets or sets the script attributes.
        /// </summary>
        public string[] Attributes { get; set; } = Consts.DefaultExecuteScriptAttributes.Split(' ', StringSplitOptions.RemoveEmptyEntries);

        /// <summary>
        /// Gets or sets the retry duration.
        /// </summary>
        public TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;

        /// <summary>
        /// Gets the default options.
        /// </summary>
        public static ExecuteScriptOptions Defaults => new ExecuteScriptOptions();
    }

    /// <summary>
    /// Common options for events.
    /// </summary>
    public class EventOptions
    {
        /// <summary>
        /// Gets or sets the event ID.
        /// </summary>
        public Optional<string> EventId { get; set; } = Optional<string>.None;

        /// <summary>
        /// Gets or sets the retry duration.
        /// </summary>
        public TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;

        /// <summary>
        /// Gets the default options.
        /// </summary>
        public static EventOptions Defaults => new EventOptions();
    }

    /// <summary>
    /// Interface for reading signals from a request.
    /// </summary>
    public interface IReadSignals
    {
        /// <summary>
        /// Reads the signals from the request.
        /// </summary>
        /// <returns>A task that represents the asynchronous read operation. The result contains the signals if present.</returns>
        Task<Optional<Signals>> ReadSignalsAsync();

        /// <summary>
        /// Reads the signals from the request and deserializes them to the specified type.
        /// </summary>
        /// <typeparam name="T">The type to deserialize to.</typeparam>
        /// <returns>A task that represents the asynchronous read operation. The result contains the deserialized data if present.</returns>
        Task<Optional<T>> ReadSignalsAsync<T>();

        /// <summary>
        /// Reads the signals from the request and deserializes them to the specified type using the provided options.
        /// </summary>
        /// <typeparam name="T">The type to deserialize to.</typeparam>
        /// <param name="jsonSerializerOptions">The JSON serializer options to use.</param>
        /// <returns>A task that represents the asynchronous read operation. The result contains the deserialized data if present.</returns>
        Task<Optional<T>> ReadSignalsAsync<T>(JsonSerializerOptions jsonSerializerOptions);
    }

    /// <summary>
    /// Interface for sending server-sent events.
    /// </summary>
    public interface ISendServerEvent
    {
        /// <summary>
        /// Sends a server-sent event.
        /// </summary>
        /// <param name="serverSentEvent">The server-sent event to send.</param>
        /// <returns>A task that represents the asynchronous send operation.</returns>
        Task SendServerEventAsync(ServerSentEvent serverSentEvent);
    }
}
