namespace Starfederation.Datastar.Types.Options;

/// <summary>
///     Common options for events.
/// </summary>
public class EventOptions
{
    /// <summary>
    ///     Gets or sets the event ID.
    /// </summary>
    public Optional<string> EventId { get; set; } = Optional<string>.None;

    /// <summary>
    ///     Gets or sets the retry duration.
    /// </summary>
    public TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;

    /// <summary>
    ///     Gets the default options.
    /// </summary>
    public static EventOptions Defaults => new();
}