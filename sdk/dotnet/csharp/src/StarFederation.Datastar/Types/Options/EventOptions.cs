using StarFederation.Datastar.Interfaces;

namespace StarFederation.Datastar.Types.Options;

/// <summary>
///     Common options for events.
/// </summary>
public class EventOptions : IDatastarOptions
{
    /// <summary>
    ///     Gets or sets the event ID.
    /// </summary>
    public string EventId { get; set; } = string.Empty;

    /// <summary>
    ///     Gets or sets the retry duration.
    /// </summary>
    public TimeSpan Retry { get; set; } = DatastarConstants.DefaultSseRetryDuration;
}