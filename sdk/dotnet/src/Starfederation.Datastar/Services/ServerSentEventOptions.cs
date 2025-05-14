namespace Starfederation.Datastar.Services;

/// <summary>
///     Common options for server-sent events.
/// </summary>
public class ServerSentEventOptions
{
    /// <summary>
    ///     Gets or sets the event ID.
    /// </summary>
    public string? EventId { get; set; }

    /// <summary>
    ///     Gets or sets the retry duration.
    /// </summary>
    public TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;
}