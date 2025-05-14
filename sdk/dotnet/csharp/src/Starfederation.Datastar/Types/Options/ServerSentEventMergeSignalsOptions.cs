namespace Starfederation.Datastar.Types.Options;

/// <summary>
///     Options for merging signals in server-sent events.
/// </summary>
public class ServerSentEventMergeSignalsOptions
{
    /// <summary>
    ///     Gets or sets whether to only merge if missing.
    /// </summary>
    public bool OnlyIfMissing { get; set; } = Consts.DefaultMergeSignalsOnlyIfMissing;

    /// <summary>
    ///     Gets or sets the event ID.
    /// </summary>
    public string? EventId { get; set; }

    /// <summary>
    ///     Gets or sets the retry duration.
    /// </summary>
    public TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;
}