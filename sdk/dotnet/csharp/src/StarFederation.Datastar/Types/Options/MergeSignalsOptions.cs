using StarFederation.Datastar.Interfaces;

namespace StarFederation.Datastar.Types.Options;

/// <summary>
///     Options for merging signals.
/// </summary>
public class MergeSignalsOptions : IDatastarOptions
{
    /// <summary>
    ///     Gets or sets whether to only merge if missing.
    /// </summary>
    public bool OnlyIfMissing { get; set; } = DatastarConstants.DefaultMergeSignalsOnlyIfMissing;

    /// <summary>
    ///     Gets or sets the event ID.
    /// </summary>
    public string? EventId { get; set; }

    /// <summary>
    ///     Gets or sets the retry duration.
    /// </summary>
    public TimeSpan Retry { get; set; } = DatastarConstants.DefaultSseRetryDuration;
}