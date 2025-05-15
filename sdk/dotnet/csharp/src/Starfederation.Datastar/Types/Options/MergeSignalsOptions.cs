namespace Starfederation.Datastar.Types.Options;

/// <summary>
///     Options for merging signals.
/// </summary>
public class MergeSignalsOptions
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

    /// <summary>
    ///     Gets the default options.
    /// </summary>
    public static MergeSignalsOptions Defaults => new();
}