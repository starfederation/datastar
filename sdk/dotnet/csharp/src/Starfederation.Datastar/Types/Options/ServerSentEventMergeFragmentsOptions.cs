using Starfederation.Datastar.Enumerations;

namespace Starfederation.Datastar.Types.Options;

/// <summary>
///     Options for merging fragments in server-sent events.
/// </summary>
public class ServerSentEventMergeFragmentsOptions
{
    /// <summary>
    ///     Gets or sets the event ID.
    /// </summary>
    public string? EventId { get; set; }

    /// <summary>
    ///     Gets or sets the selector.
    /// </summary>
    public string? Selector { get; set; }

    /// <summary>
    ///     Gets or sets the merge mode.
    /// </summary>
    public FragmentMergeMode MergeMode { get; set; } = Consts.DefaultFragmentMergeMode;

    /// <summary>
    ///     Gets or sets the retry duration.
    /// </summary>
    public TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;
}