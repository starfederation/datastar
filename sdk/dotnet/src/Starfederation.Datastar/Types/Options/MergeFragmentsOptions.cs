using Starfederation.Datastar.Enumerations;

namespace Starfederation.Datastar.Types.Options;

/// <summary>
///     Options for merging fragments.
/// </summary>
public class MergeFragmentsOptions
{
    /// <summary>
    ///     Gets or sets the selector.
    /// </summary>
    public Optional<Selector> Selector { get; set; } = Optional<Selector>.None;

    /// <summary>
    ///     Gets or sets the merge mode.
    /// </summary>
    public FragmentMergeMode MergeMode { get; set; } = Consts.DefaultFragmentMergeMode;

    /// <summary>
    ///     Gets or sets the retry duration.
    /// </summary>
    public TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;

    /// <summary>
    ///     Gets the default options.
    /// </summary>
    public static MergeFragmentsOptions Defaults => new();
}