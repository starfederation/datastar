using StarFederation.Datastar.Enumerations;
using StarFederation.Datastar.Interfaces;

namespace StarFederation.Datastar.Types.Options;

/// <summary>
///     Options for merging fragments.
/// </summary>
public class MergeFragmentsOptions : IDatastarOptions
{
    /// <summary>
    ///     Gets or sets the selector.
    /// </summary>
    public DatastarSelector? Selector { get; set; }

    /// <summary>
    ///     Gets or sets the merge mode.
    /// </summary>
    public FragmentMergeMode MergeMode { get; set; } = DatastarConstants.DefaultFragmentMergeMode;

    /// <summary>
    ///     Gets or sets the retry duration.
    /// </summary>
    public TimeSpan Retry { get; set; } = DatastarConstants.DefaultSseRetryDuration;
}