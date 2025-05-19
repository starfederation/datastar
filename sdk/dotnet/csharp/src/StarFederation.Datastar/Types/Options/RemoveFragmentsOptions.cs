using StarFederation.Datastar.Interfaces;

namespace StarFederation.Datastar.Types.Options;

/// <summary>
///     Options for removing fragments.
/// </summary>
public class RemoveFragmentsOptions : IDatastarOptions
{
    /// <summary>
    ///     Gets or sets whether to use view transition.
    /// </summary>
    public bool UseViewTransition { get; set; } = DatastarConstants.DefaultFragmentsUseViewTransitions;

    /// <summary>
    ///     Gets or sets the event ID.
    /// </summary>
    public string? EventId { get; set; }

    /// <summary>
    ///     Gets or sets the retry duration.
    /// </summary>
    public TimeSpan Retry { get; set; } = DatastarConstants.DefaultSseRetryDuration;
}