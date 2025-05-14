namespace Starfederation.Datastar.Types.Options;

/// <summary>
///     Options for removing fragments.
/// </summary>
public class RemoveFragmentsOptions
{
    /// <summary>
    ///     Gets or sets whether to use view transition.
    /// </summary>
    public bool UseViewTransition { get; set; } = Consts.DefaultFragmentsUseViewTransitions;

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
    public static RemoveFragmentsOptions Defaults => new();
}