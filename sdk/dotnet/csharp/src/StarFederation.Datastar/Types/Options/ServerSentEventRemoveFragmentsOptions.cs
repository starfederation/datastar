using StarFederation.Datastar.Interfaces;

namespace StarFederation.Datastar.Types.Options;

/// <summary>
///     Options for removing fragments in server-sent events.
/// </summary>
public class ServerSentEventRemoveFragmentsOptions : IDatastarServerSentEventOptions
{
    /// <summary>
    ///     Gets or sets the event ID.
    /// </summary>
    public string? EventId { get; set; }

    /// <summary>
    ///     Gets or sets whether to use view transition.
    /// </summary>
    public bool UseViewTransition { get; set; } = DatastarConstants.DefaultFragmentsUseViewTransitions;

    /// <summary>
    ///     Gets or sets the retry duration.
    /// </summary>
    public TimeSpan Retry { get; set; } = DatastarConstants.DefaultSseRetryDuration;
}