using System.Text.Json;

namespace StarFederation.Datastar.Types.Options;

/// <summary>
///     Options for merging signals in server-sent events.
/// </summary>
public sealed class ServerSentEventMergeSignalsOptions
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

    public JsonSerializerOptions? JsonSerializerOptions { get; set; }
}