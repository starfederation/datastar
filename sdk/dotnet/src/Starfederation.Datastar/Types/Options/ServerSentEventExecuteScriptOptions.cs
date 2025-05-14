namespace Starfederation.Datastar.Types.Options;

/// <summary>
///     Options for executing scripts in server-sent events.
/// </summary>
public class ServerSentEventExecuteScriptOptions
{
    /// <summary>
    ///     Gets or sets whether to auto remove the script.
    /// </summary>
    public bool AutoRemove { get; set; } = Consts.DefaultExecuteScriptAutoRemove;

    /// <summary>
    ///     Gets or sets the script attributes.
    /// </summary>
    public string? Attributes { get; set; } = Consts.DefaultExecuteScriptAttributes;

    /// <summary>
    ///     Gets or sets the retry duration.
    /// </summary>
    public TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;
}