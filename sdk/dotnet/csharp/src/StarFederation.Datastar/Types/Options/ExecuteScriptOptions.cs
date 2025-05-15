namespace StarFederation.Datastar.Types.Options;

/// <summary>
///     Options for executing scripts.
/// </summary>
public class ExecuteScriptOptions
{
    /// <summary>
    ///     Gets or sets whether to auto remove the script.
    /// </summary>
    public bool AutoRemove { get; set; } = Consts.DefaultExecuteScriptAutoRemove;

    /// <summary>
    ///     Gets or sets the script attributes.
    /// </summary>
    public string[] Attributes { get; set; } = Consts.DefaultExecuteScriptAttributes.Split(' ', StringSplitOptions.RemoveEmptyEntries);

    /// <summary>
    ///     Gets or sets the retry duration.
    /// </summary>
    public TimeSpan Retry { get; set; } = Consts.DefaultSseRetryDuration;

    /// <summary>
    ///     Gets the default options.
    /// </summary>
    public static ExecuteScriptOptions Defaults => new();
}