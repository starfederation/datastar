using NetEscapades.EnumGenerators;

namespace StarFederation.Datastar.Enumerations;

/// <summary>
///     Represents the action to perform in the browser console.
/// </summary>
[EnumExtensions]
public enum BrowserConsoleAction
{
    /// <summary>
    ///     Clears the browser console.
    /// </summary>
    Clear,

    /// <summary>
    ///     Logs a message to the browser console.
    /// </summary>
    Log,

    /// <summary>
    ///     Logs an error to the browser console.
    /// </summary>
    Error
}