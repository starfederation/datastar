using NetEscapades.EnumGenerators;

namespace Starfederation.Datastar.Enumerations;

/// <summary>
///     Represents the different types of events.
/// </summary>
[EnumExtensions]
public enum EventType
{
    /// <summary>
    ///     An event for merging HTML fragments into the DOM.
    /// </summary>
    MergeFragments,

    /// <summary>
    ///     An event for merging signals.
    /// </summary>
    MergeSignals,

    /// <summary>
    ///     An event for removing HTML fragments from the DOM.
    /// </summary>
    RemoveFragments,

    /// <summary>
    ///     An event for removing signals.
    /// </summary>
    RemoveSignals,

    /// <summary>
    ///     An event for executing &lt;script/&gt; elements in the browser.
    /// </summary>
    ExecuteScript
}