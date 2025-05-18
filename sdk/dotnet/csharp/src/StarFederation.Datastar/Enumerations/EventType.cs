
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using NetEscapades.EnumGenerators;

namespace StarFederation.Datastar.Enumerations;

//TODO the [Display] can be [Description]

/// <summary>
///     Represents the different types of events.
/// </summary>
[EnumExtensions]
public enum EventType
{
    /// <summary>
    ///     An event for merging HTML fragments into the DOM.
    /// </summary>
    [Description("datastar-merge-fragments")]
    MergeFragments,

    /// <summary>
    ///     An event for merging signals.
    /// </summary>
    [Description("datastar-merge-signals")]
    MergeSignals,

    /// <summary>
    ///     An event for removing HTML fragments from the DOM.
    /// </summary>
    [Description("datastar-remove-fragments")]
    RemoveFragments,

    /// <summary>
    ///     An event for removing signals.
    /// </summary>
    [Description("datastar-remove-signals")]
    RemoveSignals,

    /// <summary>
    ///     An event for executing &lt;script/&gt; elements in the browser.
    /// </summary>
    [Description("datastar-execute-script")]
    ExecuteScript
}