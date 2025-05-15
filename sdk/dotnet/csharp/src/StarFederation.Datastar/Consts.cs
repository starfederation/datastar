using StarFederation.Datastar.Enumerations;

namespace StarFederation.Datastar;

/// <summary>
///     Constants used throughout the Datastar library.
/// </summary>
public static class Consts
{
    public const string DatastarKey = "datastar";
    public const string Version = "1.0.0-beta.11";

    public const string BindingSourceName = "DatastarSource";

    public const bool DefaultFragmentsUseViewTransitions = false;
    public const bool DefaultMergeSignalsOnlyIfMissing = false;
    public const bool DefaultExecuteScriptAutoRemove = true;

    public const string DefaultExecuteScriptAttributes = "type module";

    public const string DatastarDatalineSelector = "selector";
    public const string DatastarDatalineMergeMode = "mergeMode";
    public const string DatastarDatalineFragments = "fragments";
    public const string DatastarDatalineUseViewTransition = "useViewTransition";
    public const string DatastarDatalineSignals = "signals";
    public const string DatastarDatalineOnlyIfMissing = "onlyIfMissing";
    public const string DatastarDatalinePaths = "paths";
    public const string DatastarDatalineScript = "script";
    public const string DatastarDatalineAttributes = "attributes";
    public const string DatastarDatalineAutoRemove = "autoRemove";

    /// <summary>
    ///     Default: TimeSpan.FromMilliseconds 1000
    /// </summary>
    public static readonly TimeSpan DefaultSseRetryDuration = TimeSpan.FromMilliseconds(1000);

    /// <summary>
    ///     Default: morph - Morphs the fragment into the existing element using idiomorph.
    /// </summary>
    public static readonly FragmentMergeMode DefaultFragmentMergeMode = FragmentMergeMode.Morph;

    /// <summary>
    ///     Helper class for working with FragmentMergeMode.
    /// </summary>
    public static class FragmentMergeModeHelper
    {
        /// <summary>
        ///     Converts a FragmentMergeMode to its string representation.
        /// </summary>
        /// <param name="mode">The FragmentMergeMode to convert.</param>
        /// <returns>The string representation of the FragmentMergeMode.</returns>
        public static string ToString(FragmentMergeMode mode)
        {
            return mode switch
            {
                FragmentMergeMode.Morph            => "morph",
                FragmentMergeMode.UpsertAttributes => "upsertAttributes",
                _                                  => mode.ToString().ToLowerInvariant()
            };
        }
    }

    /// <summary>
    ///     Helper class for working with EventType.
    /// </summary>
    public static class EventType
    {
        /// <summary>
        ///     Converts an EventType to its string representation.
        /// </summary>
        /// <param name="eventType">The EventType to convert.</param>
        /// <returns>The string representation of the EventType.</returns>
        public static string ToString(Enumerations.EventType eventType)
        {
            return eventType switch
            {
                Enumerations.EventType.MergeFragments  => "datastar-merge-fragments",
                Enumerations.EventType.ExecuteScript   => "datastar-execute-script",
                Enumerations.EventType.MergeSignals    => "datastar-merge-signals",
                Enumerations.EventType.RemoveFragments => "datastar-remove-fragments",
                Enumerations.EventType.RemoveSignals   => "datastar-remove-signals",
                _                                      => throw new ArgumentOutOfRangeException(nameof(eventType))
            };
        }
    }
}