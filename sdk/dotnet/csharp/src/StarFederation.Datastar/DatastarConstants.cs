using StarFederation.Datastar.Enumerations;

namespace StarFederation.Datastar;

/// <summary>
///     Constants used throughout the Datastar library.
/// </summary>
public static class DatastarConstants
{
    public const string DatastarKey = "datastar";
    public const string DatastarVersion = "1.0.0-beta.11";

    public const string BindingSourceName = "DatastarSource";
    
    public const string DatastarSseContentType = "text/event-stream";

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
    public const FragmentMergeMode DefaultFragmentMergeMode = FragmentMergeMode.Morph;
}