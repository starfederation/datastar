using Microsoft.FSharp.Core;
using Core = StarFederation.Datastar.FSharp;

namespace StarFederation.Datastar.DependencyInjection;

public class PatchElementsOptions
{
    public string? Selector { get; init; } = null;
    public ElementPatchMode PatchMode { get; init; } = Consts.DefaultElementPatchMode;
    public bool UseViewTransition { get; init; } = Consts.DefaultElementsUseViewTransitions;
    public string? EventId { get; init; } = null;
    public TimeSpan Retry { get; init; } = Consts.DefaultSseRetryDuration;

    public static implicit operator FSharpValueOption<Core.PatchElementsOptions>(PatchElementsOptions options) => ToFSharp(options);
    public static implicit operator Core.PatchElementsOptions(PatchElementsOptions options) => ToFSharp(options);

    private static Core.PatchElementsOptions ToFSharp(PatchElementsOptions options)
    {
        return new Core.PatchElementsOptions(
            options.Selector ?? FSharpValueOption<string>.ValueNone,
            From(options.PatchMode),
            options.UseViewTransition,
            options.EventId ?? FSharpValueOption<string>.ValueNone,
            options.Retry
        );

        static Core.ElementPatchMode From(ElementPatchMode patchElementsMode) => patchElementsMode switch
        {
            ElementPatchMode.Inner => Core.ElementPatchMode.Inner,
            ElementPatchMode.Outer => Core.ElementPatchMode.Outer,
            ElementPatchMode.Prepend => Core.ElementPatchMode.Prepend,
            ElementPatchMode.Append => Core.ElementPatchMode.Append,
            ElementPatchMode.Before => Core.ElementPatchMode.Before,
            ElementPatchMode.After => Core.ElementPatchMode.After,
            ElementPatchMode.Remove => Core.ElementPatchMode.Remove,
            ElementPatchMode.Replace => Core.ElementPatchMode.Replace,
            _ => throw new ArgumentOutOfRangeException(nameof(patchElementsMode), patchElementsMode, null)
        };
    }
}

public class PatchSignalsOptions
{
    public bool OnlyIfMissing { get; init; } = Consts.DefaultPatchSignalsOnlyIfMissing;
    public string? EventId { get; init; } = null;
    public TimeSpan Retry { get; init; } = Consts.DefaultSseRetryDuration;

    public static implicit operator Core.PatchSignalsOptions(PatchSignalsOptions options) => ToFSharp(options);
    public static implicit operator FSharpValueOption<Core.PatchSignalsOptions>(PatchSignalsOptions options) => ToFSharp(options);

    private static Core.PatchSignalsOptions ToFSharp(PatchSignalsOptions options) => new(
        options.OnlyIfMissing,
        options.EventId ?? FSharpValueOption<string>.ValueNone,
        options.Retry);
}

public class RemoveFragmentOptions
{
    public bool UseViewTransition { get; init; } = Consts.DefaultElementsUseViewTransitions;
    public string? EventId { get; init; } = null;
    public TimeSpan Retry { get; init; } = Consts.DefaultSseRetryDuration;

    public static implicit operator Core.RemoveElementOptions(RemoveFragmentOptions options) => ToFSharp(options);
    public static implicit operator FSharpValueOption<Core.RemoveElementOptions>(RemoveFragmentOptions options) => ToFSharp(options);

    private static Core.RemoveElementOptions ToFSharp(RemoveFragmentOptions options) => new(
        options.UseViewTransition,
        options.EventId ?? FSharpValueOption<string>.ValueNone,
        options.Retry);
}

public class ExecuteScriptOptions
{
    public string? EventId { get; init; } = null;
    public TimeSpan Retry { get; init; } = Consts.DefaultSseRetryDuration;

    public static implicit operator Core.ExecuteScriptOptions(ExecuteScriptOptions options) => ToFSharp(options);
    public static implicit operator FSharpValueOption<Core.ExecuteScriptOptions>(ExecuteScriptOptions options) => ToFSharp(options);

    private static Core.ExecuteScriptOptions ToFSharp(ExecuteScriptOptions options) => new(
        options.EventId ?? FSharpValueOption<string>.ValueNone,
        options.Retry);
}
