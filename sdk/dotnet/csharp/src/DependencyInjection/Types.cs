using Microsoft.FSharp.Core;
using Core = StarFederation.Datastar.FSharp;

namespace StarFederation.Datastar.DependencyInjection;

public class MergeFragmentsOptions
{
    public string? Selector { get; init; } = null;
    public FragmentMergeMode MergeMode { get; init; } = Consts.DefaultFragmentMergeMode;
    public bool UseViewTransition { get; init; } = Consts.DefaultFragmentsUseViewTransitions;
    public string? EventId { get; init; } = null;
    public TimeSpan Retry { get; init; } = Consts.DefaultSseRetryDuration;

    public static implicit operator FSharpValueOption<Core.MergeFragmentsOptions>(MergeFragmentsOptions options) => ToFSharp(options);
    public static implicit operator Core.MergeFragmentsOptions(MergeFragmentsOptions options) => ToFSharp(options);

    private static Core.MergeFragmentsOptions ToFSharp(MergeFragmentsOptions options)
    {
        return new Core.MergeFragmentsOptions(
            options.Selector ?? FSharpValueOption<string>.ValueNone,
            From(options.MergeMode),
            options.UseViewTransition,
            options.EventId ?? FSharpValueOption<string>.ValueNone,
            options.Retry
        );

        static Core.FragmentMergeMode From(FragmentMergeMode fragmentMergeMode) => fragmentMergeMode switch
        {
            FragmentMergeMode.Morph => Core.FragmentMergeMode.Morph,
            FragmentMergeMode.Inner => Core.FragmentMergeMode.Inner,
            FragmentMergeMode.Outer => Core.FragmentMergeMode.Outer,
            FragmentMergeMode.Prepend => Core.FragmentMergeMode.Prepend,
            FragmentMergeMode.Append => Core.FragmentMergeMode.Append,
            FragmentMergeMode.Before => Core.FragmentMergeMode.Before,
            FragmentMergeMode.After => Core.FragmentMergeMode.After,
            FragmentMergeMode.UpsertAttributes => Core.FragmentMergeMode.UpsertAttributes,
            _ => throw new ArgumentOutOfRangeException(nameof(fragmentMergeMode), fragmentMergeMode, message: null)
        };
    }
}

public class MergeSignalsOptions
{
    public bool OnlyIfMissing { get; init; } = Consts.DefaultMergeSignalsOnlyIfMissing;
    public string? EventId { get; init; } = null;
    public TimeSpan Retry { get; init; } = Consts.DefaultSseRetryDuration;

    public static implicit operator Core.MergeSignalsOptions(MergeSignalsOptions options) => ToFSharp(options);
    public static implicit operator FSharpValueOption<Core.MergeSignalsOptions>(MergeSignalsOptions options) => ToFSharp(options);

    private static Core.MergeSignalsOptions ToFSharp(MergeSignalsOptions options) => new(
        options.OnlyIfMissing,
        options.EventId ?? FSharpValueOption<string>.ValueNone,
        options.Retry);
}

public class RemoveFragmentsOptions
{
    public bool UseViewTransition { get; init; } = Consts.DefaultFragmentsUseViewTransitions;
    public string? EventId { get; init; } = null;
    public TimeSpan Retry { get; init; } = Consts.DefaultSseRetryDuration;

    public static implicit operator Core.RemoveFragmentsOptions(RemoveFragmentsOptions options) => ToFSharp(options);
    public static implicit operator FSharpValueOption<Core.RemoveFragmentsOptions>(RemoveFragmentsOptions options) => ToFSharp(options);

    private static Core.RemoveFragmentsOptions ToFSharp(RemoveFragmentsOptions options) => new(
        options.UseViewTransition,
        options.EventId ?? FSharpValueOption<string>.ValueNone,
        options.Retry);
}

public class ExecuteScriptOptions
{
    public bool AutoRemove { get; init; } = Consts.DefaultExecuteScriptAutoRemove;
    public string[] Attributes { get; init; } = [];
    public string? EventId { get; init; } = null;
    public TimeSpan Retry { get; init; } = Consts.DefaultSseRetryDuration;

    public static implicit operator Core.ExecuteScriptOptions(ExecuteScriptOptions options) => ToFSharp(options);
    public static implicit operator FSharpValueOption<Core.ExecuteScriptOptions>(ExecuteScriptOptions options) => ToFSharp(options);

    private static Core.ExecuteScriptOptions ToFSharp(ExecuteScriptOptions options) => new(
        options.AutoRemove,
        options.Attributes,
        options.EventId ?? FSharpValueOption<string>.ValueNone,
        options.Retry);
}

public class EventOptions
{
    public string? EventId { get; init; } = null;
    public TimeSpan Retry { get; init; } = Consts.DefaultSseRetryDuration;

    public static implicit operator Core.EventOptions(EventOptions options) => ToFSharp(options);
    public static implicit operator FSharpValueOption<Core.EventOptions>(EventOptions options) => ToFSharp(options);

    private static Core.EventOptions ToFSharp(EventOptions options) => new(options.EventId, options.Retry);
}