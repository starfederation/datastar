namespace StarFederation.Datastar.FSharp

open StarFederation.Datastar.FSharp.Utility

[<AbstractClass; Sealed>]
type ServerSentEventGenerator =
    static member MergeFragments(fragments, options:MergeFragmentsOptions) =
        { EventType = MergeFragments
          Id = options.EventId
          Retry = options.Retry
          DataLines = [|
            if (options.Selector |> ValueOption.isSome) then $"{Consts.DatastarDatalineSelector} {options.Selector |> ValueOption.get |> Selector.value}"
            if (options.MergeMode <> Consts.DefaultFragmentMergeMode) then $"{Consts.DatastarDatalineMergeMode} {options.MergeMode |> Consts.FragmentMergeMode.toString}"
            if (options.UseViewTransition <> Consts.DefaultFragmentsUseViewTransitions) then $"{Consts.DatastarDatalineUseViewTransition} %A{options.UseViewTransition}"
            yield! (fragments |> String.split String.newLines |> Seq.map (fun fragmentLine -> $"{Consts.DatastarDatalineFragments} %s{fragmentLine}"))
            |] }
    static member MergeFragments fragments = ServerSentEventGenerator.MergeFragments (fragments, MergeFragmentsOptions.defaults)

    static member RemoveFragments(selector, options:RemoveFragmentsOptions) =
        { EventType = RemoveFragments
          Id = options.EventId
          Retry = options.Retry
          DataLines = [|
            $"{Consts.DatastarDatalineSelector} {selector |> Selector.value}"
            if (options.UseViewTransition <> Consts.DefaultFragmentsUseViewTransitions) then $"{Consts.DatastarDatalineUseViewTransition} %A{options.UseViewTransition}"
            |] }
    static member RemoveFragments selector = ServerSentEventGenerator.RemoveFragments(selector, RemoveFragmentsOptions.defaults)

    static member MergeSignals(signals, options:MergeSignalsOptions) =
        { EventType = MergeSignals
          Id = options.EventId
          Retry = options.Retry
          DataLines = [|
            if (options.OnlyIfMissing <> Consts.DefaultMergeSignalsOnlyIfMissing) then $"{Consts.DatastarDatalineOnlyIfMissing} %A{options.OnlyIfMissing}"
            yield! signals |> Signals.value |> String.split String.newLines |> Seq.map (fun dataLine -> $"{Consts.DatastarDatalineSignals} %s{dataLine}")
            |] }
    static member MergeSignals signals = ServerSentEventGenerator.MergeSignals(signals, MergeSignalsOptions.defaults)

    static member RemoveSignals(signalPaths, options:EventOptions) =
        let paths' = signalPaths |> Seq.map SignalPath.value |> String.concat " "
        { EventType = RemoveSignals
          Id = options.EventId
          Retry = options.Retry
          DataLines = [| $"{Consts.DatastarDatalinePaths} {paths'}" |] }
    static member RemoveSignals signalPaths = ServerSentEventGenerator.RemoveSignals(signalPaths, EventOptions.defaults)

    static member ExecuteScript(script, options:ExecuteScriptOptions) =
        { EventType = ExecuteScript
          Id = options.EventId
          Retry = options.Retry
          DataLines = [|
            if (options.AutoRemove <> Consts.DefaultExecuteScriptAutoRemove) then $"{Consts.DatastarDatalineAutoRemove} %A{options.AutoRemove}"
            if (not <| Seq.forall2 (=) options.Attributes [| Consts.DefaultExecuteScriptAttributes |] ) then
                yield! options.Attributes |> Seq.map (fun attr -> $"{Consts.DefaultExecuteScriptAttributes} {attr}")
            yield! script |> String.split String.newLines |> Seq.map (fun scriptLine -> $"{Consts.DatastarDatalineScript} %s{scriptLine}")
            |] }
    static member ExecuteScript script = ServerSentEventGenerator.ExecuteScript(script, ExecuteScriptOptions.defaults)
