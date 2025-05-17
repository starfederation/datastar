namespace StarFederation.Datastar.FSharp

open StarFederation.Datastar.FSharp.Utility

/// <summary>
/// Converts requests into SSEs, serializes, and sends to sseHandler handlers
/// </summary>
[<AbstractClass; Sealed>]
type ServerSentEventGenerator =
    static member inline private send (seeHandler: ISendServerEvent) sse = seeHandler.SendServerEvent sse

    static member Send (sse, sseHandler:ISendServerEvent) = ServerSentEventGenerator.send sseHandler sse

    static member MergeFragments (sseHandler, fragments, ?options:MergeFragmentsOptions) =
        let options = defaultArg options MergeFragmentsOptions.defaults
        { EventType = MergeFragments
          Id = options.EventId
          Retry = options.Retry
          DataLines = [|
            if (options.Selector |> ValueOption.isSome) then $"{Consts.DatastarDatalineSelector} {options.Selector |> ValueOption.get |> Selector.value}"
            if (options.MergeMode <> Consts.DefaultFragmentMergeMode) then $"{Consts.DatastarDatalineMergeMode} {options.MergeMode |> Consts.FragmentMergeMode.toString}"
            if (options.UseViewTransition <> Consts.DefaultFragmentsUseViewTransitions) then $"{Consts.DatastarDatalineUseViewTransition} %A{options.UseViewTransition}"
            yield! (fragments |> String.split String.newLines |> Seq.map (fun fragmentLine -> $"{Consts.DatastarDatalineFragments} %s{fragmentLine}"))
            |] }
        |> ServerSentEventGenerator.send sseHandler

    static member RemoveFragments (sseHandler, selector, ?options:RemoveFragmentsOptions) =
        let options = defaultArg options RemoveFragmentsOptions.defaults
        { EventType = RemoveFragments
          Id = options.EventId
          Retry = options.Retry
          DataLines = [|
            $"{Consts.DatastarDatalineSelector} {selector |> Selector.value}"
            if (options.UseViewTransition <> Consts.DefaultFragmentsUseViewTransitions) then $"{Consts.DatastarDatalineUseViewTransition} %A{options.UseViewTransition}"
            |] }
        |> ServerSentEventGenerator.send sseHandler

    static member MergeSignals (sseHandler, mergeSignals, ?options:MergeSignalsOptions) =
        let options = defaultArg options MergeSignalsOptions.defaults
        { EventType = MergeSignals
          Id = options.EventId
          Retry = options.Retry
          DataLines = [|
            if (options.OnlyIfMissing <> Consts.DefaultMergeSignalsOnlyIfMissing) then $"{Consts.DatastarDatalineOnlyIfMissing} %A{options.OnlyIfMissing}"
            yield! mergeSignals |> Signals.value |> String.split String.newLines |> Seq.map (fun dataLine -> $"{Consts.DatastarDatalineSignals} %s{dataLine}")
            |] }
       |> ServerSentEventGenerator.send sseHandler

    static member RemoveSignals (sseHandler, paths, ?options:EventOptions) =
        let options = defaultArg options EventOptions.defaults
        let paths' = paths |> Seq.map SignalPath.value |> String.concat " "
        { EventType = RemoveSignals
          Id = options.EventId
          Retry = options.Retry
          DataLines = [| $"{Consts.DatastarDatalinePaths} {paths'}" |] }
        |> ServerSentEventGenerator.send sseHandler

    static member ExecuteScript (sseHandler, script, ?options:ExecuteScriptOptions) =
        let options = defaultArg options ExecuteScriptOptions.defaults
        { EventType = ExecuteScript
          Id = options.EventId
          Retry = options.Retry
          DataLines = [|
            if (options.AutoRemove <> Consts.DefaultExecuteScriptAutoRemove) then $"{Consts.DatastarDatalineAutoRemove} %A{options.AutoRemove}"
            if (not <| Seq.forall2 (=) options.Attributes [| Consts.DefaultExecuteScriptAttributes |] ) then
                yield! options.Attributes |> Seq.map (fun attr -> $"{Consts.DefaultExecuteScriptAttributes} {attr}")
            yield! script |> String.split String.newLines |> Seq.map (fun scriptLine -> $"{Consts.DatastarDatalineScript} %s{scriptLine}")
            |] }
        |> ServerSentEventGenerator.send sseHandler
