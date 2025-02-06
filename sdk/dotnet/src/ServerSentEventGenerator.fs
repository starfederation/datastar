namespace StarFederation.Datastar

open System.Threading.Tasks

/// <summary>
/// Converts requests into SSEs, serializes, and sends to env handlers
/// </summary>
module ServerSentEventGenerator =

    let send (env:ISendServerEvent) sse = env.SendServerEvent sse

    let mergeFragmentsWithOptions (options:MergeFragmentsOptions) env fragments =
        { EventType = MergeFragments
          Id = options.EventId
          Retry = options.Retry
          DataLines = [|
            if (options.Selector |> ValueOption.isSome) then $"{Consts.DatastarDatalineSelector} {options.Selector |> ValueOption.get |> Selector.value}"
            if (options.MergeMode <> Consts.DefaultFragmentMergeMode) then $"{Consts.DatastarDatalineMergeMode} {options.MergeMode |> Consts.FragmentMergeMode.toString}"
            if (options.SettleDuration <> Consts.DefaultFragmentsSettleDuration) then $"{Consts.DatastarDatalineSettleDuration} {options.SettleDuration.Milliseconds}"
            if (options.UseViewTransition <> Consts.DefaultFragmentsUseViewTransitions) then $"{Consts.DatastarDatalineUseViewTransition} %A{options.UseViewTransition}"
            yield! (fragments |> Utility.splitLine |> Seq.map (fun fragmentLine -> $"{Consts.DatastarDatalineFragments} %s{fragmentLine}"))
            |] }
        |> send env
    let mergeFragments env = mergeFragmentsWithOptions MergeFragmentsOptions.defaults env

    let removeFragmentsWithOptions (options:RemoveFragmentsOptions) env selector =
        { EventType = RemoveFragments
          Id = options.EventId
          Retry = options.Retry
          DataLines = [|
            $"{Consts.DatastarDatalineSelector} {selector |> Selector.value}"
            if (options.SettleDuration <> Consts.DefaultFragmentsSettleDuration) then $"{Consts.DatastarDatalineSettleDuration} {options.SettleDuration.Milliseconds}"
            if (options.UseViewTransition <> Consts.DefaultFragmentsUseViewTransitions) then $"{Consts.DatastarDatalineUseViewTransition} %A{options.UseViewTransition}"
            |] }
        |> send env
    let removeFragments env = removeFragmentsWithOptions RemoveFragmentsOptions.defaults env

    let mergeSignalsWithOptions (options:MergeSignalsOptions) env (mergeSignals:Signals) : Task =
        { EventType = MergeSignals
          Id = options.EventId
          Retry = options.Retry
          DataLines = [|
            if (options.OnlyIfMissing <> Consts.DefaultMergeSignalsOnlyIfMissing) then $"{Consts.DatastarDatalineOnlyIfMissing} %A{options.OnlyIfMissing}"
            yield! mergeSignals |> Signals.value |> Utility.splitLine |> Seq.map (fun dataLine -> $"{Consts.DatastarDatalineSignals} %s{dataLine}")
            |] }
       |> send env
    let mergeSignals env = mergeSignalsWithOptions MergeSignalsOptions.defaults env

    let removeSignalsWithOptions options env paths =
        let paths' = paths |> Seq.map SignalsPath.value |> String.concat " "
        { EventType = RemoveSignals
          Id = options.EventId
          Retry = options.Retry
          DataLines = [| $"{Consts.DatastarDatalineSelector} {paths'}" |] }
        |> send env
    let removeSignals env = removeSignalsWithOptions EventOptions.defaults env

    let executeScriptWithOptions (options:ExecuteScriptOptions) env script =
        { EventType = ExecuteScript
          Id = options.EventId
          Retry = options.Retry
          DataLines = [|
            if (options.AutoRemove <> Consts.DefaultExecuteScriptAutoRemove) then $"{Consts.DatastarDatalineAutoRemove} %A{options.AutoRemove}"
            if (not <| Seq.forall2 (=) options.Attributes [| Consts.DefaultExecuteScriptAttributes |] ) then
                yield! options.Attributes |> Seq.map (fun attr -> $"{Consts.DefaultExecuteScriptAttributes} {attr}")
            yield! script |> Utility.splitLine |> Seq.map (fun scriptLine -> $"{Consts.DatastarDatalineScript} %s{scriptLine}")
          |] }
        |> send env
    let executeScript env = executeScriptWithOptions ExecuteScriptOptions.defaults env
