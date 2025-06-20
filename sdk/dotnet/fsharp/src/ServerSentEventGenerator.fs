namespace StarFederation.Datastar.FSharp

open System
open StarFederation.Datastar.FSharp.Utility

[<AbstractClass; Sealed>]
type ServerSentEventGenerator =
    static member PatchElements(elements, options:PatchElementsOptions) =
        { EventType = PatchElements
          Id = options.EventId
          Retry = options.Retry
          DataLines = [|
            if (options.Selector |> ValueOption.isSome) then $"{Consts.DatastarDatalineSelector} {options.Selector |> ValueOption.get |> Selector.value}"
            if (options.PatchMode <> Consts.DefaultElementPatchMode) then $"{Consts.DatastarDatalineMode} {options.PatchMode |> Consts.ElementPatchMode.toString}"
            if (options.UseViewTransition <> Consts.DefaultElementsUseViewTransitions) then $"{Consts.DatastarDatalineUseViewTransition} %A{options.UseViewTransition}"
            yield! (elements |> String.split String.newLines |> Seq.map (fun elementLine -> $"{Consts.DatastarDatalineElements} %s{elementLine}"))
            |] }
    static member PatchElements elements = ServerSentEventGenerator.PatchElements (elements, PatchElementsOptions.defaults)

    static member RemoveElement(selector, options:RemoveElementOptions) =
        { EventType = PatchElements
          Id = options.EventId
          Retry = options.Retry
          DataLines = [|
            $"{Consts.DatastarDatalineSelector} {selector |> Selector.value}"
            $"{Consts.DatastarDatalineMode} {ElementPatchMode.Remove |> Consts.ElementPatchMode.toString}"
            if (options.UseViewTransition <> Consts.DefaultElementsUseViewTransitions) then $"{Consts.DatastarDatalineUseViewTransition} %A{options.UseViewTransition}"
            |] }
    static member RemoveElement selector = ServerSentEventGenerator.RemoveElement(selector, RemoveElementOptions.defaults)

    static member PatchSignals(signals, options:PatchSignalsOptions) =
        { EventType = PatchSignals
          Id = options.EventId
          Retry = options.Retry
          DataLines = [|
            if (options.OnlyIfMissing <> Consts.DefaultPatchSignalsOnlyIfMissing) then $"{Consts.DatastarDatalineOnlyIfMissing} %A{options.OnlyIfMissing}"
            yield! signals |> Signals.value |> String.split String.newLines |> Seq.map (fun dataLine -> $"{Consts.DatastarDatalineSignals} %s{dataLine}")
            |] }
    static member PatchSignals signals = ServerSentEventGenerator.PatchSignals(signals, PatchSignalsOptions.defaults)

    static member ExecuteScript(script: string, options:ExecuteScriptOptions) =
        let script = if script.StartsWith("<script>", StringComparison.CurrentCultureIgnoreCase) then script else $"<script>{script}</script>"
        { EventType = PatchElements
          Id = options.EventId
          Retry = options.Retry
          DataLines = [|
            yield! (script |> String.split String.newLines |> Seq.map (fun scriptLine -> $"{Consts.DatastarDatalineElements} %s{scriptLine}"))
            |] }
    static member ExecuteScript script = ServerSentEventGenerator.ExecuteScript(script, ExecuteScriptOptions.defaults)
