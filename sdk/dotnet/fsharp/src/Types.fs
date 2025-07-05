namespace StarFederation.Datastar.FSharp

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.RegularExpressions
open StarFederation.Datastar.FSharp.Utility

/// <summary>
/// Signals read to and from Datastar on the front end
/// </summary>
type Signals = string

/// <summary>
/// A dotted path into Signals to access a key/value pair
/// </summary>
type SignalPath = string

/// <summary>
/// An HTML selector name
/// </summary>
type Selector = string

[<Struct>]
type PatchElementsOptions =
    { Selector: Selector voption
      PatchMode: ElementPatchMode
      UseViewTransition: bool
      EventId: string voption
      Retry: TimeSpan }
    with
    static member Defaults =
        { Selector = ValueNone
          PatchMode = Consts.DefaultElementPatchMode
          UseViewTransition = Consts.DefaultElementsUseViewTransitions
          EventId = ValueNone
          Retry = Consts.DefaultSseRetryDuration }

[<Struct>]
type RemoveElementOptions =
    { UseViewTransition: bool
      EventId: string voption
      Retry: TimeSpan }
    with
    static member Defaults =
        { UseViewTransition = Consts.DefaultElementsUseViewTransitions
          EventId = ValueNone
          Retry = Consts.DefaultSseRetryDuration }

[<Struct>]
type PatchSignalsOptions =
    { OnlyIfMissing: bool
      EventId: string voption
      Retry: TimeSpan }
    with
    static member Defaults =
        { OnlyIfMissing = Consts.DefaultPatchSignalsOnlyIfMissing
          EventId = ValueNone
          Retry = Consts.DefaultSseRetryDuration }

[<Struct>]
type ExecuteScriptOptions =
    { EventId: string voption; Retry: TimeSpan }
    with
    static member Defaults =
        { EventId = ValueNone
          Retry = Consts.DefaultSseRetryDuration }

module JsonSerializerOptions =
    let SignalsDefault =
        let options = JsonSerializerOptions()
        options.PropertyNameCaseInsensitive <- true
        options

module Signals =
    let inline value (signals:Signals) : string = signals
    let create (signalsString:string) = Signals signalsString
    let tryCreate (signalsString:string) =
        try
            let _ = JsonObject.Parse(signalsString)
            ValueSome (Signals signalsString)
        with _ -> ValueNone
    let empty = Signals "{ }"

module SignalPath =
    let inline value (signalPath:SignalPath) = signalPath
    let isValidKey (signalPathKey:string) =
        signalPathKey |> String.isPopulated && signalPathKey.ToCharArray() |> Seq.forall (fun chr -> Char.IsLetter chr || Char.IsNumber chr || chr = '_')
    let isValid (signalPathString:string) = signalPathString.Split('.') |> Array.forall isValidKey
    let tryCreate (signalPathString:string) =
        if isValid signalPathString
        then ValueSome (SignalPath signalPathString)
        else ValueNone
    let sp (signalPathString:string) =
        if isValid signalPathString
        then SignalPath signalPathString
        else failwith $"{signalPathString} is not a valid signal path"
    let create = sp
    let kebabValue signals = signals |> value |> String.toKebab
    let keys (signalPath:SignalPath) = signalPath.Split('.')
    let createJsonNodePathToValue<'T> signalPath (signalValue:'T) =
        signalPath
        |> keys
        |> Seq.rev
        |> Seq.fold (fun json key ->
            JsonObject([ KeyValuePair<string, JsonNode> (key, json) ]) :> JsonNode
            ) (JsonValue.Create(signalValue) :> JsonNode)

module Selector =
    let inline value (selector:Selector) = selector
    let regex = Regex(@"[#.][-_]?[_a-zA-Z]+(?:\w|\\.)*|(?<=\s+|^)(?:\w+|\*)|\[[^\s""'=<>`]+?(?<![~|^$*])([~|^$*]?=(?:['""].*['""]|[^\s""'=<>`]+))?\]|:[\w-]+(?:\(.*\))?", RegexOptions.Compiled)
    let isValid (selectorString:string) = regex.IsMatch selectorString
    let tryCreate (selectorString:string) =
        if isValid selectorString
        then ValueSome (Selector selectorString)
        else ValueNone
    let sel (selectorString:string) =
        if isValid selectorString
        then Selector selectorString
        else failwith $"{selectorString} is not a valid selector"
    let create = sel
