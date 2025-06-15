namespace StarFederation.Datastar.FSharp

open System
open System.Collections.Generic
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open StarFederation.Datastar.FSharp.Utility

type ServerSentEvent =
    { EventType: EventType
      Id: string voption
      Retry: TimeSpan
      DataLines: string[] }

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

type MergeFragmentsOptions =
    { Selector: Selector voption
      MergeMode: FragmentMergeMode
      UseViewTransition: bool
      EventId: string voption
      Retry: TimeSpan }
type MergeSignalsOptions =
    { OnlyIfMissing: bool
      EventId: string voption
      Retry: TimeSpan }
type RemoveFragmentsOptions =
    { UseViewTransition: bool
      EventId: string voption
      Retry: TimeSpan }
type ExecuteScriptOptions =
    { AutoRemove: bool
      Attributes: string[]
      EventId: string voption
      Retry: TimeSpan }
type EventOptions = { EventId: string voption; Retry: TimeSpan }

/// <summary>
/// Read the signals from the request
/// </summary>
type IReadSignals =
    abstract GetSignalsStream : unit -> Stream
    //
    abstract ReadSignalsAsync : unit -> Task<Signals>
    abstract ReadSignalsAsync : CancellationToken -> Task<Signals>
    abstract ReadSignalsAsync<'T> : unit -> Task<'T voption>
    abstract ReadSignalsAsync<'T> : JsonSerializerOptions -> Task<'T voption>
    abstract ReadSignalsAsync<'T> : JsonSerializerOptions * CancellationToken -> Task<'T voption>

/// <summary>
/// Can send SSEs to the client
/// </summary>
type ISendServerEvent =
    abstract StartServerEventStream : unit -> Task
    abstract StartServerEventStream : additionalHeaders:(string * string)[] -> Task
    abstract StartServerEventStream : additionalHeaders:(string * string)[] * CancellationToken -> Task
    //
    abstract SendServerEvent : ServerSentEvent -> Task
    abstract SendServerEvent : ServerSentEvent * CancellationToken -> Task

module ServerSentEvent =
    let serialize sse =
        seq {
            $"event: {sse.EventType |> Consts.EventType.toString}"

            if sse.Id |> ValueOption.isSome
            then $"id: {sse.Id |> ValueOption.get}"

            if (sse.Retry <> Consts.DefaultSseRetryDuration)
            then $"retry: {sse.Retry.TotalMilliseconds}"

            yield! sse.DataLines |> Array.map (fun dataLine -> $"data: {dataLine}")

            ""; ""; ""
        } |> String.concat "\n"

module Signals =
    let value (signals:Signals) : string = signals.ToString()
    let create (signalsString:string) = Signals signalsString
    let tryCreate (signalsString:string) =
        try
            let _ = JsonObject.Parse(signalsString)
            ValueSome (Signals signalsString)
        with _ -> ValueNone
    let empty = Signals "{ }"

module SignalPath =
    let value (signalPath:SignalPath) = signalPath.ToString()
    let kebabValue signals = signals |> value |> String.toKebab
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
    let keys signalPath = signalPath |> value |> String.split ["."]
    let createJsonNodePathToValue<'T> signalPath (signalValue:'T) =
       signalPath
        |> keys
        |> Seq.rev
        |> Seq.fold (fun json key ->
            JsonObject([ KeyValuePair<string, JsonNode> (key, json) ]) :> JsonNode
            ) (JsonValue.Create(signalValue) :> JsonNode)

module Selector =
    let regex = Regex(@"[#.][-_]?[_a-zA-Z]+(?:\w|\\.)*|(?<=\s+|^)(?:\w+|\*)|\[[^\s""'=<>`]+?(?<![~|^$*])([~|^$*]?=(?:['""].*['""]|[^\s""'=<>`]+))?\]|:[\w-]+(?:\(.*\))?", RegexOptions.Compiled)
    let value (selector:Selector) = selector.ToString()
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

module MergeFragmentsOptions =
    let defaults =
        { Selector = ValueNone
          MergeMode = Consts.DefaultFragmentMergeMode
          UseViewTransition = Consts.DefaultFragmentsUseViewTransitions
          EventId = ValueNone
          Retry = Consts.DefaultSseRetryDuration }

module MergeSignalsOptions =
    let defaults =
        { OnlyIfMissing = Consts.DefaultMergeSignalsOnlyIfMissing
          EventId = ValueNone
          Retry = Consts.DefaultSseRetryDuration }

module RemoveFragmentsOptions =
    let defaults =
        { UseViewTransition = Consts.DefaultFragmentsUseViewTransitions
          EventId = ValueNone
          Retry = Consts.DefaultSseRetryDuration }

module ExecuteScriptOptions =
    let defaults =
        { AutoRemove = Consts.DefaultExecuteScriptAutoRemove
          Attributes = [| Consts.DefaultExecuteScriptAttributes |]
          EventId = ValueNone
          Retry = Consts.DefaultSseRetryDuration }

module EventOptions =
    let defaults = { EventId = ValueNone; Retry = Consts.DefaultSseRetryDuration }

