namespace StarFederation.Datastar

open System
open System.Text.RegularExpressions
open System.Threading.Tasks

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
type SignalsPath = string

/// <summary>
/// An HTML selector name
/// </summary>
type Selector = string

type MergeFragmentsOptions =
    { Selector: Selector voption
      MergeMode: FragmentMergeMode
      SettleDuration: TimeSpan
      UseViewTransition: bool
      EventId: string voption
      Retry: TimeSpan }
type MergeSignalsOptions =
    { OnlyIfMissing: bool
      EventId: string voption
      Retry: TimeSpan }
type RemoveFragmentsOptions =
    { SettleDuration: TimeSpan
      UseViewTransition: bool
      EventId: string voption
      Retry: TimeSpan }
type ExecuteScriptOptions =
    { AutoRemove: bool
      Attributes: string[]
      EventId: string voption
      Retry: TimeSpan }
type EventOptions = { EventId: string voption; Retry: TimeSpan }

/// <summary>
///
/// </summary>
type IReadSignals =
    abstract ReadSignals: unit -> ValueTask<Signals voption>
    abstract ReadSignals<'T>: unit -> ValueTask<'T voption>

/// <summary>
/// Can send SSEs to the client
/// </summary>
type ISendServerEvent = abstract SendServerEvent: ServerSentEvent -> Task

module ServerSentEvent =
    let serialize sse =
        seq {
            $"event: {sse.EventType |> Consts.EventType.toString}"

            if sse.Id |> ValueOption.isSome
            then $"id: {sse.Id |> ValueOption.get}"

            if (sse.Retry <> Consts.DefaultSseRetryDuration)
            then $"retry: {sse.Retry.Milliseconds}"

            yield! sse.DataLines |> Array.map (fun dataLine -> $"data: {dataLine}")

            ""; ""; ""
        } |> String.concat "\n"

module Signals =
    let value = id
    let create (signals:string) = Signals signals
    let tryCreate (signals:string) = ValueSome (Signals signals)
    let empty = Signals "{ }"

module SignalsPath =
    let value = id
    // TODO: Validation on string -> path
    let create (signalsPath:string) = SignalsPath signalsPath
    let tryCreate (signalsPath:string) = ValueSome (create signalsPath)

module Selector =
    let value (selector:Selector) = selector
    let regex = Regex(@"[#.][-_]?[_a-zA-Z]+(?:\w|\\.)*|(?<=\s+|^)(?:\w+|\*)|\[[^\s""'=<>`]+?(?<![~|^$*])([~|^$*]?=(?:['""].*['""]|[^\s""'=<>`]+))?\]|:[\w-]+(?:\(.*\))?", RegexOptions.Compiled)
    let create (selectorString:string) = Selector selectorString
    let tryCreate (selector:string) =
        match regex.IsMatch selector with
        | true -> ValueSome (create selector)
        | false -> ValueNone

module MergeFragmentsOptions =
    let defaults =
        { Selector = ValueNone
          MergeMode = Consts.DefaultFragmentMergeMode
          SettleDuration = Consts.DefaultFragmentsSettleDuration
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
        { SettleDuration = Consts.DefaultFragmentsSettleDuration
          UseViewTransition = Consts.DefaultFragmentsUseViewTransitions
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

