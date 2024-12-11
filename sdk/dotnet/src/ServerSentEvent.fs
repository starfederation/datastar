namespace StarFederation.Datastar

open System
open System.Text.RegularExpressions
open System.Threading.Tasks

type ISendServerEvent = abstract SendServerEvent: string -> Task

type ServerSentEvent =
    { EventType: EventType
      Id: string voption
      Retry: TimeSpan
      DataLines: string[] }
module ServerSentEvent =
    let serialize sse =
        seq {
            $"event: {sse.EventType |> Consts.EventType.toString}"

            if sse.Id |> ValueOption.isSome
            then $"id: {sse.Id |> ValueOption.get}"

            if (sse.Retry <> Consts.DefaultSseRetryDuration)
            then $"retry: {sse.Retry.Milliseconds}"

            yield! sse.DataLines |> Array.map (fun dataLine -> $"data: {dataLine}")

            ""; ""
        } |> String.concat "\n"

type Selector = string
module Selector =
    let value (selector:Selector) = selector
    let regex = Regex(@"[#.][-_]?[_a-zA-Z]+(?:\w|\\.)*|(?<=\s+|^)(?:\w+|\*)|\[[^\s""'=<>`]+?(?<![~|^$*])([~|^$*]?=(?:['""].*['""]|[^\s""'=<>`]+))?\]|:[\w-]+(?:\(.*\))?", RegexOptions.Compiled)
    let create (selectorString:string) = Selector selectorString
    let tryCreate (selector:string) =
        match regex.IsMatch selector with
        | true -> ValueSome (create selector)
        | false -> ValueNone

type MergeFragmentsOptions =
    { Selector: Selector voption
      MergeMode: FragmentMergeMode
      SettleDuration: TimeSpan
      UseViewTransition: bool
      EventId: string voption
      Retry: TimeSpan }
module MergeFragmentsOptions =
    let defaults =
        { Selector = ValueNone
          MergeMode = Consts.DefaultFragmentMergeMode
          SettleDuration = Consts.DefaultFragmentsSettleDuration
          UseViewTransition = Consts.DefaultFragmentsUseViewTransitions
          EventId = ValueNone
          Retry = Consts.DefaultSseRetryDuration }

type MergeSignalsOptions =
    { OnlyIfMissing: bool
      EventId: string voption
      Retry: TimeSpan }
module MergeSignalsOptions =
    let defaults =
        { OnlyIfMissing = Consts.DefaultMergeSignalsOnlyIfMissing
          EventId = ValueNone
          Retry = Consts.DefaultSseRetryDuration }

type RemoveFragmentsOptions =
    { SettleDuration: TimeSpan
      UseViewTransition: bool
      EventId: string voption
      Retry: TimeSpan }
module RemoveFragmentsOptions =
    let defaults =
        { SettleDuration = Consts.DefaultFragmentsSettleDuration
          UseViewTransition = Consts.DefaultFragmentsUseViewTransitions
          EventId = ValueNone
          Retry = Consts.DefaultSseRetryDuration }

type ExecuteScriptOptions =
    { AutoRemove: bool
      Attributes: string[]
      EventId: string voption
      Retry: TimeSpan }
module ExecuteScriptOptions =
    let defaults =
        { AutoRemove = Consts.DefaultExecuteScriptAutoRemove
          Attributes = [| Consts.DefaultExecuteScriptAttributes |]
          EventId = ValueNone
          Retry = Consts.DefaultSseRetryDuration }

type EventOptions = { EventId: string voption; Retry: TimeSpan }
module EventOptions =
    let defaults = { EventId = ValueNone; Retry = Consts.DefaultSseRetryDuration }

