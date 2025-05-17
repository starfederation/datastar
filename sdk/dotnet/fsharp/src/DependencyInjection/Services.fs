namespace StarFederation.Datastar.FSharp.DependencyInjection

open System.Threading.Tasks
open StarFederation.Datastar.FSharp
open StarFederation.Datastar.FSharp.Utility

type IDatastarServerSentEventService =
    abstract Handler : ISendServerEvent
    abstract MergeFragmentsAsync: fragment:string -> Task
    abstract MergeFragmentsAsync: fragment:string * options:ServerSentEventMergeFragmentsOptions -> Task
    abstract RemoveFragmentsAsync: selector:Selector -> Task
    abstract RemoveFragmentsAsync: selector:Selector * options:ServerSentEventRemoveFragmentsOptions -> Task
    abstract MergeSignalsAsync: dataSignals:Signals -> Task
    abstract MergeSignalsAsync: dataSignals:Signals * options:ServerSentEventMergeSignalsOptions -> Task
    abstract RemoveSignalsAsync: paths:SignalPath seq -> Task
    abstract RemoveSignalsAsync: paths:SignalPath seq * options:ServerSentEventOptions -> Task
    abstract ExecuteScriptAsync: script:string -> Task
    abstract ExecuteScriptAsync: script:string * options:ServerSentEventExecuteScriptOptions -> Task

and IDatastarSignalsReaderService =
    abstract Handler : IReadSignals
    /// <summary>
    /// Read the signals and return as a serialized string
    /// </summary>
    /// <returns>A task that represents the asynchronous read operation. The result contains the serialized signals.</returns>
    abstract ReadSignalsAsync : unit -> Task<Signals>
    /// <summary>
    /// Read the signals and deserialize as a 'T
    /// </summary>
    /// <returns>A task that represents the asynchronous read and deserialize operation. The result contains the deserialized data.</returns>
    abstract ReadSignalsAsync<'T when 'T : null> : unit -> Task<'T>

and ServerSentEventService (handler:ISendServerEvent) =
    member _.Handler = handler

    with
    interface IDatastarServerSentEventService with
        member this.Handler = this.Handler
        member this.MergeFragmentsAsync(fragment) = ServerSentEventGenerator.MergeFragments (this.Handler, fragment)
        member this.MergeFragmentsAsync(fragments, options) = ServerSentEventGenerator.MergeFragments (this.Handler, fragments, options.AsOptions)
        member this.MergeSignalsAsync(dataSignals) = ServerSentEventGenerator.MergeSignals(this.Handler, dataSignals)
        member this.MergeSignalsAsync(dataSignals, options:ServerSentEventMergeSignalsOptions): Task = ServerSentEventGenerator.MergeSignals (this.Handler, dataSignals, options.AsOptions)
        member this.RemoveFragmentsAsync(selector) = ServerSentEventGenerator.RemoveFragments(this.Handler, selector)
        member this.RemoveFragmentsAsync(selector, options: ServerSentEventRemoveFragmentsOptions) = ServerSentEventGenerator.RemoveFragments (this.Handler, selector, options.AsOptions)
        member this.RemoveSignalsAsync(paths) = ServerSentEventGenerator.RemoveSignals(this.Handler, paths)
        member this.RemoveSignalsAsync(paths, options) = ServerSentEventGenerator.RemoveSignals (this.Handler, paths, options.AsOptions)
        member this.ExecuteScriptAsync(script) = ServerSentEventGenerator.ExecuteScript(this.Handler, script)
        member this.ExecuteScriptAsync(script, options) = ServerSentEventGenerator.ExecuteScript (this.Handler, script, options.AsOptions)

and SignalsReaderService (handler:IReadSignals) =
    member _.Handler = handler

    with
    interface IDatastarSignalsReaderService with
        member this.Handler = this.Handler
        member this.ReadSignalsAsync() = task {
            let! signalsOpt = this.Handler.ReadSignals()
            let signals =
                match signalsOpt with
                | ValueSome signals -> signals
                | ValueNone -> Signals.empty
            return (signals |> Signals.value)
            }
        member this.ReadSignalsAsync<'T when 'T: null>() = task {
            let! signalsOpt = this.Handler.ReadSignals<'T>()
            let signals =
                match signalsOpt with
                | ValueSome signals -> signals
                | ValueNone -> null
            return signals
            }

and ServerSentEventMergeFragmentsOptions() =
    member val EventId = "" with get, set
    member val Retry = Consts.DefaultSseRetryDuration with get, set
    member val MergeMode = MergeFragmentsOptions.defaults.MergeMode with get, set
    member val Selector = "" with get, set
    member val UseViewTransition = MergeFragmentsOptions.defaults.UseViewTransition with get, set
    member internal this.AsOptions : MergeFragmentsOptions =
        { Selector = this.Selector |> ValueOption.fromEmptyString
          MergeMode = this.MergeMode
          UseViewTransition = this.UseViewTransition
          EventId = this.EventId |> ValueOption.fromEmptyString
          Retry = this.Retry }

and ServerSentEventMergeSignalsOptions() =
    member val OnlyIfMissing = Consts.DefaultMergeSignalsOnlyIfMissing with get, set
    member val EventId = "" with get, set
    member val Retry = Consts.DefaultSseRetryDuration with get, set
    member internal this.AsOptions : MergeSignalsOptions =
        { OnlyIfMissing = this.OnlyIfMissing
          EventId = this.EventId |> ValueOption.fromEmptyString
          Retry = this.Retry }

and ServerSentEventRemoveFragmentsOptions() =
    member val EventId = "" with get, set
    member val Retry = Consts.DefaultSseRetryDuration with get, set
    member val UseViewTransition = MergeFragmentsOptions.defaults.UseViewTransition with get, set
    member internal this.AsOptions : RemoveFragmentsOptions =
        { UseViewTransition = this.UseViewTransition
          EventId = this.EventId |> ValueOption.fromEmptyString
          Retry = this.Retry }

and ServerSentEventOptions() =
    member val EventId = "" with get, set
    member val Retry = Consts.DefaultSseRetryDuration with get, set
    member internal this.AsOptions : EventOptions =
        { EventId = this.EventId |> ValueOption.fromEmptyString
          Retry = this.Retry }

and ServerSentEventExecuteScriptOptions() =
    member val AutoRemove:bool = Consts.DefaultExecuteScriptAutoRemove with get, set
    member val Attributes:string[] = [| Consts.DefaultExecuteScriptAttributes |] with get, set
    member val EventId = "" with get, set
    member val Retry = Consts.DefaultSseRetryDuration with get, set
    member internal this.AsOptions : ExecuteScriptOptions =
        { AutoRemove = this.AutoRemove
          Attributes = this.Attributes
          EventId = this.EventId |> ValueOption.fromEmptyString
          Retry = this.Retry }
