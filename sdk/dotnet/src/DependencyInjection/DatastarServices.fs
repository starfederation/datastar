namespace StarFederation.Datastar.DependencyInjection

open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open StarFederation.Datastar
open StarFederation.Datastar.Utility

type IServerSentEventService =
    abstract Handler : ISendServerEvent
    abstract MergeFragmentsAsync: fragment:string -> Task
    abstract MergeFragmentsAsync: fragment:string * options:ServerSentEventMergeFragmentsOptions -> Task
    abstract RemoveFragmentsAsync: selector:Selector -> Task
    abstract RemoveFragmentsAsync: selector:Selector * options:ServerSentEventRemoveFragmentsOptions -> Task
    abstract MergeSignalsAsync: dataSignals:Signals -> Task
    abstract MergeSignalsAsync: dataSignals:Signals * options:ServerSentEventMergeSignalsOptions -> Task
    abstract RemoveSignalsAsync: paths:SignalsPath seq -> Task
    abstract RemoveSignalsAsync: paths:SignalsPath seq * options:ServerSentEventOptions -> Task
    abstract ExecuteScriptAsync: script:string -> Task
    abstract ExecuteScriptAsync: script:string * options:ServerSentEventExecuteScriptOptions -> Task

and ISignals =
    abstract Handler : IReadSignals
    abstract ReadSignalsAsync : unit -> Task<Signals>
    abstract ReadSignalsAsync<'T when 'T:null> : unit -> Task<'T>

and ServerSentEventService (handler:ISendServerEvent) =
    new (httpContext:HttpContext) =
        ServerSentEventService (ServerSentEventHttpHandlers httpContext.Response)
    new (httpContextAccessor:IHttpContextAccessor) =
        ServerSentEventService httpContextAccessor.HttpContext

    member _.Handler = handler

    with
    interface IServerSentEventService with
        member this.Handler = this.Handler
        member this.MergeFragmentsAsync(fragment) = ServerSentEventGenerator.mergeFragments this.Handler fragment
        member this.MergeFragmentsAsync(fragment, options) = ServerSentEventGenerator.mergeFragmentsWithOptions options.AsOptions this.Handler fragment
        member this.MergeSignalsAsync(dataSignals) = ServerSentEventGenerator.mergeSignals this.Handler dataSignals
        member this.MergeSignalsAsync(dataSignals, options:ServerSentEventMergeSignalsOptions): Task = ServerSentEventGenerator.mergeSignalsWithOptions options.AsOptions this.Handler dataSignals
        member this.RemoveFragmentsAsync(selector) = ServerSentEventGenerator.removeFragments this.Handler selector
        member this.RemoveFragmentsAsync(selector, options: ServerSentEventRemoveFragmentsOptions) = ServerSentEventGenerator.removeFragmentsWithOptions options.AsOptions this.Handler selector
        member this.RemoveSignalsAsync(paths) = ServerSentEventGenerator.removeSignals this.Handler paths
        member this.RemoveSignalsAsync(paths, options) = ServerSentEventGenerator.removeSignalsWithOptions options.AsOptions this.Handler paths
        member this.ExecuteScriptAsync(script) = ServerSentEventGenerator.executeScript this.Handler script
        member this.ExecuteScriptAsync(script, options) = ServerSentEventGenerator.executeScriptWithOptions options.AsOptions this.Handler script

and ReadSignalsService (handler:IReadSignals) =
    new (httpContext:HttpContext) =
        ReadSignalsService (SignalsHttpHandlers httpContext.Request)
    new (httpContextAccessor:IHttpContextAccessor) =
        ReadSignalsService httpContextAccessor.HttpContext

    member _.Handler = handler

    with
    interface ISignals with
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
    member val SettleDuration = MergeFragmentsOptions.defaults.SettleDuration with get, set
    member val UseViewTransition = MergeFragmentsOptions.defaults.UseViewTransition with get, set
    member internal this.AsOptions : MergeFragmentsOptions =
        { Selector = this.Selector |> ValueOption.fromEmptyString
          MergeMode = this.MergeMode
          SettleDuration = this.SettleDuration
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
    member val SettleDuration = MergeFragmentsOptions.defaults.SettleDuration with get, set
    member val UseViewTransition = MergeFragmentsOptions.defaults.UseViewTransition with get, set
    member internal this.AsOptions : RemoveFragmentsOptions =
        { SettleDuration = this.SettleDuration
          UseViewTransition = this.UseViewTransition
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
