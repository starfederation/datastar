namespace StarFederation.Datastar.DependencyInjection

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open StarFederation.Datastar
open StarFederation.Datastar.Utility



type IServerSentEventService =
    abstract Handler : IServerSentEventHandler
    abstract MergeFragments: fragment:string -> Task
    abstract MergeFragments: fragment:string * options:ServerSentEventMergeFragmentsOptions -> Task
    abstract RemoveFragments: selector:Selector -> Task
    abstract RemoveFragments: selector:Selector * options:ServerSentEventRemoveFragmentsOptions -> Task
    abstract MergeSignals: dataSignals:ISignals -> Task
    abstract MergeSignals: dataSignals:ISignals * options:ServerSentEventMergeSignalsOptions -> Task
    abstract RemoveSignals: paths:SignalsPath seq -> Task
    abstract RemoveSignals: paths:SignalsPath seq * options:ServerSentEventOptions -> Task
    abstract ExecuteScript: script:string -> Task
    abstract ExecuteScript: script:string * options:ServerSentEventExecuteScriptOptions -> Task

and ServerSentEventService (handler:IServerSentEventHandler) =
    new (httpContext:HttpContext) =
        ServerSentEventService (ServerSentEventHttpHandler httpContext)
    new (httpContextAccessor:IHttpContextAccessor) =
        ServerSentEventService httpContextAccessor.HttpContext

    member _.Handler = handler

    with
    interface IServerSentEventService with
        member this.Handler = this.Handler
        member this.MergeFragments(fragment) = ServerSentEventGenerator.mergeFragments this.Handler fragment
        member this.MergeFragments(fragment, options) = ServerSentEventGenerator.mergeFragmentsWithOptions options.AsOptions this.Handler fragment
        member this.MergeSignals(dataSignals) = ServerSentEventGenerator.mergeSignals this.Handler dataSignals
        member this.MergeSignals(dataSignals, options:ServerSentEventMergeSignalsOptions): Task = ServerSentEventGenerator.mergeSignalsWithOptions options.AsOptions this.Handler dataSignals
        member this.RemoveFragments(selector) = ServerSentEventGenerator.removeFragments this.Handler selector
        member this.RemoveFragments(selector: Selector, options: ServerSentEventRemoveFragmentsOptions) = ServerSentEventGenerator.removeFragmentsWithOptions options.AsOptions this.Handler selector
        member this.RemoveSignals(paths) = ServerSentEventGenerator.removeSignals this.Handler paths
        member this.RemoveSignals(paths, options) = ServerSentEventGenerator.removeSignalsWithOptions options.AsOptions this.Handler paths
        member this.ExecuteScript(script) = ServerSentEventGenerator.executeScript this.Handler script
        member this.ExecuteScript(script, options) = ServerSentEventGenerator.executeScriptWithOptions options.AsOptions this.Handler script

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
