namespace Falco.Datastar

open Falco.Markup
open Microsoft.AspNetCore.Http
open StarFederation.Datastar
open StarFederation.Datastar.DependencyInjection
open StarFederation.Datastar.Scripts

[<RequireQualifiedAccess>]
module Response =
    let ofMergePlainTextWithOptions options fragments= (fun (ctx:HttpContext) ->
        let sseService = ctx.RequestServices.GetService(typedefof<IServerSentEventService>) :?> IServerSentEventService
        ServerSentEventGenerator.mergeFragmentsWithOptions options sseService.Handler fragments
        )
    let ofMergePlainText fragments = ofMergePlainTextWithOptions MergeFragmentsOptions.defaults fragments

    let ofMergeHtmlWithOptions options fragments= (fun (ctx:HttpContext) ->
        let fragments = renderHtml fragments
        let sseService = ctx.RequestServices.GetService(typedefof<IServerSentEventService>) :?> IServerSentEventService
        ServerSentEventGenerator.mergeFragmentsWithOptions options sseService.Handler fragments
        )
    let ofMergeHtml fragments = ofMergeHtmlWithOptions MergeFragmentsOptions.defaults fragments

    let ofRemoveFragmentsWithOptions options selector = (fun (ctx:HttpContext) ->
        let sseService = ctx.RequestServices.GetService(typedefof<IServerSentEventService>) :?> IServerSentEventService
        ServerSentEventGenerator.removeFragmentsWithOptions options sseService.Handler selector
        )
    let ofRemoveFragments selector = ofRemoveFragmentsWithOptions RemoveFragmentsOptions.defaults selector

    let ofMergeSignalsWithOptions options signals = (fun (ctx:HttpContext) ->
        let sseService = ctx.RequestServices.GetService(typedefof<IServerSentEventService>) :?> IServerSentEventService
        ServerSentEventGenerator.mergeSignalsWithOptions options sseService.Handler signals
        )
    let ofMergeSignals signals = ofMergeSignalsWithOptions MergeSignalsOptions.defaults signals

    let ofRemoveSignalsWithOptions options paths = (fun (ctx:HttpContext) ->
        let sseService = ctx.RequestServices.GetService(typedefof<IServerSentEventService>) :?> IServerSentEventService
        ServerSentEventGenerator.removeSignalsWithOptions options sseService.Handler paths
        )
    let ofRemoveSignals paths = ofRemoveSignalsWithOptions EventOptions.defaults paths

    let ofExecuteScriptWithOptions options script = (fun (ctx:HttpContext) ->
        let sseService = ctx.RequestServices.GetService(typedefof<IServerSentEventService>) :?> IServerSentEventService
        ServerSentEventGenerator.executeScriptWithOptions options sseService.Handler script
        )
    let ofExecuteScript script = ofExecuteScriptWithOptions script

    let dsRedirectWithOptions options url = (fun (ctx:HttpContext) ->
        let sseService = ctx.RequestServices.GetService(typedefof<IServerSentEventService>) :?> IServerSentEventService
        Redirect.redirectWithOptions options sseService.Handler url
        )
    let dsRedirect url = dsRedirectWithOptions EventOptions.defaults url

    let dsConsoleActionWithOptions options consoleAction = (fun (ctx:HttpContext) ->
        let sseService = ctx.RequestServices.GetService(typedefof<IServerSentEventService>) :?> IServerSentEventService
        BrowserConsole.consoleActionWithOptions options sseService.Handler consoleAction
        )
    let dsConsoleAction consoleAction = dsConsoleActionWithOptions EventOptions.defaults consoleAction
