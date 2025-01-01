
open System
open System.Text.Json
open System.Threading.Tasks
open FalcoServer
open Microsoft.Extensions.FileProviders
open Microsoft.AspNetCore.Builder
open StarFederation.Datastar
open StarFederation.Datastar.DependencyInjection
open global.Falco
open Falco.Routing
open Falco.HostBuilder
open Falco.Datastar

[<Struct>]
type Signals = { input: string; output: string; show: bool }
    with
    static member defaults = { input = ""; output = ""; show = false }
    interface ISignals with
        member this.Serialize() = JsonSerializer.Serialize(this)

type SignalsOutputPatch = { output: string }
    with
    interface ISignals with
        member this.Serialize() = JsonSerializer.Serialize(this)


[<EntryPoint>]
let main args =

    let ss = Examples.exampleEndpoints

    webHost args {

        use_middleware (fun appBuilder ->
            let webAppBuilder = appBuilder :?> WebApplication
            let defaultFileOptions = DefaultFilesOptions()
            defaultFileOptions.FileProvider <- new PhysicalFileProvider(System.IO.Path.Combine(webAppBuilder.Environment.ContentRootPath, "..", "Shared", "wwwroot"))
            let staticFileOptions = StaticFileOptions()
            staticFileOptions.FileProvider <- new PhysicalFileProvider(System.IO.Path.Combine(webAppBuilder.Environment.ContentRootPath, "..", "Shared", "wwwroot"))
            webAppBuilder.UseDefaultFiles(defaultFileOptions).UseStaticFiles(staticFileOptions)
            )

        // if you had one set of signals: add_service ServerSentEventServices.create<Signals>
        add_service ServerSentEventServiceProvider.createWithoutSignals

        endpoints ([
            get "/" (Response.redirectTemporarily "index.html")
            get "/title/{title:required}"
                (Request.mapRoute
                    (fun route -> route.GetString "title")
                    (fun title -> Response.ofMergePlainText $"""<span id="title">F# + {title}</span>""")
                    )
            get "/check_more_examples" (Response.ofMergeSignals { new ISignals with member this.Serialize() = @"{'more_examples':true}" })
            get "/goto_more_examples" (Response.dsRedirect "examples.html")
            get "/patch" (fun ctx -> task {
                let! signals =
                    Request.deserializeSignals
                        (fun str -> str |> ValueOption.map JsonSerializer.Deserialize<Signals> |> ValueOption.defaultValue Signals.defaults)
                        ctx
                do! Response.ofMergeSignals { output = $"Patched Output: {signals.input}" } ctx
                })
            get "/target" (fun ctx ->
                let today = DateTime.Now.ToString("%y-%M-%d %h:%m:%s");
                Response.ofMergePlainText $"""<div id='target'><span class="mr-2" id='date'><b>{today}</b><button class="btn" data-on-click="@sse('/removeDate')">Remove</button></span></div>""" ctx
                )
            get "/removeDate" (Response.ofRemoveFragments ("#date" |> Selector.create))
            get "/feed" (fun ctx -> task {
                let requestCanceled = ctx.RequestAborted
                while (requestCanceled.IsCancellationRequested |> not) do
                    let rand = Random.Shared.NextInt64(1000000000000000000L, 5999999999999999999L);
                    do! Response.ofMergePlainText $"<span id='feed'>{rand}</span>" ctx
                    do! Task.Delay(TimeSpan.FromSeconds 1, requestCanceled)
                })
        ]
        |> List.append Examples.exampleEndpoints
        )
    }
    0