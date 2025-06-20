namespace HelloWorld
#nowarn "20"
open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open StarFederation.Datastar.FSharp

module Program =
    [<CLIMutable>]
    type MySignals = { delay:float }

    let [<Literal>] Message = "Hello, world!"

    [<EntryPoint>]
    let main args =

        let builder = WebApplication.CreateBuilder(args)
        builder.Services.AddHttpContextAccessor();
        let app = builder.Build()
        app.UseStaticFiles()

        app.MapGet("/hello-world", Func<IHttpContextAccessor, Task>(fun ctx -> task {
            let sseHandler : ISendServerEvent = ServerSentEventHttpHandler ctx.HttpContext.Response
            do! sseHandler.StartServerEventStream()

            let signalsHandler : IReadSignals = SignalsHttpHandler ctx.HttpContext.Request
            let! signals = signalsHandler.ReadSignalsAsync<MySignals>()
            let delayMs = (signals |> ValueOption.map _.delay |> ValueOption.defaultValue 1000)

            [0 .. (Message.Length - 1)]
            |> Seq.map (fun length -> Message[0..length])
            |> Seq.map (fun message -> $"""<div id="message">{message}</div>""")
            |> Seq.map ServerSentEventGenerator.PatchElements
            |> Seq.map (fun sse -> async {
                do! sse |> sseHandler.SendServerEvent |> Async.AwaitTask
                do! Async.Sleep(TimeSpan.FromMilliseconds(delayMs)) })
            |> Async.Sequential
            |> Async.RunSynchronously
            }))

        app.Run()

        0