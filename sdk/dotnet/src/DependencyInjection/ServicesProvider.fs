namespace StarFederation.Datastar.DependencyInjection

open System
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open StarFederation.Datastar

[<Extension>]
type ServiceCollectionExtensionMethods() =
    [<Extension>]
    static member AddDatastar (serviceCollection:IServiceCollection) =
        serviceCollection
            .AddHttpContextAccessor()
            .AddScoped<IDatastarSignalsReaderService>(fun (svcPvd:IServiceProvider) ->
                let httpContextAccessor = svcPvd.GetService<IHttpContextAccessor>()
                let signalsHttpHandler = SignalsHttpHandlers httpContextAccessor.HttpContext.Request
                SignalsReaderService signalsHttpHandler
                )
            .AddScoped<IDatastarServerSentEventService>(fun (svcPvd:IServiceProvider) ->
                let httpContextAccessor = svcPvd.GetService<IHttpContextAccessor>()
                let sseHttpHandler = ServerSentEventHttpHandlers httpContextAccessor.HttpContext.Response
                sseHttpHandler.StartResponse() |> ignore
                //sseHttpHandler.AddHeader ("X-Accel-Buffering", "no")  // an example of adding a header
                ServerSentEventService sseHttpHandler
                )
