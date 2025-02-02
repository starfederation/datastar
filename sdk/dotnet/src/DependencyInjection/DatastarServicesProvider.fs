namespace StarFederation.Datastar.DependencyInjection

open System
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

module DatastarServicesProvider =

    let create (serviceCollection:IServiceCollection) =
        serviceCollection.AddHttpContextAccessor() |> ignore

        serviceCollection
            .AddScoped<IServerSentEventService>(fun (svcPvd:IServiceProvider) ->
                let httpContextAccessor = svcPvd.GetService<IHttpContextAccessor>()
                ServerSentEventService(httpContextAccessor)
                )
            .AddScoped<ISignals>(fun (svcPvd:IServiceProvider) ->
                let httpContextAccessor = svcPvd.GetService<IHttpContextAccessor>()
                ReadSignalsService(httpContextAccessor)
                )

[<Extension>]
type ServiceCollectionExtensionMethods() =

    [<Extension>]
    static member AddDatastar serviceCollection =
        DatastarServicesProvider.create serviceCollection