namespace StarFederation.Datastar.DependencyInjection

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

[<Extension>]
type ServiceCollectionExtensionMethods() =
    [<Extension>]
    static member AddDatastar (serviceCollection:IServiceCollection) =
        serviceCollection
            .AddHttpContextAccessor()
            .AddScoped<IDatastarSignalsReaderService, SignalsReaderService>()
            .AddScoped<IDatastarServerSentEventService>(fun (svcPvd:IServiceProvider) -> ServerSentEventService(svcPvd.GetService<IHttpContextAccessor>(), additionalHeaders = Seq.empty))
