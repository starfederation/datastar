namespace StarFederation.Datastar.DependencyInjection

open System.Runtime.CompilerServices
open Microsoft.Extensions.DependencyInjection

[<Extension>]
type ServiceCollectionExtensionMethods() =
    [<Extension>]
    static member AddDatastar (serviceCollection:IServiceCollection) =
        serviceCollection
            .AddHttpContextAccessor()
            .AddScoped<IDatastarServerSentEventService, ServerSentEventService>()
            .AddScoped<IDatastarSignalsReaderService, SignalsReaderService>()