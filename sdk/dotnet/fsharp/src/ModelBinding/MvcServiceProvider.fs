namespace StarFederation.Datastar.FSharp.ModelBinding

open Microsoft.Extensions.DependencyInjection
open StarFederation.Datastar.FSharp.DependencyInjection
open System.Linq
open System.Runtime.CompilerServices

[<AbstractClass; Sealed; Extension>]
type ServiceCollectionExtensionMethods =
    [<Extension>]
    static member AddDatastarMvc (serviceCollection:IServiceCollection) =
        // check that AddDatastar has been run
        if not <| serviceCollection.Any(fun svc -> svc.ServiceType = typedefof<IDatastarSignalsReaderService>)
        then failwith "ServiceCollection.AddDatastarMvc requires that AddDatastar be added first"

        // Add the SignalsModelBinderProvider
        serviceCollection
            .AddControllers(_.ModelBinderProviders.Insert(0, SignalsModelBinderProvider()))
