namespace StarFederation.Datastar.DependencyInjection

open System
open System.Runtime.CompilerServices
open System.Text.Json
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open StarFederation.Datastar

module ServerSentEventServiceProvider =

    let createWithoutSignals (serviceCollection:IServiceCollection) =
        serviceCollection.AddHttpContextAccessor() |> ignore

        serviceCollection.AddScoped<IServerSentEventService>(fun (svcPvd:IServiceProvider) ->
            let httpContextAccessor : IHttpContextAccessor|null = svcPvd.GetService<IHttpContextAccessor>()
            ServerSentEventService(httpContextAccessor)
            )

    let createWithCustomDeserializer<'T when 'T :> ISignals> (signalsDeserializer:string -> 'T) (serviceCollection:IServiceCollection) =
        serviceCollection.AddHttpContextAccessor() |> ignore

        serviceCollection
            .AddScoped<IServerSentEventService>(fun (svcPvd:IServiceProvider) ->
                let httpContext = svcPvd.GetService<IHttpContextAccessor>()
                ServerSentEventService(httpContext)
                )
            .AddSingleton<IDeserializeSignals>(fun (svcPvd:IServiceProvider) ->
                { new IDeserializeSignals with
                     member this.DeserializeSignals(serializedSignals) =
                         signalsDeserializer serializedSignals}
                )
            .AddScoped<ISignals>(fun (svcPvd:IServiceProvider) ->
                let httpContextAccessor = svcPvd.GetService<IHttpContextAccessor>()
                let signalsDeserializer = svcPvd.GetService<IDeserializeSignals>()
                let rawSignals = ServerSentEventHttpHandler.readRawSignals(httpContextAccessor.HttpContext.Request).GetAwaiter().GetResult()
                match rawSignals with
                | ValueSome rawSignals' -> signalsDeserializer.DeserializeSignals(rawSignals')
                | ValueNone -> signalsDeserializer.DeserializeSignals("")
                )

    let create<'T when 'T :> ISignals|null> (serviceCollection:IServiceCollection) =
        createWithCustomDeserializer JsonSerializer.Deserialize<'T> serviceCollection

[<Extension>]
type ServiceCollectionExtensionMethods() =

    [<Extension>]
    static member AddDatastar serviceCollection =
        ServerSentEventServiceProvider.createWithoutSignals serviceCollection

    [<Extension>]
    static member AddDatastar<'T when 'T :> ISignals|null> serviceCollection =
        ServerSentEventServiceProvider.create<'T> serviceCollection

    [<Extension>]
    static member AddDatastar<'T when 'T :> ISignals> (serviceCollection:ServiceCollection, signalsCustomDeserializer:Func<string, 'T>) =
        ServerSentEventServiceProvider.createWithCustomDeserializer<'T> signalsCustomDeserializer.Invoke serviceCollection
