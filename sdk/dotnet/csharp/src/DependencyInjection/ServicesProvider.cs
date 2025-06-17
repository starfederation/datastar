using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Core = StarFederation.Datastar.FSharp;

namespace StarFederation.Datastar.DependencyInjection;

public static class ServiceCollectionExtensionMethods
{
    public static IServiceCollection AddDatastar(this IServiceCollection serviceCollection)
    {
        serviceCollection
            .AddHttpContextAccessor()
            .AddScoped<IDatastarSignalsReaderService>(svcPvd =>
            {
                IHttpContextAccessor? httpContextAccessor = svcPvd.GetService<IHttpContextAccessor>();
                Core.IReadSignals signalsHttpHandler = new Core.SignalsHttpHandler(httpContextAccessor!.HttpContext!.Request);
                return new SignalsReaderService(signalsHttpHandler);
            })
            .AddScoped<IDatastarServerSentEventService>(svcPvd =>
            {
                IHttpContextAccessor? httpContextAccessor = svcPvd.GetService<IHttpContextAccessor>();
                Core.ISendServerEvent sseHttpHandler = new Core.ServerSentEventHttpHandler(httpContextAccessor!.HttpContext!.Response);
                return new ServerSentEventService(sseHttpHandler);
            });
        return serviceCollection;
    }
}