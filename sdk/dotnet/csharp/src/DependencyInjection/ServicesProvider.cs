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
            .AddScoped<IDatastarService>(svcPvd =>
            {
                IHttpContextAccessor? httpContextAccessor = svcPvd.GetService<IHttpContextAccessor>();
                Core.ServerSentEventGenerator serverSentEventGenerator = new(httpContextAccessor);
                return new DatastarService(serverSentEventGenerator);
            });
        return serviceCollection;
    }
}
