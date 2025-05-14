using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;

namespace Starfederation.Datastar.DependencyInjection
{
    /// <summary>
    /// Extension methods for IServiceCollection to add Datastar services.
    /// </summary>
    public static class ServiceCollectionExtensions
    {
        /// <summary>
        /// Adds Datastar services to the service collection.
        /// </summary>
        /// <param name="serviceCollection">The service collection.</param>
        /// <returns>The service collection.</returns>
        public static IServiceCollection AddDatastar(this IServiceCollection serviceCollection)
        {
            return serviceCollection
                .AddHttpContextAccessor()
                .AddScoped<IDatastarSignalsReaderService>(serviceProvider =>
                {
                    var httpContextAccessor = serviceProvider.GetRequiredService<IHttpContextAccessor>();
                    var signalsHttpHandler = new SignalsHttpHandlers(httpContextAccessor.HttpContext!.Request);
                    return new SignalsReaderService(signalsHttpHandler);
                })
                .AddScoped<IDatastarServerSentEventService>(serviceProvider =>
                {
                    var httpContextAccessor = serviceProvider.GetRequiredService<IHttpContextAccessor>();
                    
                    var sseHttpHandler = new ServerSentEventHttpHandlers(httpContextAccessor.HttpContext!.Response);
                    
                    // Start the response
                    sseHttpHandler.StartResponse();
                    
                    // You can add headers here if needed
                    // sseHttpHandler.AddHeader("X-Accel-Buffering", "no");
                    
                    return new ServerSentEventService(sseHttpHandler);
                });
        }
    }
}
