using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using StarFederation.Datastar.Handlers;
using StarFederation.Datastar.ModelBinding;
using StarFederation.Datastar.Services;

namespace StarFederation.Datastar.Extensions;

/// <summary>
///     Extension methods for IServiceCollection to add Datastar services.
/// </summary>
public static class ServiceCollectionExtensions
{
    /// <summary>
    ///     Adds Datastar services to the service collection.
    /// </summary>
    /// <param name="services">The service collection.</param>
    /// <returns>The service collection.</returns>
    public static IServiceCollection AddDatastar(this IServiceCollection services)
    {
        return services
              .AddHttpContextAccessor()
              .AddScoped<IDatastarSignalsReaderService>(serviceProvider =>
               {
                   var httpContextAccessor = serviceProvider.GetRequiredService<IHttpContextAccessor>();
                   var signalsHttpHandler = new SignalsReaderHttpHandlers(httpContextAccessor.HttpContext!.Request);
                   return new SignalsReaderService(signalsHttpHandler);
               })
              .AddScoped<IDatastarServerSentEventService>(serviceProvider =>
               {
                   var httpContextAccessor = serviceProvider.GetRequiredService<IHttpContextAccessor>();

                   var sseHttpHandler = new ServerSentEventSenderHttpHandlers(httpContextAccessor.HttpContext!.Response);

                   // Start the response
                   sseHttpHandler.StartResponse();

                   // You can add headers here if needed
                   // sseHttpHandler.AddHeader("X-Accel-Buffering", "no");

                   var signalsReader = new SignalsReaderHttpHandlers(httpContextAccessor.HttpContext.Request);

                   return new ServerSentEventService(sseHttpHandler, signalsReader);
               });
    }

    /// <summary>
    ///     Adds Datastar MVC services to the service collection.
    /// </summary>
    /// <param name="services">The service collection.</param>
    /// <returns>The service collection.</returns>
    /// <exception cref="InvalidOperationException">Thrown when AddDatastar has not been called before AddDatastarMvc.</exception>
    public static IServiceCollection AddDatastarMvc(this IServiceCollection services)
    {
        // Check that AddDatastar has been run
        if (services.All(svc => svc.ServiceType != typeof(IDatastarSignalsReaderService)))
        {
            //Add datastar as it is not registered
            services.AddDatastar();
        }

        // Add the SignalsModelBinderProvider
        services
           .AddControllers(options => options.ModelBinderProviders.Insert(0, new SignalsModelBinderProvider()));

        return services;
    }
}