using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using StarFederation.Datastar.Interfaces;
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
        //Check if HttpContextAccessor is already registered, if not, register it
        if (services.All(svc => svc.ServiceType != typeof(IHttpContextAccessor)))
        {
            services.AddHttpContextAccessor();
        }
        
        return services
              .AddScoped<IDatastarSignalsReaderService>()
              .AddScoped<IDatastarServerSentEventService>();
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