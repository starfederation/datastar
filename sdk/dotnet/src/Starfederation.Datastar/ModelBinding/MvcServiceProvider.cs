using Microsoft.Extensions.DependencyInjection;
using Starfederation.Datastar.DependencyInjection;

namespace Starfederation.Datastar.ModelBinding
{
    /// <summary>
    /// Extension methods for IServiceCollection to add Datastar MVC services.
    /// </summary>
    public static class MvcServiceCollectionExtensions
    {
        /// <summary>
        /// Adds Datastar MVC services to the service collection.
        /// </summary>
        /// <param name="serviceCollection">The service collection.</param>
        /// <returns>The service collection.</returns>
        /// <exception cref="InvalidOperationException">Thrown when AddDatastar has not been called before AddDatastarMvc.</exception>
        public static IServiceCollection AddDatastarMvc(this IServiceCollection serviceCollection)
        {
            // Check that AddDatastar has been run
            if (!serviceCollection.Any(svc => svc.ServiceType == typeof(IDatastarSignalsReaderService)))
            {
                throw new InvalidOperationException("ServiceCollection.AddDatastarMvc requires that AddDatastar be added first");
            }

            // Add the SignalsModelBinderProvider
            serviceCollection
                .AddControllers(options => options.ModelBinderProviders.Insert(0, new SignalsModelBinderProvider()));
                
            return serviceCollection;
        }
    }
}
