using Microsoft.Extensions.DependencyInjection;
using StarFederation.Datastar.DependencyInjection;

namespace StarFederation.Datastar.ModelBinding;

public static class ServiceCollectionExtensionMethods
{
    public static IServiceCollection AddDatastarMvc(this IServiceCollection serviceCollection)
    {
        // ReSharper disable once SuspiciousTypeConversion.Global
        if (!serviceCollection.Any(_ => _.ServiceType == typeof(IDatastarSignalsReaderService)))
        {
            throw new Exception($"{nameof(AddDatastarMvc)} requires that {nameof(StarFederation.Datastar.DependencyInjection.ServiceCollectionExtensionMethods.AddDatastar)} is added first");
        }

        serviceCollection.AddControllers(options => options.ModelBinderProviders.Insert(0, new SignalsModelBinderProvider()));
        return serviceCollection;
    }
}
