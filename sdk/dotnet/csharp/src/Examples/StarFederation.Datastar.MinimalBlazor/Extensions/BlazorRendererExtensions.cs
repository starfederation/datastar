using Microsoft.AspNetCore.Components;
using Microsoft.AspNetCore.Components.Web;
using IComponent = System.ComponentModel.IComponent;

namespace StarFederation.Datastar.MinimalBlazor.Extensions;

public static class BlazorRendererExtensions
{
    public static void AddBlazorRenderer(this IServiceCollection services)
    {
        //Check if ILoggerFactory is registered
        if (services.All(svc => svc.ServiceType != typeof(ILoggerFactory)))
        {
            throw new InvalidOperationException("ILoggerFactory must be registered before BlazorRenderer.");
        }
        
        services.AddSingleton<HtmlRenderer>();
        services.AddSingleton<BlazorRenderer>();
    }
}