using Microsoft.AspNetCore.Components;
using Microsoft.AspNetCore.Components.Web;

namespace Starfederation.Datastar.MinimalBlazor.Extensions;

public class BlazorRenderer(HtmlRenderer htmlRenderer)
{
    public async Task<IResult> RenderComponent<TComponent>(Dictionary<string, object?>? parameters)
        where TComponent : IComponent
    {
        var html = await htmlRenderer.Dispatcher.InvokeAsync(async () =>
        {
            var parameterView = parameters != null
                ? ParameterView.FromDictionary(parameters)
                : ParameterView.Empty;
            var output = await htmlRenderer.RenderComponentAsync<TComponent>(parameterView);

            return output.ToHtmlString();
        });
        
        return Results.Content(html, "text/html");
    }
    
    public Task<string> RenderComponentString<TComponent>(Dictionary<string, object?>? parameters)
        where TComponent : IComponent
    {
        return htmlRenderer.Dispatcher.InvokeAsync(async () =>
        {
            var parameterView = parameters != null
                ? ParameterView.FromDictionary(parameters)
                : ParameterView.Empty;
            var output = await htmlRenderer.RenderComponentAsync<TComponent>(parameterView);

            return output.ToHtmlString();
        });
    }
}