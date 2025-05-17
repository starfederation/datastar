using Microsoft.AspNetCore.Components;

namespace Starfederation.Datastar.Components;

/// <summary>
///     This co
/// </summary>
public class DatastarHeadOutlet : IComponent
{
    [Parameter]
    public bool UseEmbeddedDatastar { get; set; } = true;

    /// <inheritdoc />
    public void Attach(RenderHandle renderHandle)
    {
        renderHandle.Render(builder =>
        {
            if (UseEmbeddedDatastar)
            {
                builder.AddMarkupContent(0, """<script type="module" src="_content/Starfederation.Datastar/datastar.js"></script>""");
            }
        });
    }

    /// <inheritdoc />
    public Task SetParametersAsync(ParameterView parameters)
    {
        return Task.CompletedTask;
    }
}