using Microsoft.AspNetCore.Mvc;
using StarFederation.Datastar.MinimalBlazor.Extensions;
using Index = StarFederation.Datastar.MinimalBlazor.Components.Index;

var builder = WebApplication.CreateBuilder(args);

builder.Services.AddOpenApi();

builder.Services.AddBlazorRenderer();

var app = builder.Build();

if (app.Environment.IsDevelopment())
{
    app.UseHttpsRedirection();
}

app.MapOpenApi();
app.MapStaticAssets();

app.MapGet("/", async ([FromServices] BlazorRenderer blazorRenderer) =>
{
    var result = await blazorRenderer.RenderComponent<Index>(new Dictionary<string, object?>
    {
        { "Title", "Datastar Minimal Blazor example" },
        { "Description", "Hello there, this is the minimal api + blazor demo;" }
    });

    return result;
});

app.Run();

//TODO remove this program reference for unit testing as it will not be needed in .net 10
namespace StarFederation.Datastar.MinimalBlazor
{
    public partial class Program;
}