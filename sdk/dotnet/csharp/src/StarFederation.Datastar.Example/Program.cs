using StarFederation.Datastar;
using StarFederation.Datastar.Example.Components;
using StarFederation.Datastar.Extensions;

var builder = WebApplication.CreateBuilder(args);

// Add services to the container.
builder.Services.AddRazorComponents();

var app = builder.Build();

// Configure the HTTP request pipeline.
if (app.Environment.IsProduction())
{
    app.UseExceptionHandler("/Error", createScopeForErrors: true);
    // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
    app.UseHsts();
}

if (app.Environment.IsDevelopment())
{
    app.UseHttpsRedirection();
}

app.UseAntiforgery();

app.MapStaticAssets();
app.MapRazorComponents<App>();

app.MapPost("/increase", async context =>
{
    var signals = await context.Request.ReadSignalsAsJsonNodeAsync();
    if (signals == null)
    {
        context.Response.StatusCode = 400;
        return;
    }
    var count = signals["serverCounter"]?.GetValue<int>() ?? 0;
    count++;
    
    var signalsResponse = new
    {
        serverCounter = count,
    };

    await context.Response.MergeSignals(signalsResponse);
});

app.MapPost("/decrease", async context =>
{
    var signals = await context.Request.ReadSignalsAsJsonNodeAsync();
    if (signals == null)
    {
        context.Response.StatusCode = 400;
        return;
    }
    var count = signals["serverCounter"]?.GetValue<int>() ?? 0;
    count--;
    
    var signalsResponse = new
    {
        serverCounter = count,
    };

    await context.Response.MergeSignals(signalsResponse);
});

app.Run();

//TODO remove this program reference for unit testing as it will not be needed in .net 10
namespace StarFederation.Datastar.Example
{
    public partial class Program;
}