using System.Text.Json;
using System.Text.Json.Serialization;
using Microsoft.Extensions.FileProviders;
using StarFederation.Datastar;
using StarFederation.Datastar.DependencyInjection;

namespace CsharpAspServer;

public record Signals : ISignals
{
    [JsonPropertyName("input")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string? Input { get; init; } = null;

    [JsonPropertyName("output")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string? Output { get; init; } = null;

    [JsonPropertyName("show_rocket")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public bool? ShowRocket { get; init; } = null;

    public string Serialize() => JsonSerializer.Serialize(this);
}

public static class Program
{
    public static void Main(string[] args)
    {
        WebApplicationBuilder builder = WebApplication.CreateBuilder(args);
        builder.Services.AddDatastar<Signals>();

        WebApplication app = builder.Build();
        app.UseDefaultFiles(new DefaultFilesOptions
        {
            FileProvider = new PhysicalFileProvider(Path.Combine(app.Environment.ContentRootPath, "..", "Shared", "wwwroot")),
        });
        app.UseStaticFiles(new StaticFileOptions
        {
            FileProvider = new PhysicalFileProvider(Path.Combine(app.Environment.ContentRootPath, "..", "Shared", "wwwroot")),
        });

        app.MapGet("/", (context) =>
        {
            context.Response.Redirect("index.html");
            return Task.CompletedTask;
        });
        app.MapGet("/title/{title:required}", (string title, IServerSentEventService sseService) => sseService.MergeFragments($"""<span id="title">C# + {title}</span>"""));
        app.MapGet("/check_more_examples", () => Task.CompletedTask); // do nothing
        app.MapGet("/patch", async (IServerSentEventService sseService, ISignals dsSignals) =>
        {
            Signals signals = (dsSignals as Signals) ?? throw new InvalidCastException("Unknown signals passed");
            Signals mergeSignals = new() { Output = $"Patched Output: {signals.Input}" };
            await sseService.MergeSignals(mergeSignals);
        });
        app.MapGet("/target", async (IServerSentEventService sseService) =>
        {
            string today = DateTime.Now.ToString("%y-%M-%d %h:%m:%s");
            await sseService.MergeFragments(
                $"""<div id='target'><span id='date'><b>{today}</b><button class="btn mx-2" data-on-click="@get('/removeDate')">Remove</button></span></div>""");
        });
        app.MapGet("/removeDate", (IServerSentEventService sseService) => sseService.RemoveFragments("#date"));
        app.MapGet("/feed", async (IServerSentEventService sseService, CancellationToken ct) =>
        {
            while (!ct.IsCancellationRequested)
            {
                long rand = Random.Shared.NextInt64(1000000000000000000, 5999999999999999999);
                await sseService.MergeFragments($"<span id='feed'>{rand}</span>");
                await Task.Delay(TimeSpan.FromSeconds(1), ct);
            }
        });

        app.Run();
    }
}
