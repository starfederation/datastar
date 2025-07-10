using System.Diagnostics;
using System.Text.Json.Serialization;
using StarFederation.Datastar.DependencyInjection;

namespace HelloWorld;

public class Program
{
    public record Signals
    {
        [JsonPropertyName("delay")]
        [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        public float? Delay { get; set; } = null;
    }

    public const string Message = "Hello, world!";

    public static void Main(string[] args)
    {
        WebApplicationBuilder builder = WebApplication.CreateBuilder(args);
        builder.Services.AddDatastar();

        WebApplication app = builder.Build();
        app.UseStaticFiles();

        app.MapGet("/hello-world", async (IDatastarService datastarService) =>
        {
            Signals? mySignals = await datastarService.ReadSignalsAsync<Signals>();
            Debug.Assert(mySignals != null, nameof(mySignals) + " != null");

            for (int index = 0; index < Message.Length; ++index)
            {
                await datastarService.PatchElementsAsync($"""<div id="message">{Message[..index]}</div>""");
                if (!char.IsWhiteSpace(Message[index]))
                {
                    await Task.Delay(TimeSpan.FromMilliseconds(mySignals.Delay.GetValueOrDefault(0)));
                }
            }
            await datastarService.PatchElementsAsync($"""<div id="message">{Message}</div>""");
        });

        app.Run();
    }
}