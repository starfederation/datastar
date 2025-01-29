using System.Text.Json;
using System.Text.Json.Serialization;
using StarFederation.Datastar;
using StarFederation.Datastar.DependencyInjection;

namespace HelloWorld;

public class Program
{
    public record Signals : ISignals
    {
        [JsonPropertyName("delay")]
        [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        public float? Delay { get; init; } = null;

        public string Serialize() => JsonSerializer.Serialize(this);
    }

    public const string Message = "Hello, World!";

    public static void Main(string[] args)
    {
        WebApplicationBuilder builder = WebApplication.CreateBuilder(args);
        builder.Services.AddDatastar<Signals>();

        WebApplication app = builder.Build();
        app.UseStaticFiles();

        app.MapGet("/hello-world", async (IServerSentEventService sse, ISignals signals) =>
        {
            Signals signal = signals as Signals ?? new Signals();
            for (int index = 0; index < Message.Length; ++index)
            {
                await sse.MergeFragments($"""<div id="message">{Message[..index]}</div>""");
                if (!char.IsWhiteSpace(Message[index]))
                {
                    await Task.Delay(TimeSpan.FromMilliseconds(signal.Delay.GetValueOrDefault(0)));
                }
            }
            await sse.MergeFragments($"""<div id="message">{Message}</div>""");
        });

        app.Run();
    }
}