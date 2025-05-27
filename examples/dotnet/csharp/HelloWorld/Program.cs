using System.Text.Json;
using System.Text.Json.Serialization;
using StarFederation.Datastar;
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

    public const string Message = "Hello, World!";

    public static void Main(string[] args)
    {
        WebApplicationBuilder builder = WebApplication.CreateBuilder(args);
        builder.Services.AddDatastar();

        WebApplication app = builder.Build();
        app.UseStaticFiles();

        app.MapGet("/hello-world", async (IDatastarServerSentEventService sse, IDatastarSignalsReaderService signals) =>
        {
            Signals mySignals = await signals.ReadSignalsAsync<Signals>();
            for (int index = 0; index < Message.Length; ++index)
            {
                await sse.MergeFragmentsAsync($"""<div id="message">{Message[..index]}</div>""");
                if (!char.IsWhiteSpace(Message[index]))
                {
                    await Task.Delay(TimeSpan.FromMilliseconds(mySignals.Delay.GetValueOrDefault(0)));
                }
            }
            await sse.MergeFragmentsAsync($"""<div id="message">{Message}</div>""");
        });

        app.Run();
    }
}