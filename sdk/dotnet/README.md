# Datastar + dotnet

Real-time Hypermedia first Library and Framework for dotnet

# HTML Frontend

```html
<html lang="en">
  <head>
    <script type="module" defer src="https://cdn.jsdelivr.net/gh/starfederation/datastar/bundles/datastar.js"></script>
    <title>D* Demo</title>
  </head>
  <body>
    <main class="container" id="main" data-signals="{'input':'','output':''}">
      <button data-on-click="sse('/displayDate')">Display Date</button>
      <div id="target"></div>
      <input type="text" placeholder="input:" data-bind-input /><br />
      <span data-text="output.value"></span>
      <button data-on-click="sse('/changeOutput',{method:'post'})">Change Output</button>
    </main>
  </body>
</html>
```

# C# Backend

```csharp
using StarFederation.Datastar;
using StarFederation.Datastar.DependencyInjection;
using System.Text.Json;
using System.Text.Json.Serialization;
...
// define your signals
public record DatastarSignals : ISignals
{
    [JsonPropertyName("input")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string? Input { get; init; } = null;

    [JsonPropertyName("output")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string? Output { get; init; } = null;

    public string Serialize() => JsonSerializer.Serialize(this);
}
...
// add as an ASP Service
//  allows injection of IServerSentEventService, to respond to a request with a Datastar friendly ServerSentEvent
//                  and ISignals, to read the signals sent by the client
builder.Services.AddDatastar<DatastarSignals>();
...
app.UseStaticFiles();

// add endpoints
app.MapGet("/displayDate", async (IServerSentEventService sse) =>
{
    string today = DateTime.Now.ToString("%y-%M-%d %h:%m:%s");
    await sse.MergeFragments($"""<div id='target'><span id='date'><b>{today}</b><button data-on-click="sse('/removeDate')">Remove</button></span></div>""");
});
app.MapGet("/removeDate", async (IServerSentEventService sse) => { await sse.RemoveFragments("#date"); });
app.MapPost("/changeOutput", async (IServerSentEventService sse, ISignals signals) =>
{
    DatastarSignals dsSignals = (signals as DatastarSignals) ?? throw new InvalidCastException("Unknown ISignals passed");
    DatastarSignals newSignals = new() { Output = $"Your Input: {dsSignals.Input}" };
    await sse.MergeSignals(newSignals);
});
```
