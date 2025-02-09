# Datastar + dotnet

Real-time Hypermedia first Library and Framework for dotnet

# HTML Frontend

```html
<html lang="en">
<head>
    <script type="module"
            src="https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-beta.3/bundles/datastar.js"></script>
    <title>D* Demo</title>
</head>
<body>
<main class="container" id="main" data-signals="{'input':'','output':'what'}">
    <button data-on-click="@get('/displayDate')">Display Date</button>
    <div id="target"></div>
    <input type="text" placeholder="input:" data-bind-input/><br/>
    <span data-text-output></span>
    <button data-on-click="@post('/changeOutput')">Change Output</button>
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

// add as an ASP Service
//  allows injection of IServerSentEventService, to respond to a request with a Datastar friendly ServerSentEvent
//  and ISignals, to read the signals sent by the client
builder.Services.AddDatastar();

// displayDate - merging a fragment
app.MapGet("/displayDate", async (IServerSentEventService sse) =>
{
    string today = DateTime.Now.ToString("%y-%M-%d %h:%m:%s");
    await sse.MergeFragmentsAsync($"""<div id='target'><span id='date'><b>{today}</b><button data-on-click="@get('/removeDate')">Remove</button></span></div>""");
});

// removeDate - removing a fragment
app.MapGet("/removeDate", async (IServerSentEventService sse) => { await sse.RemoveFragmentsAsync("#date"); });

public record Signals {
    [JsonPropertyName("input")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string? Input { get; init; } = null;

    [JsonPropertyName("output")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string? Output { get; init; } = null;

    public string Serialize() => ...
}

// changeOutput - reads the signals, update the Output, and merge back
app.MapPost("/changeOutput", async (IDatastarServerSentEventService sse, IDatastarSignalsReaderService dsSignals) => ...
{
    Signals signals = await dsSignals.ReadSignalsAsync<Signals>();
    Signals newSignals = new() { Output = $"Your Input: {signals.Input}" };
    await sse.MergeSignalsAsync(newSignals.Serialize());
});
```

# Model Binding

```csharp
public class MySignals {
    public string myString { get; set; } = "";
    public int myInt { get; set; } = 0;
    public InnerSignals myInner { get; set; } = new();

    public class InnerSignals {
        public string myInnerString { get; set; } = "";
        public int myInnerInt { get; set; } = 0;
    }
}

public IActionResult Test_GetSignals([FromSignals] MySignals signals) => ...

public IActionResult Test_GetValues([FromSignals] string myString, [FromSignals] int myInt) => ...

public IActionResult Test_GetInner([FromSignals] MySignals.InnerSignals myInner) => ...

public IActionResult Test_GetInnerPathed([FromSignals(Path = "myInner")] MySignals.InnerSignals myInnerOther) => ...

public IActionResult Test_GetInnerValues([FromSignals(Path = "myInner.myInnerString")] string myInnerStringOther, [FromSignals(Path = "myInner.myInnerInt")] int myInnerIntOther) => ...

```