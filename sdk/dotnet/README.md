# Datastar + dotnet

[![NuGet Version](https://img.shields.io/nuget/v/Starfederation.Datastar.svg)](https://www.nuget.org/packages/Starfederation.Datastar)

Real-time Hypermedia first Library and Framework for dotnet

# HTML Frontend

```html
<main class="container" id="main" data-signals="{'input':'','output':'what'}">
    <button data-on-click="@get('/displayDate')">Display Date</button>
    <div id="target"></div>
    <input type="text" placeholder="input:" data-bind-input/><br/>
    <span data-text-output></span>
    <button data-on-click="@post('/changeOutput')">Change Output</button>
</main>
```

# C# Backend

```csharp
using StarFederation.Datastar;
using StarFederation.Datastar.DependencyInjection;
using System.Text.Json;
using System.Text.Json.Serialization;

// add as an ASP Service
//  allows injection of IDatastarService, to respond to a request with a Datastar friendly ServerSentEvent
//  and to read the signals sent by the client
builder.Services.AddDatastar();

// displayDate - merging a fragment
app.MapGet("/displayDate", async (IDatastarService datastarService) =>
{
    string today = DateTime.Now.ToString("%y-%M-%d %h:%m:%s");
    await datastarService.PatchElementsAsync($"""<div id='target'><span id='date'><b>{today}</b><button data-on-click="@get('/removeDate')">Remove</button></span></div>""");
});

// removeDate - removing a fragment
app.MapGet("/removeDate", async (IDatastarService datastarService) => { await datastarService.RemoveFragmentAsync("#date"); });

public record MySignals {
    [JsonPropertyName("input")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string? Input { get; init; } = null;

    [JsonPropertyName("output")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string? Output { get; init; } = null;

    public string Serialize() => ...
}

// changeOutput - reads the signals, update the Output, and merge back
app.MapPost("/changeOutput", async (IDatastarService datastarService) => ...
{
    MySignals signals = await datastarService.ReadSignalsAsync<MySignals>();
    MySignals newSignals = new() { Output = $"Your Input: {signals.Input}" };
    await datastarService.PatchSignalsAsync(newSignals.Serialize());
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

public IActionResult Test_GetInnerPathed([FromSignals(Path = "myInner")] MySignals.InnerSignals myInnerOther) => ...

public IActionResult Test_GetInnerValues([FromSignals(Path = "myInner.myInnerString")] string myInnerStringOther, [FromSignals(Path = "myInner.myInnerInt")] int myInnerIntOther) => ...

```