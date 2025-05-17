# Datastar + dotnet

[![NuGet Version](https://img.shields.io/nuget/v/StarFederation.Datastar.FSharp.svg)](https://www.nuget.org/packages/StarFederation.Datastar.FSharp)

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

# F# Backend

```fsharp
backend
```