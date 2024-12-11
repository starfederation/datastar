# Falco.Datastar

# THIS IS INCOMPLETE AND A WORK IN PROGRESS

[![NuGet Version](https://img.shields.io/nuget/v/Falco.Datastar.svg)](https://www.nuget.org/packages/Falco.Datastar)
[![build](https://github.com/pimbrouwers/Falco.Datastar/actions/workflows/build.yml/badge.svg)](https://github.com/pimbrouwers/Falco.Datastar/actions/workflows/build.yml)

```fsharp
open Falco.Markup
open Falco.Datastar

let demo =
    Elem.button
        [ Ds.onClick (Get "/click-me") ]
        [ Text.raw "Reset" ]
```

[Falco.Datastar](https://github.com/pimbrouwers/Falco.Datastar) brings type-safe [Datastar](https://data-star.dev/) support to [Falco](https://github.com/pimbrouwers/Falco). It provides a complete mapping of all attributes, typed request data and ready-made response modifiers.

## Key Features

- Idiomatic mapping of `Datastar` attributes (e.g., `data-bind`, `data-signals`, `data-model`, etc.).
- Typed access to Datastar request headers.
- Prepared response modifiers for common use-cases

## Design Goals

- Create a self-documenting way to integrate Datastar into Falco applications.
- Match the specification of Datastar as closely as possible, ideally one-to-one.
- Provide type safety without over-abstracting.

## Getting Started

This guide assumes you have a [Falco](https://github.com/pimbrouwers/Falco) project setup. If you don't, you can create a new Falco project using the following commands. The full code for this guide can be found in the [Hello World example](examples/HelloWorld/).

```shell
> dotnet new web -lang F# -o HelloWorld
> cd HelloWorldApp
```

Install the nuget package:

```shell
> dotnet add package Falco
> dotnet add package Falco.Datastar
```

Remove any `*.fs` files created automatically, create a new file named `Program.fs` and set the contents to the following:

```fsharp
open Falco
open Falco.Datastar
open Falco.Markup
open Falco.Routing
open Microsoft.AspNetCore.Builder
open System.Text.Json

[<Struct>]
type Signals =
    { text: string
      show: bool }
    with
    interface ISignals with
        member this.Serialize () = JsonSerialize.Serialize(this)

let bldr = WebApplication.CreateBuilder()
let wapp = bldr.Build()
let signals = { show = true }

let endpoints = [ ]

wapp.UseFalco(endpoints).Run()
```

Now, let's incorporate Datastar into our Falco application. First we'll define a simple route that returns a button that, when clicked, will swap the inner HTML of a target element with the response from a GET request.

```fsharp
let handleIndex : HttpHandler =
    let html =
        Elem.html [] [
            Elem.head [] [
                Elem.script DatastarScript.cdn [] ]
            Elem.body [ Ds.signals signals ] [
                Elem.div [ Ds.show "show" ] [ Text.h1 "Example: Hello World" ]
                Elem.button
                    [ Ds.onClick (Script "$show=!$show") ]
                    [ Text.raw "Click Me" ] ] ]

    Response.ofHtml html
```

Next, we'll define a handler for the click event that will return HTML from the server to replace the outer HTML of the button.

```fsharp
let handleClick : HttpHandler =
    let html = Text.h2 "Hello, World from the Server!"

    Response.ofHtml html
```

And lastly, we'll make Falco aware of these routes by adding them to the `endpoints` list.

```fsharp
let endpoints =
    [
        get "/" handleIndex
        get "/click" handleClick
    ]
```

Save the file and run the application:

```shell
> dotnet run
```

Navigate to `https://localhost:5001` in your browser and click the button. You should see the text "Hello, World from the Server!" appear in place of the button.

## Datastar Attributes

### `data-on-click="@[get|post|put|patch|delete](...)`

```fsharp
Elem.button [ Ds.put "/messages" ] [ Text.raw "Click me to PUT to Messages" ]
```

## Kudos

Big thanks and kudos to [@SpiralOSS](https://github.com/SpiralOSS) for their collaboration in starting this repo!

## Find a bug?

There's an [issue](https://github.com/pimbrouwers/Falco.Datastar/issues) for that.

## License

Built with ♥ by [Pim Brouwers](https://github.com/pimbrouwers) and [Greg H](https://github.com/SpiralOSS). Licensed under [Apache License 2.0](https://github.com/pimbrouwers/Falco.Datastar/blob/master/LICENSE).
