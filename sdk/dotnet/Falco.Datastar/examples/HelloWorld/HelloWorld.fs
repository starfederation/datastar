open Falco
open Falco.Markup
open Falco.Routing
open Falco.Datastar
open Microsoft.AspNetCore.Builder
open StarFederation.Datastar.DependencyInjection

let handleIndex : HttpHandler =
    let html =
        Elem.html [] [
            Elem.head [] [
                Elem.script DatastarScript.cdn [] ]
            Elem.body [] [
                Text.h1 "Example: Hello World"
                Elem.button
                    [ Attr.id "swap"; Ds.get "/click" ]
                    [ Text.raw "Click Me" ] ] ]

    Response.ofHtml html

let handleClick : HttpHandler =
    let html =
        Elem.div [ Attr.id "swap" ] [ Text.h2 "Hello, World from the Server!" ]

    Response.ofMergeHtml html

let wApp = WebApplication.CreateBuilder()
wApp.Services.AddDatastar() |> ignore

let endpoints =
    [
        get "/" handleIndex
        get "/click" handleClick
    ]

wApp.Build()
    .UseFalco(endpoints)
    .Run()
