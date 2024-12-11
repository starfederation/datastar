open Falco
open Falco.Markup
open Falco.Routing
open Falco.Datastar
open Microsoft.AspNetCore.Builder
open StarFederation.Datastar.DependencyInjection

module View =
    let template content =
        Elem.html [ Attr.lang "en" ] [
            Elem.head [] [
                Elem.script DatastarScript.cdn [] ]
            Elem.body []
                content ]

    module Components =
        let clicker =
            Elem.button
                [ Attr.id "click"; Ds.get "/click" ]
                [ Text.raw "Click Me" ]

        let resetter =
            Elem.div [ Attr.id "click" ] [
                Text.h2 "Way to go! You clicked it!"
                Elem.br []
                Elem.button
                    [ Ds.get "/reset" ]
                    [ Text.raw "Reset" ] ]

module App =
    let handleIndex : HttpHandler =
        let html =
            View.template [
                Text.h1 "Example: Click & Swap"
                View.Components.clicker ]

        Response.ofHtml html

    let handleClick : HttpHandler =
        Response.ofMergeHtml View.Components.resetter

    let handleReset : HttpHandler =
        Response.ofMergeHtml View.Components.clicker

[<EntryPoint>]
let main args =
    let wApp = WebApplication.CreateBuilder()
    wApp.Services.AddDatastar() |> ignore

    let endpoints =
        [
            get "/" App.handleIndex
            get "/click" App.handleClick
            get "/reset" App.handleReset
        ]
    wApp.Build()
        .UseFalco(endpoints)
        .Run()
    0 // Exit code
