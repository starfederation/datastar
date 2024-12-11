module FalcoServer.Examples

open Falco
open Falco.Routing
open Falco.Markup
open Falco.Datastar
open FalcoServer.Example

type Example = { Name:string; BaseUrl:string; Start:XmlNode; Endpoints:HttpEndpoint list }
let examples = [
    { Name = "Click to Edit"; BaseUrl = click_to_edit.rootSegments; Start = Elem.div [Attr.id "contact_1"; Ds.onLoad (Get "/examples/click_to_edit/contact/1")] []; Endpoints = click_to_edit.endpoints }
    ]

let examplesList =
    Elem.ul [] (examples |> List.map (fun example -> Elem.li [] [ Elem.button [Attr.class' "btn btn-primary"; Ds.get example.BaseUrl] [Text.raw example.Name] ]))

let exampleEndpoints =
    examples
    |> Seq.map (fun example ->
        let start = get example.BaseUrl (Response.ofMergeHtml (Elem.div [Attr.id "example"] [example.Start] ))
        let inner = example.Endpoints
        start :: inner
        )
    |> List.concat
    |> List.append [ (get "/example_list" (Response.ofMergeHtml (Elem.div [Attr.id "example_list"] [examplesList] ))) ]
