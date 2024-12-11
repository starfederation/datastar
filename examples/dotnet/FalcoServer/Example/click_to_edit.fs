module FalcoServer.Example.click_to_edit

open System.Text.Json
open Microsoft.FSharp.Core
open StarFederation.Datastar
open global.Falco
open Falco.Routing
open Falco.Markup
open Falco.Datastar

let [<Literal>] examplesSegment = "examples"
let [<Literal>] clickToEditSegment = "click_to_edit"
let rootSegments = $"/{examplesSegment}/{clickToEditSegment}"

[<Struct>]
type Contact =
    { FirstName: string
      LastName: string
      Email: string }
    with
    interface ISignals with
        member this.Serialize () = JsonSerializer.Serialize(this)

module Contact =
    let defaults =
        { FirstName = "John"
          LastName = "Doe"
          Email = "joe@blow.com" }

module Fragments =
    let UserComponent (contact:Contact) =
        Elem.div [ Attr.id "contact_1"; Attr.class' "flex flex-col max-w-sm gap-2" ] [
            Elem.label [ Attr.class' "input-label" ] [ Text.raw $"First Name:" ]
            Elem.label [ Attr.class' "input-label-text" ] [ Text.raw $"{contact.FirstName}" ]
            Elem.label [ Attr.class' "input-label" ] [ Text.raw $"Last Name:" ]
            Elem.label [ Attr.class' "input-label-text" ] [ Text.raw contact.LastName ]
            Elem.label [ Attr.class' "input-label" ] [ Text.raw $"Email:" ]
            Elem.label [ Attr.class' "input-label-text" ] [ Text.raw contact.Email ]
            Elem.div [ Attr.class' "flex gap-4" ] [
                Elem.button [Attr.class' "btn btn-primary"; Ds.onClick (Get $"{rootSegments}/contact/1/edit") ] [
                    Text.raw "Edit"
                    ]
                Elem.button [Attr.class' "btn btn-secondary"; Ds.onClick (Patch $"{rootSegments}/contact/1/reset") ] [
                    Text.raw "Reset"
                    ]
                ]
            ]

    let UserEdit (contact:Contact) =
        Elem.div [ Attr.id "contact_1"; Attr.class' "flex flex-col gap-2"; Ds.signals contact ] [
            Elem.label [ Attr.class' "input-label" ] [ Text.raw $"First Name:" ]
            Elem.input [ Attr.class' "input-text"; Ds.bind "FirstName" ]
            Elem.label [ Attr.class' "input-label" ] [ Text.raw $"Last Name:" ]
            Elem.input [ Attr.class' "input-text"; Ds.bind "LastName" ]
            Elem.label [ Attr.class' "input-label" ] [ Text.raw $"Email:" ]
            Elem.input [ Attr.class' "input-text"; Ds.bind "Email" ]
            Elem.div [ Attr.class' "flex gap-4" ] [
                Elem.button [ Attr.class' "btn btn-primary"; Ds.onClick (Put $"{rootSegments}/contact/1") ] [ Text.raw "Save" ]
                Elem.button [ Attr.class' "btn btn-secondary"; Ds.onClick (Get $"{rootSegments}/contact/1") ] [ Text.raw "Cancel" ]
                ]
            ]

let mutable contact1 = Contact.defaults
let endpoints : HttpEndpoint list = [
    get $"{rootSegments}/contact/{{id}}"
        (Request.mapRoute
            (fun reader -> reader.Get "id")
            (fun _ ctx -> Response.ofMergeHtml (Fragments.UserComponent contact1) ctx))
    get $"{rootSegments}/contact/{{id}}/edit"
        (Request.mapRoute
            (fun reader -> reader.Get "id")
            (fun _ ctx -> Response.ofMergeHtml (Fragments.UserEdit contact1) ctx))
    patch $"{rootSegments}/contact/{{id}}/reset"
        (Request.mapRoute
            (fun reader -> reader.Get "id")
            (fun _ ctx -> task {
                contact1 <- Contact.defaults
                do! Response.ofMergeHtml (Fragments.UserComponent contact1) ctx
                }))
    put $"{rootSegments}/contact/{{id}}/"
        (Request.mapRoute
            (fun reader -> reader.Get "id")
            (fun _ ctx -> task {
                let! contact =
                    Request.deserializeSignals<Contact>
                        (fun str -> str |> ValueOption.map JsonSerializer.Deserialize<Contact> |> ValueOption.defaultValue Contact.defaults)
                        ctx
                contact1 <- contact
                do! Response.ofMergeHtml (Fragments.UserComponent contact1) ctx
                }))
    ]
