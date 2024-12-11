namespace Falco.Datastar

open Falco.Markup
open StarFederation.Datastar

module Ds =
    let signals (signals:ISignals) = KeyValueAttr (Signals |> DsAttr.AsString, System.Web.HttpUtility.HtmlEncode(signals.Serialize()))
    let bind name = KeyValueAttr (Bind |> DsAttr.AsString, name)
    let ref selector = KeyValueAttr(Ref |> DsAttr.AsString, selector |> Selector.value)

    let show selector = KeyValueAttr(Show |> DsAttr.AsString, selector |> Selector.value)
    let intersects modifier action = KeyValueAttr (Intersects modifier |> DsAttr.AsString, action |> DsAction.AsString)
    let teleport modifier selector = KeyValueAttr (Teleport modifier |> DsAttr.AsString, selector |> Selector.value)

    let onAction actionType action = KeyValueAttr ($"data-on-{actionType}", action |> DsAction.AsString)
    let onClick = onAction "click"
    let onLoad = onAction "load"

    let get url = onClick (Get url)
    let put url = onClick (Put url)
    let post url = onClick (Post url)
    let patch url = onClick (Patch url)
    let delete url = onClick (Delete url)
