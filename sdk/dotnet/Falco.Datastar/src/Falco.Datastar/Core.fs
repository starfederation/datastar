namespace Falco.Datastar

open Falco.Markup

module DatastarScript =
    let cdnSrc = "https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-beta.1/bundles/datastar.js"
    let cdn = [ Attr.type' "module"; Attr.defer; Attr.src cdnSrc ]

type DebounceTime =
    | Milliseconds of uint
    | Seconds of uint
    | Leading
    | NoTrail
    static member AsString debounceTime =
        match debounceTime with
        | Milliseconds ms -> $"_{ms}ms"
        | Seconds s -> $"_{s}s"
        | Leading -> "_leading"
        | NoTrail -> "_noTrail"

type ThrottleTime =
    | Milliseconds of uint
    | Seconds of uint
    | NoLead
    | NoTrail
    static member AsString throttleTime =
        match throttleTime with
        | Milliseconds ms -> $"_{ms}ms"
        | Seconds s -> $"_{s}s"
        | NoLead -> "_noLead"
        | NoTrail -> "_noTrail"

type DsAttrMod =
    | Once
    | Passive
    | Capture
    | Debounce of DebounceTime
    | Throttle of ThrottleTime
    | Window
    static member AsString attrModifier =
        match attrModifier with
        | Once -> ".once"
        | Passive -> ".passive"
        | Capture -> ".capture"
        | Debounce debounceTime -> $".debounce{debounceTime |> DebounceTime.AsString}"
        | Throttle throttleTime -> $".throttle{throttleTime |> ThrottleTime.AsString}"
        | Window -> ".window"

type DsAction =
    | Get of url:string
    | Post of url:string
    | Put of url:string
    | Patch of url:string
    | Delete of url:string
    | SetAll of pathPrefix:string * value:string
    | ToggleAll of PathPrefix:string
    | Clipboard of text:string
    | Script of string
    static member AsString action =
        match action with
        | Get url -> $"@get('{url}')"
        | Post url -> $"@post('{url}')"
        | Put url -> $"@put('{url}')"
        | Patch url -> $"@patch('{url}')"
        | Delete url -> $"@delete('{url}')"
        | SetAll(pathPrefix, value) -> $"@setAll('{pathPrefix}', {value})"
        | ToggleAll pathPrefix -> $"@toggleAll('{pathPrefix}')"
        | Clipboard text -> $"@clipboard('{text}')"
        | Script script -> script

type IntersectsModifier =
    | Default
    | Once
    | Half
    | Full
    static member AsString modifier =
        match modifier with
        | Default -> ""
        | Once -> ".once"
        | Half -> ".half"
        | Full -> ".full"

type TeleportModifier =
    | Default
    | Prepend
    | Append
    static member AsString modifier =
        match modifier with
        | Default -> ""
        | Prepend -> ".prepend"
        | Append -> ".append"

type DsAttr =
    | Signals
    | Ref
    | Bind
    | Show
    | Intersects of IntersectsModifier
    | Teleport of TeleportModifier
    static member AsString (attr:DsAttr) =
        match attr with
        | Signals -> "data-signals"
        | Ref -> "data-ref"
        | Bind -> "data-bind"
        | Show -> "data-show"
        | Intersects modifier -> $"data-intersects{modifier |> IntersectsModifier.AsString}"
        | Teleport modifier -> $"data-teleport{modifier |> TeleportModifier.AsString}"
