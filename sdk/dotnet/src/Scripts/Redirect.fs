namespace StarFederation.Datastar.Scripts

open StarFederation.Datastar

[<AbstractClass; Sealed>]
type Redirect =
    static member Redirect (env, url, ?options:EventOptions) =
        let options = options |> Option.defaultValue EventOptions.defaults
        let scriptOptions = { ExecuteScriptOptions.defaults with EventId = options.EventId; Retry = options.Retry }
        ServerSentEventGenerator.ExecuteScript (env, $"window.location.href = '%s{url}';", scriptOptions)
