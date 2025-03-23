namespace StarFederation.Datastar.Scripts

open StarFederation.Datastar

type Redirect =
    static member redirect (env, url, ?options:EventOptions) =
        let options = options |> Option.defaultValue EventOptions.defaults
        let scriptOptions = { ExecuteScriptOptions.defaults with EventId = options.EventId; Retry = options.Retry }
        ServerSentEventGenerator.executeScript (env, $"window.location.href = '%s{url}';", scriptOptions)