namespace StarFederation.Datastar.Scripts

open StarFederation.Datastar

module Redirect =
    let redirectWithOptions (options:EventOptions) env url =
        let scriptOptions = { ExecuteScriptOptions.defaults with EventId = options.EventId; Retry = options.Retry }
        ServerSentEventGenerator.executeScriptWithOptions scriptOptions env $"window.location.href = '%s{url}';"
    let redirect env = redirectWithOptions EventOptions.defaults env