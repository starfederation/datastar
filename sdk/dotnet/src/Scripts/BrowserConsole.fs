namespace StarFederation.Datastar.Scripts

open StarFederation.Datastar

type BrowserConsoleAction =
    | Clear
    | Log of message:string
    | Error of message:string
module BrowserConsoleAction =
    let private escapeMessage (str:string) = str.Replace("'", @"\'")
    let toJavaScript =
        function
        | Clear -> "console.clear()"
        | Log message -> $"console.log('{escapeMessage message}')"
        | Error message -> $"console.log('{escapeMessage message}')"

module BrowserConsole =
    let consoleActionWithOptions (options:EventOptions) env consoleAction =
        let scriptOptions = { ExecuteScriptOptions.defaults with EventId = options.EventId; Retry = options.Retry }
        ServerSentEventGenerator.executeScriptWithOptions scriptOptions env (consoleAction |> BrowserConsoleAction.toJavaScript)
    let browserConsoleAction env = consoleActionWithOptions EventOptions.defaults env