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

type BrowserConsole =
    static member browserConsoleAction (env, consoleAction, ?options:EventOptions) =
        let options = options |> Option.defaultValue EventOptions.defaults
        let scriptOptions = { ExecuteScriptOptions.defaults with EventId = options.EventId; Retry = options.Retry }
        ServerSentEventGenerator.executeScript (env, consoleAction |> BrowserConsoleAction.toJavaScript, scriptOptions)