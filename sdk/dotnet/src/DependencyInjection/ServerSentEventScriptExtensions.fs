namespace StarFederation.Datastar.Scripts

open System.Runtime.CompilerServices
open StarFederation.Datastar.DependencyInjection

[<Extension>]
type ServerSentEventScriptExtensions() =

    [<Extension>]
    static member Redirect (sse:IDatastarServerSentEventService, url:string) =
        Redirect.redirect (sse.Handler, url)

    [<Extension>]
    static member BrowserConsoleAction (sse:IDatastarServerSentEventService, consoleAction:BrowserConsoleAction) =
        BrowserConsole.browserConsoleAction (sse.Handler, consoleAction)