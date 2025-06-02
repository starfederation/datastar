namespace StarFederation.Datastar.Scripts

open System.Runtime.CompilerServices
open StarFederation.Datastar.FSharp.Scripts
open StarFederation.Datastar.DependencyInjection

[<AbstractClass; Sealed; Extension>]
type ServerSentEventScriptExtensions() =

    [<Extension>]
    static member Redirect (sse:IDatastarServerSentEventService, url:string) =
        Redirect.Redirect (sse.Handler, url)

    [<Extension>]
    static member BrowserConsoleAction (sse:IDatastarServerSentEventService, consoleAction:BrowserConsoleAction) =
        BrowserConsole.BrowserConsoleAction (sse.Handler, consoleAction)
