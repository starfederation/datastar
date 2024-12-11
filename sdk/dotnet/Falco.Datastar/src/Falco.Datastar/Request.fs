namespace Falco.Datastar

open Falco
open Microsoft.AspNetCore.Http
open StarFederation.Datastar

[<RequireQualifiedAccess>]
module Request =
    let deserializeSignals<'T when 'T :> ISignals> (deserializer:string voption -> 'T) = (fun (ctx:HttpContext) -> task {
        let! signals = ServerSentEventHttpHandler.readRawSignals ctx.Request
        return deserializer signals
        })
    let getSignals<'T when 'T :> ISignals> = (fun (ctx:HttpContext) -> task {
        let sseService = ctx.RequestServices.GetService(typedefof<ISignals>)
        if sseService = null then failwith "Default ISignals deserializer not set; add the Datastar service with ServerSentEventServices.create<> or call Request.deserializeSignals instead"
        let sseService' = sseService :?> 'T
        return sseService'
        })
