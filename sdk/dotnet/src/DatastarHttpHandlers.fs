namespace StarFederation.Datastar

open System.IO
open System.Text
open System.Text.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Primitives
open Microsoft.Net.Http.Headers

/// <summary>
/// Functions to start a text/event-stream response, send an SSE, and read the signals from a request
/// </summary>
module DatastarHttpHandlers =

    /// <summary>
    /// Starts the SSE response, 'text/event-stream', so more SSEs can be sent
    /// </summary>
    let startResponse (response:HttpResponse) = task {
        let setHeader (response:HttpResponse) (name, content:string) =
            if response.Headers.ContainsKey(name) |> not then
                response.Headers.Add(name, StringValues(content))

        seq {
            ("Cache-Control", "no-cache, max-age, must-revalidate, no-store")
            (HeaderNames.ContentType, "text/event-stream")
            if (response.HttpContext.Request.Protocol = HttpProtocol.Http11) then
                ("Connection", "keep-alive")
            } |> Seq.iter (setHeader response)
        do! response.StartAsync()
        do! response.Body.FlushAsync()
        }

    /// <summary>
    /// Read the signals, as a string, from the HttpRequest
    /// </summary>
    let readSignals (httpRequest:HttpRequest) : ValueTask<Signals voption> =
        match httpRequest.Method with
        | System.Net.WebRequestMethods.Http.Get ->
            match httpRequest.Query.TryGetValue(Consts.DatastarKey) with
            | true, json when json.Count > 0 -> ValueSome json[0]
            | _ -> ValueNone
            |> ValueTask.FromResult
        | _ ->
            task {
                use readResult = new StreamReader(httpRequest.BodyReader.AsStream())
                let! str = readResult.ReadToEndAsync()
                return ValueSome str
                } |> ValueTask<Signals voption>

    /// <summary>
    /// Sends an SSE to the HttpResponse
    /// </summary>
    let sendServerEvent (httpResponse:HttpResponse) (sse:ServerSentEvent) =
        let serializedEvent = sse |> ServerSentEvent.serialize
        let bytes = Encoding.UTF8.GetBytes(serializedEvent)
        httpResponse.BodyWriter.WriteAsync(bytes).AsTask()

/// <summary>
/// Implementation of ISendServerEvent, for sending SSEs to the HttpResponse
/// </summary>
type ServerSentEventHttpHandlers (httpResponse:HttpResponse) =
    do
        let startResponseTask = DatastarHttpHandlers.startResponse httpResponse
        startResponseTask.GetAwaiter().GetResult()

    member _.HttpResponse = httpResponse

    interface ISendServerEvent with
        member this.SendServerEvent (event:ServerSentEvent) = DatastarHttpHandlers.sendServerEvent this.HttpResponse event

/// <summary>
/// Implementation of IReadSignals, for reading the Signals from the HttpRequest
/// </summary>
type SignalsHttpHandlers (httpRequest:HttpRequest) =
    let mutable _cachedSignals : Signals voption voption = ValueNone
    member _.HttpRequest = httpRequest

    interface IReadSignals with
        member this.ReadSignals () =
            let readTask =
                match _cachedSignals with
                | ValueSome signals -> task { return signals }
                | ValueNone ->
                    task {
                        let! signals = (DatastarHttpHandlers.readSignals this.HttpRequest)
                        _cachedSignals <- (ValueSome signals)
                        return signals
                    }
            readTask |> ValueTask<Signals voption>
        member this.ReadSignals<'T> () =
            let readTask = task {
                let! signals = (this :> IReadSignals).ReadSignals()
                let ret =
                    match signals with
                    | ValueNone -> ValueNone
                    | ValueSome rs ->
                        try ValueSome (JsonSerializer.Deserialize<'T>(rs))
                        with _ -> ValueNone
                return ret
                }
            readTask |> ValueTask<'T voption>
