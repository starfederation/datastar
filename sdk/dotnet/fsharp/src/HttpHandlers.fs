namespace StarFederation.Datastar.FSharp

open System.IO
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Primitives
open Microsoft.Net.Http.Headers

module JsonSerializerOptions =
    /// JsonSerializerOptions but case insensitive
    let SignalsDefault =
        let options = JsonSerializerOptions()
        options.PropertyNameCaseInsensitive <- true
        options

/// Implementation of ISendServerEvent, for sending SSEs to the HttpResponse
[<Sealed>]
type ServerSentEventHttpHandler (httpResponse:HttpResponse) =
    let mutable _startResponseTask : Task = null
    let _startResponseLock = obj()

    static member StartServerEventStream (httpResponse:HttpResponse, additionalHeaders:(string * string)[], cancellationToken:CancellationToken) =
        let task = backgroundTask {
            let setHeader (httpResponse:HttpResponse) (name, content:string) =
                if httpResponse.Headers.ContainsKey(name) |> not then
                    httpResponse.Headers.Add(name, StringValues(content))

            seq {
                (HeaderNames.ContentType, "text/event-stream")
                if (httpResponse.HttpContext.Request.Protocol = HttpProtocol.Http11) then
                    ("Connection", "keep-alive")
                yield! additionalHeaders
                } |> Seq.iter (setHeader httpResponse)
            do! httpResponse.StartAsync(cancellationToken)
            return! httpResponse.BodyWriter.FlushAsync(cancellationToken)
            }
        task :> Task
    static member StartServerEventStream (httpResponse, additionalHeader) = ServerSentEventHttpHandler.StartServerEventStream(httpResponse, additionalHeader, httpResponse.HttpContext.RequestAborted)

    static member SendServerEvent (httpResponse:HttpResponse, sse, cancellationToken:CancellationToken) =
        let task = task {
            let serializedSse = sse |> ServerSentEvent.serializeAsBytes |> Seq.toArray
            return! httpResponse.BodyWriter.WriteAsync(serializedSse, cancellationToken)
            }
        task :> Task
    static member SendServerEvent (sse, httpResponse) = ServerSentEventHttpHandler.SendServerEvent(httpResponse, sse, httpResponse.HttpContext.RequestAborted)

    interface ISendServerEvent with

        member this.StartServerEventStream (additionalHeaders, cancellationToken) =
            lock _startResponseLock (fun () -> if _startResponseTask = null then _startResponseTask <- ServerSentEventHttpHandler.StartServerEventStream(httpResponse, additionalHeaders, cancellationToken))
            _startResponseTask
        member this.StartServerEventStream(additionalHeaders) = (this:>ISendServerEvent).StartServerEventStream(additionalHeaders, httpResponse.HttpContext.RequestAborted)
        member this.StartServerEventStream() = (this:>ISendServerEvent).StartServerEventStream(Array.empty, httpResponse.HttpContext.RequestAborted)

        member this.SendServerEvent(sse, cancellationToken) = task {
            do! (this :> ISendServerEvent).StartServerEventStream(Array.empty, cancellationToken)
            return! ServerSentEventHttpHandler.SendServerEvent(httpResponse, sse, cancellationToken)
            }
        member this.SendServerEvent(sse) = (this:>ISendServerEvent).SendServerEvent(sse, httpResponse.HttpContext.RequestAborted)

/// Implementation of IReadSignals, for reading the Signals from the HttpRequest
[<Sealed>]
type SignalsHttpHandler (httpRequest:HttpRequest) =

    static member GetSignalsStream (httpRequest:HttpRequest) =
        match httpRequest.Method with
        | System.Net.WebRequestMethods.Http.Get ->
            match httpRequest.Query.TryGetValue(Consts.DatastarKey) with
            | true, stringValues when stringValues.Count > 0 -> (new MemoryStream(Encoding.UTF8.GetBytes(stringValues[0])) :> Stream)
            | _ -> Stream.Null
        | _ -> httpRequest.Body

    static member ReadSignalsAsync (httpRequest:HttpRequest, cancellationToken:CancellationToken) = task {
        match httpRequest.Method with
        | System.Net.WebRequestMethods.Http.Get ->
            match httpRequest.Query.TryGetValue(Consts.DatastarKey) with
            | true, stringValues when stringValues.Count > 0 -> return (stringValues[0] |> Signals.create)
            | _ -> return Signals.empty
        | _ ->
            try
                use readResult = new StreamReader(httpRequest.Body)
                let! signals = readResult.ReadToEndAsync(cancellationToken)
                return (signals |> Signals.create)
            with _ -> return Signals.empty
        }

    static member ReadSignalsAsync (httpRequest:HttpRequest) = SignalsHttpHandler.ReadSignalsAsync(httpRequest, httpRequest.HttpContext.RequestAborted)

    static member ReadSignalsAsync<'T> (httpRequest:HttpRequest, jsonSerializerOptions:JsonSerializerOptions, cancellationToken:CancellationToken) = task {
        try
            match httpRequest.Method with
            | System.Net.WebRequestMethods.Http.Get ->
                match httpRequest.Query.TryGetValue(Consts.DatastarKey) with
                | true, stringValues when stringValues.Count > 0 ->
                    return ValueSome (JsonSerializer.Deserialize<'T>(stringValues[0], jsonSerializerOptions))
                | _ ->
                    return ValueNone
            | _ ->
                let! t = JsonSerializer.DeserializeAsync<'T>(httpRequest.Body, jsonSerializerOptions, cancellationToken)
                return (ValueSome t)
        with _ -> return ValueNone
        }

    static member ReadSignalsAsync<'T> (httpRequest:HttpRequest, cancellationToken:CancellationToken) = SignalsHttpHandler.ReadSignalsAsync<'T>(httpRequest, JsonSerializerOptions.SignalsDefault, cancellationToken)
    static member ReadSignalsAsync<'T> (httpRequest:HttpRequest, jsonSerializerOptions:JsonSerializerOptions) = SignalsHttpHandler.ReadSignalsAsync<'T>(httpRequest, jsonSerializerOptions, httpRequest.HttpContext.RequestAborted)
    static member ReadSignalsAsync<'T> (httpRequest:HttpRequest) = SignalsHttpHandler.ReadSignalsAsync<'T>(httpRequest, JsonSerializerOptions.SignalsDefault)

    interface IReadSignals with
        member this.GetSignalsStream() = SignalsHttpHandler.GetSignalsStream(httpRequest)
        member this.ReadSignalsAsync(): Task<Signals> = SignalsHttpHandler.ReadSignalsAsync(httpRequest)
        member this.ReadSignalsAsync(cancellationToken: CancellationToken): Task<Signals> = SignalsHttpHandler.ReadSignalsAsync(httpRequest, cancellationToken)
        member this.ReadSignalsAsync<'T>(): Task<'T voption> = SignalsHttpHandler.ReadSignalsAsync<'T>(httpRequest)
        member this.ReadSignalsAsync<'T>(jsonSerializerOptions: JsonSerializerOptions): Task<'T voption> = SignalsHttpHandler.ReadSignalsAsync<'T>(httpRequest, jsonSerializerOptions)
        member this.ReadSignalsAsync<'T>(jsonSerializerOptions, cancellationToken) = SignalsHttpHandler.ReadSignalsAsync<'T>(httpRequest, jsonSerializerOptions, cancellationToken)