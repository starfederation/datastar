namespace StarFederation.Datastar

open System.IO
open System.Text
open System.Text.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Primitives
open Microsoft.Net.Http.Headers

/// <summary>
/// Implementation of ISendServerEvent, for sending SSEs to the HttpResponse
/// </summary>
type ServerSentEventHttpHandlers (httpResponse:HttpResponse) =
    let mutable _additionalHeaders : (string * string) list = []
    let mutable _respondTask = null

    member _.HttpResponse = httpResponse
    member this.AddHeader header =
        _additionalHeaders <- _additionalHeaders @ [ header ]

    member this.StartResponse () =
        let respondTask = task {
            let setHeader (httpResponse:HttpResponse) (name, content:string) =
                if httpResponse.Headers.ContainsKey(name) |> not then
                    httpResponse.Headers.Add(name, StringValues(content))

            seq {
                ("Cache-Control", "no-cache, max-age, must-revalidate, no-store")
                (HeaderNames.ContentType, "text/event-stream")
                if (httpResponse.HttpContext.Request.Protocol = HttpProtocol.Http11) then
                    ("Connection", "keep-alive")
                yield! _additionalHeaders
                } |> Seq.iter (setHeader httpResponse)
            do! httpResponse.StartAsync()
            do! httpResponse.Body.FlushAsync()
            }
        _respondTask <- respondTask
        respondTask :> Task

    interface ISendServerEvent with
        member this.SendServerEvent sse = task {
            do! _respondTask  // it is required that StartResponse() has already run
            let serializedEvent = sse |> ServerSentEvent.serialize
            let bytes = Encoding.UTF8.GetBytes(serializedEvent)
            return! httpResponse.BodyWriter.WriteAsync(bytes)
            }

/// <summary>
/// Implementation of IReadSignals, for reading the Signals from the HttpRequest
/// </summary>
type SignalsHttpHandlers (httpRequest:HttpRequest) =
    let mutable _cachedSignals : Signals voption voption = ValueNone

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

    member _.HttpRequest = httpRequest

    interface IReadSignals with
        member this.ReadSignals () =
            let readTask =
                match _cachedSignals with
                | ValueSome signals -> task { return signals }
                | ValueNone ->
                    task {
                        let! signals = (readSignals this.HttpRequest)
                        _cachedSignals <- (ValueSome signals)
                        return signals
                    }
            readTask |> ValueTask<Signals voption>
        member this.ReadSignals<'T> jsonSerializerOptions =
            let readTask = task {
                let! signals = (this :> IReadSignals).ReadSignals()
                let ret =
                    match signals with
                    | ValueNone -> ValueNone
                    | ValueSome rs ->
                        try ValueSome (JsonSerializer.Deserialize<'T>(rs, jsonSerializerOptions))
                        with _ -> ValueNone
                return ret
                }
            readTask |> ValueTask<'T voption>
        member this.ReadSignals<'T> () = (this :> IReadSignals).ReadSignals<'T> JsonSerializerOptions.Default
