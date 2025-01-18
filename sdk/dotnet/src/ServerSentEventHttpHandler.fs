namespace StarFederation.Datastar

open System.IO
open System.Text
open System.Text.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.Primitives
open Microsoft.Net.Http.Headers

module ServerSentEventHttpHandler =
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

    let readRawSignals (httpRequest:HttpRequest) : ValueTask<string voption> =
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
                } |> ValueTask<string voption>

    let sendServerEvent (httpResponse:HttpResponse) (event:string) =
        let bytes = Encoding.UTF8.GetBytes(event)
        httpResponse.BodyWriter.WriteAsync(bytes).AsTask()

type IServerSentEventHandler =
    interface
        inherit ISendServerEvent
        inherit IReadRawSignals
    end

type ServerSentEventHttpHandler(httpContext:HttpContext) =
    do
        let startResponseTask = ServerSentEventHttpHandler.startResponse httpContext.Response
        startResponseTask.GetAwaiter().GetResult()

    member _.HttpContext = httpContext

    with
    interface IServerSentEventHandler

    interface ISendServerEvent with
        member this.SendServerEvent (event:string) = ServerSentEventHttpHandler.sendServerEvent this.HttpContext.Response event

    interface IReadRawSignals with
        member this.ReadRawSignals () = ServerSentEventHttpHandler.readRawSignals this.HttpContext.Request
