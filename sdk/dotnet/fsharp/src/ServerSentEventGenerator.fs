namespace StarFederation.Datastar.FSharp

open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Primitives
open StarFederation.Datastar.FSharp.Utility

[<Sealed>]
type ServerSentEventGenerator(httpContextAccessor:IHttpContextAccessor) =
    let httpRequest = httpContextAccessor.HttpContext.Request
    let httpResponse = httpContextAccessor.HttpContext.Response
    let mutable _startResponseTask : Task = null
    let _startResponseLock = obj()
    let _eventQueue = ConcurrentQueue<unit -> Task>()

    static member StartServerEventStreamAsync(httpResponse:HttpResponse, additionalHeaders:KeyValuePair<string, StringValues> seq, cancellationToken:CancellationToken) =
        let task = backgroundTask {
            httpResponse.Headers.ContentType <- "text/event-stream"
            if (httpResponse.HttpContext.Request.Protocol = HttpProtocol.Http11) then
                httpResponse.Headers.Connection <- "keep-alive"
            for KeyValue(name, content) in additionalHeaders do
                match httpResponse.Headers.TryGetValue(name) with
                | false, _ -> httpResponse.Headers.Add(name, content)
                | true, _ -> ()
            do! httpResponse.StartAsync(cancellationToken)
            return! httpResponse.BodyWriter.FlushAsync(cancellationToken)
        }
        task :> Task

    static member PatchElementsAsync(httpResponse:HttpResponse, elements:string, options:PatchElementsOptions, cancellationToken:CancellationToken) =
        let writer = httpResponse.BodyWriter
        writer |> ServerSentEvent.sendEventType PatchElements
        writer |> ServerSentEvent.sendId options.EventId
        writer |> ServerSentEvent.sendRetry options.Retry

        match options.Selector with
        | ValueSome selector -> writer |> ServerSentEvent.sendDataLine $"{Consts.DatastarDatalineSelector} {Selector.value selector}"
        | _ -> ()

        if options.PatchMode <> Consts.DefaultElementPatchMode then
            writer |> ServerSentEvent.sendDataLine $"{Consts.DatastarDatalineMode} {Consts.ElementPatchMode.toString options.PatchMode}"

        if options.UseViewTransition <> Consts.DefaultElementsUseViewTransitions then
            writer |> ServerSentEvent.sendDataLine $"{Consts.DatastarDatalineUseViewTransition} %A{options.UseViewTransition}"

        for segment in String.splitLinesToSegments elements do
            writer |> ServerSentEvent.sendDataLine (String.buildDataLine Consts.DatastarDatalineElements segment)

        writer |> ServerSentEvent.writeNewline

        writer.FlushAsync(cancellationToken).AsTask() :> Task

    static member RemoveElementAsync(httpResponse:HttpResponse, selector:Selector, options:RemoveElementOptions, cancellationToken:CancellationToken) =
        let writer = httpResponse.BodyWriter
        writer |> ServerSentEvent.sendEventType PatchElements
        writer |> ServerSentEvent.sendId options.EventId
        writer |> ServerSentEvent.sendRetry options.Retry

        writer |> ServerSentEvent.sendDataLine $"{Consts.DatastarDatalineSelector} {selector |> Selector.value}"
        writer |> ServerSentEvent.sendDataLine $"{Consts.DatastarDatalineMode} {ElementPatchMode.Remove |> Consts.ElementPatchMode.toString}"

        if options.UseViewTransition <> Consts.DefaultElementsUseViewTransitions then
            writer |> ServerSentEvent.sendDataLine $"{Consts.DatastarDatalineUseViewTransition} %A{options.UseViewTransition}"

        writer |> ServerSentEvent.writeNewline

        writer.FlushAsync(cancellationToken).AsTask() :> Task

    static member PatchSignalsAsync(httpResponse:HttpResponse, signals:Signals, options:PatchSignalsOptions, cancellationToken:CancellationToken) =
        let writer = httpResponse.BodyWriter
        writer |> ServerSentEvent.sendEventType PatchSignals
        writer |> ServerSentEvent.sendId options.EventId
        writer |> ServerSentEvent.sendRetry options.Retry

        if options.OnlyIfMissing <> Consts.DefaultPatchSignalsOnlyIfMissing then
            writer |> ServerSentEvent.sendDataLine $"{Consts.DatastarDatalineOnlyIfMissing} %A{options.OnlyIfMissing}"

        for segment in String.splitLinesToSegments (Signals.value signals) do
            writer |> ServerSentEvent.sendDataLine (String.buildDataLine Consts.DatastarDatalineSignals segment)

        writer |> ServerSentEvent.writeNewline

        writer.FlushAsync(cancellationToken).AsTask() :> Task

    static member ExecuteScriptAsync(httpResponse:HttpResponse, script:string, options:ExecuteScriptOptions, cancellationToken:CancellationToken) =
        let writer = httpResponse.BodyWriter
        writer |> ServerSentEvent.sendEventType PatchElements
        writer |> ServerSentEvent.sendId options.EventId
        writer |> ServerSentEvent.sendRetry options.Retry

        writer |> ServerSentEvent.sendDataLine $"{Consts.DatastarDatalineElements} <script>"
        String.splitLinesToSegments script
        |> Seq.map (String.buildDataLine Consts.DatastarDatalineElements)
        |> Seq.iter (fun line -> writer |> ServerSentEvent.sendDataLine line)
        writer |> ServerSentEvent.sendDataLine $"{Consts.DatastarDatalineElements} </script>"

        writer |> ServerSentEvent.writeNewline

        writer.FlushAsync(cancellationToken).AsTask() :> Task

    static member GetSignalsStream(httpRequest:HttpRequest) =
        match httpRequest.Method with
        | System.Net.WebRequestMethods.Http.Get ->
            match httpRequest.Query.TryGetValue(Consts.DatastarKey) with
            | true, stringValues when stringValues.Count > 0 -> (new MemoryStream(Encoding.UTF8.GetBytes(stringValues[0])) :> Stream)
            | _ -> Stream.Null
        | _ -> httpRequest.Body

    static member ReadSignalsAsync(httpRequest:HttpRequest, cancellationToken:CancellationToken) =
        task {
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

    static member ReadSignalsAsync<'T>(httpRequest:HttpRequest, jsonSerializerOptions:JsonSerializerOptions, cancellationToken:CancellationToken) =
        task {
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

    member this.StartServerEventStreamAsync(additionalHeaders, cancellationToken) =
        lock _startResponseLock (fun () ->
            if _startResponseTask = null
            then _startResponseTask <- ServerSentEventGenerator.StartServerEventStreamAsync(httpResponse, additionalHeaders, cancellationToken)
            )
        _startResponseTask

    member private this.SendEventAsync(sendEventTask:unit -> Task, cancellationToken:CancellationToken) =
        task {
            _eventQueue.Enqueue(sendEventTask)
            do!
                if _startResponseTask <> null
                then _startResponseTask
                else ServerSentEventGenerator.StartServerEventStreamAsync(httpResponse, Seq.empty, cancellationToken)
            let (_, sendEventTask') = _eventQueue.TryDequeue()
            return! sendEventTask' ()
        }

    member this.PatchElementsAsync(elements, options, cancellationToken) =
        let sendTask = fun () -> ServerSentEventGenerator.PatchElementsAsync(httpResponse, elements, options, cancellationToken)
        this.SendEventAsync (sendTask, cancellationToken) :> Task

    member this.RemoveElementAsync(selector, options, cancellationToken) =
        let sendTask = fun () -> ServerSentEventGenerator.RemoveElementAsync(httpResponse, selector, options, cancellationToken)
        this.SendEventAsync (sendTask, cancellationToken) :> Task

    member this.PatchSignalsAsync(signals, options, cancellationToken) =
        let sendTask = fun () -> ServerSentEventGenerator.PatchSignalsAsync(httpResponse, signals, options, cancellationToken)
        this.SendEventAsync (sendTask, cancellationToken) :> Task

    member this.ExecuteScriptAsync(script, options, cancellationToken) =
        let sendTask = fun () -> ServerSentEventGenerator.ExecuteScriptAsync(httpResponse, script, options, cancellationToken)
        this.SendEventAsync (sendTask, cancellationToken) :> Task

    member this.GetSignalsStream() =
        ServerSentEventGenerator.GetSignalsStream(httpRequest)

    member this.ReadSignalsAsync(cancellationToken) : Task<Signals> =
        ServerSentEventGenerator.ReadSignalsAsync(httpRequest, cancellationToken)

    member this.ReadSignalsAsync<'T>(jsonSerializerOptions, cancellationToken) =
        ServerSentEventGenerator.ReadSignalsAsync<'T>(httpRequest, jsonSerializerOptions, cancellationToken)

    //
    // SHORT HAND METHODS
    //
    static member StartServerEventStreamAsync(httpResponse, additionalHeaders) =
        ServerSentEventGenerator.StartServerEventStreamAsync(httpResponse, additionalHeaders, httpResponse.HttpContext.RequestAborted)
    static member StartServerEventStreamAsync(httpResponse) =
        ServerSentEventGenerator.StartServerEventStreamAsync(httpResponse, Seq.empty, httpResponse.HttpContext.RequestAborted)
    static member StartServerEventStreamAsync(httpResponse, cancellationToken) =
        ServerSentEventGenerator.StartServerEventStreamAsync(httpResponse, Seq.empty, cancellationToken)

    static member PatchElementsAsync(httpResponse, elements, options) =
        ServerSentEventGenerator.PatchElementsAsync(httpResponse, elements, options, httpResponse.HttpContext.RequestAborted)
    static member PatchElementsAsync(httpResponse, elements) =
        ServerSentEventGenerator.PatchElementsAsync(httpResponse, elements, PatchElementsOptions.Defaults, httpResponse.HttpContext.RequestAborted)

    static member RemoveElementAsync(httpResponse, selector, options) =
        ServerSentEventGenerator.RemoveElementAsync(httpResponse, selector, options, httpResponse.HttpContext.RequestAborted)
    static member RemoveElementAsync(httpResponse, selector) =
        ServerSentEventGenerator.RemoveElementAsync(httpResponse, selector, RemoveElementOptions.Defaults, httpResponse.HttpContext.RequestAborted)

    static member PatchSignalsAsync(httpResponse, signals, options) =
        ServerSentEventGenerator.PatchSignalsAsync(httpResponse, signals, options, httpResponse.HttpContext.RequestAborted)
    static member PatchSignalsAsync(httpResponse, signals) =
        ServerSentEventGenerator.PatchSignalsAsync(httpResponse, signals, PatchSignalsOptions.Defaults, httpResponse.HttpContext.RequestAborted)

    static member ExecuteScriptAsync(httpResponse, script, options) =
        ServerSentEventGenerator.ExecuteScriptAsync(httpResponse, script, options, httpResponse.HttpContext.RequestAborted)
    static member ExecuteScriptAsync(httpResponse, script) =
        ServerSentEventGenerator.ExecuteScriptAsync(httpResponse, script, ExecuteScriptOptions.Defaults, httpResponse.HttpContext.RequestAborted)

    static member ReadSignalsAsync(httpRequest) =
        ServerSentEventGenerator.ReadSignalsAsync(httpRequest, cancellationToken=httpRequest.HttpContext.RequestAborted)
    static member ReadSignalsAsync<'T>(httpRequest, jsonSerializerOptions) =
        ServerSentEventGenerator.ReadSignalsAsync<'T>(httpRequest, jsonSerializerOptions, httpRequest.HttpContext.RequestAborted)
    static member ReadSignalsAsync<'T>(httpRequest) =
        ServerSentEventGenerator.ReadSignalsAsync<'T>(httpRequest, JsonSerializerOptions.SignalsDefault, httpRequest.HttpContext.RequestAborted)

    member this.StartServerEventStreamAsync(additionalHeaders) =
        ServerSentEventGenerator.StartServerEventStreamAsync(httpResponse, additionalHeaders, httpResponse.HttpContext.RequestAborted)
    member this.StartServerEventStreamAsync(cancellationToken:CancellationToken) =
        ServerSentEventGenerator.StartServerEventStreamAsync(httpResponse, Seq.empty, cancellationToken)
    member this.StartServerEventStreamAsync() =
        ServerSentEventGenerator.StartServerEventStreamAsync(httpResponse, Seq.empty, httpResponse.HttpContext.RequestAborted)

    member this.PatchElementsAsync(elements, options) =
        ServerSentEventGenerator.PatchElementsAsync(httpResponse, elements, options, httpResponse.HttpContext.RequestAborted)
    member this.PatchElementsAsync(elements, cancellationToken) =
        ServerSentEventGenerator.PatchElementsAsync(httpResponse, elements, PatchElementsOptions.Defaults, cancellationToken)
    member this.PatchElementsAsync(elements) =
        ServerSentEventGenerator.PatchElementsAsync(httpResponse, elements, PatchElementsOptions.Defaults, httpResponse.HttpContext.RequestAborted)

    member this.RemoveElementAsync(selector, options) =
        ServerSentEventGenerator.RemoveElementAsync(httpResponse, selector, options, httpResponse.HttpContext.RequestAborted)
    member this.RemoveElementAsync(selector, cancellationToken) =
        ServerSentEventGenerator.RemoveElementAsync(httpResponse, selector, RemoveElementOptions.Defaults, cancellationToken)
    member this.RemoveElementAsync(selector) =
        ServerSentEventGenerator.RemoveElementAsync(httpResponse, selector, RemoveElementOptions.Defaults, httpResponse.HttpContext.RequestAborted)

    member this.PatchSignalsAsync(signals, options) =
        ServerSentEventGenerator.PatchSignalsAsync(httpResponse, signals, options, httpResponse.HttpContext.RequestAborted)
    member this.PatchSignalsAsync(signals, cancellationToken) =
        ServerSentEventGenerator.PatchSignalsAsync(httpResponse, signals, PatchSignalsOptions.Defaults, cancellationToken)
    member this.PatchSignalsAsync(signals) =
        ServerSentEventGenerator.PatchSignalsAsync(httpResponse, signals, PatchSignalsOptions.Defaults, httpResponse.HttpContext.RequestAborted)

    member this.ExecuteScriptAsync(script, options) =
        ServerSentEventGenerator.ExecuteScriptAsync(httpResponse, script, options, httpResponse.HttpContext.RequestAborted)
    member this.ExecuteScriptAsync(script, cancellationToken) =
        ServerSentEventGenerator.ExecuteScriptAsync(httpResponse, script, ExecuteScriptOptions.Defaults, cancellationToken)
    member this.ExecuteScriptAsync(script) =
        ServerSentEventGenerator.ExecuteScriptAsync(httpResponse, script, ExecuteScriptOptions.Defaults, httpResponse.HttpContext.RequestAborted)

    member this.ReadSignalsAsync(): Task<Signals> =
        ServerSentEventGenerator.ReadSignalsAsync(httpRequest, httpRequest.HttpContext.RequestAborted)
    member this.ReadSignalsAsync<'T>(jsonSerializerOptions) =
        ServerSentEventGenerator.ReadSignalsAsync<'T>(httpRequest, jsonSerializerOptions, httpRequest.HttpContext.RequestAborted)
    member this.ReadSignalsAsync<'T>(cancellationToken) =
        ServerSentEventGenerator.ReadSignalsAsync<'T>(httpRequest, JsonSerializerOptions.SignalsDefault, cancellationToken)
    member this.ReadSignalsAsync<'T>() =
        ServerSentEventGenerator.ReadSignalsAsync<'T>(httpRequest, JsonSerializerOptions.SignalsDefault, httpRequest.HttpContext.RequestAborted)
