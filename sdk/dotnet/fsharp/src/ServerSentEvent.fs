namespace StarFederation.Datastar.FSharp

open System
open System.Buffers
open System.Text

module internal ServerSentEvent =
    let private eventPrefix = "event: "B
    let private idPrefix = "id: "B
    let private retryPrefix = "retry: "B
    let private dataPrefix = "data: "B

    let inline private writeUtf8String (str:string) (writer:IBufferWriter<byte>) =
        let span = writer.GetSpan(Encoding.UTF8.GetByteCount(str))
        let bytesWritten = Encoding.UTF8.GetBytes(str.AsSpan(), span)
        writer.Advance(bytesWritten)
        writer

    let inline private writeUtf8Literal (bytes:byte[]) (writer:IBufferWriter<byte>) =
        let span = writer.GetSpan(bytes.Length)
        bytes.AsSpan().CopyTo(span)
        writer.Advance(bytes.Length)
        writer

    let inline writeNewline (writer:IBufferWriter<byte>) =
        let span = writer.GetSpan(1)
        span[0] <- 10uy // '\n'
        writer.Advance(1)

    let inline sendEventType eventType writer =
        writer
        |> writeUtf8Literal eventPrefix
        |> writeUtf8String (Consts.EventType.toString eventType)
        |> writeNewline

    let inline sendId id writer =
        match id with
        | ValueSome idValue ->
            writer
            |> writeUtf8Literal idPrefix
            |> writeUtf8String idValue
            |> writeNewline
        | _ -> ()

    let inline sendRetry (retry:TimeSpan) writer =
        if retry <> Consts.DefaultSseRetryDuration then
            writer
            |> writeUtf8Literal retryPrefix
            |> writeUtf8String (retry.TotalMilliseconds.ToString())
            |> writeNewline

    let inline sendDataLine dataLine writer =
        writer
        |> writeUtf8Literal dataPrefix
        |> writeUtf8String dataLine
        |> writeNewline
