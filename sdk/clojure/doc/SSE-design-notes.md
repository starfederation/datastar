# SSE, Buffering, Design considerations

There is some design work to do when using SSE, particularly around buffering.

When using a [ring](https://github.com/ring-clojure/ring) compliant adapter our
SSE connection is a `java.io.OutputStream`. There are several considerations
when dealing with those:

| you may want to            | solution                                        |
| -------------------------- | ----------------------------------------------- |
| write bytes directly       | just use the `OutputStream`                     |
| write bytes with buffering | use a `java.io.BufferedOutputStream`            |
| compress the stream        | use a `java.util.zip.GZIPOuputStream`           |
| write text                 | use a `java.io.OutputStreamWriter`              |
| buffer the text writes     | that's where it becomes interesting for the SDK |

## Exploring buffering

### Why buffering

- Concatenating arrays without a buffer is really inefficient
- 1 write operation on an `OutputStream` result in 1 IO syscall
  (at least that is the mental model).
- With buffering we don't have a IO syscall until we explicitly flush or
  until the buffer is full and flushes by itself.

With limiting allocations when concatenating data, buffering is a strategy to
reduce the number of IO syscalls or at least be smart about when the call is
made.

### SSE considerations

In the case of SSE we want to send events as they are ready, and not have them
sit in a buffer.

However when creating the event's text we assemble it from parts (specific SSE
lines, data lines...). We could send events line by line but this would result
in 1 IO call per line.

So we need some kind of buffer to assemble an event before flushing it whole.

Here are some solutions for buffering the writes:

1. persistent buffer: use a `java.io.BufferedWriter` and keep it around
2. temporary buffer: use temporary buffer (likely a `StringBuilder`) that is
   discarded after assembling and sending 1 event
3. buffer pooling: use some sort of buffer pooling

| solution | impact on memory                                        |
| -------- | ------------------------------------------------------- |
| 1        | long lived SSE -> long lived buffer -> consuming memory |
| 2        | short lived buffer, consuming memory when sending only  |
| 3        | long lived, fix / controlable supply of buffers         |

| solution | impact on GC / allocations                   |
| -------- | -------------------------------------------- |
| 1        | 1 allocation and done                        |
| 2        | churning through buffers for each event sent |
| 3        | controlled allocations                       |

| solution | notes                                                                           |
| -------- | ------------------------------------------------------------------------------- |
| 1        | we can control the size of the buffer                                           |
| 2        | the jvm gc should be able _recycle_ short lived objects                         |
| 3        | no direct support in the jvm, gc _recycling_ maybe be better, needs to be tuned |

> [!note]
> An `OutputStream` compression wrapper comes with an internal buffer and a
> context window that will both allocate and retain memory.

> [!important]
> A `ByteArrayOutputStream` is also another buffer, it doesn't shrink in size
> when reset is called (see [javadoc](<https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/io/ByteArrayOutputStream.html#reset()>))

## Datastar SDK

### Considerations

There is too much ground to cover for a truly generic API. Some ring adapters
are partially compliant with the ring spec and provide us with other mechanisms
than an `OutputStream` to materialize the SSE connection. Buffer pooling isn't
really part of the SDK. Going that route would mean adding a dependency to the
SDK and I haven't found a ready made solution anyway.

### Current SDK implementation

#### Common SSE machinery

##### `starfederation.datastar.clojure.api.sse`

This namespace provides 2 generic functions:

- `headers`: generate HTTP headers with the SSE specific ones given a ring
  request.
- `write-event!` provides a way to assemble an SSE event's string using a
  `java.util.appendable`.

These functions provide a basis for implementing SSE and are orthogonal to
Datastar's specific SSE events.

##### `starfederation.datastar.clojure.adapter.common`

This namespace provides helpers we use to build the SSE machinery for ring
adapters. It mainly provides a mechanism called "write profiles" to allow
a user to configure the way the SSE connections should behave with regards
to Buffering and compression.

See the [write profiles doc](./Write-profiles.md).

## Beyond the SDK

If the write profile system doesn't provide enough control there is still
the possibility to implement adapters using the
`starfederation.datastar.clojure.protocols/SSEGenerator` and control everything.

The hope is that there is enough material between the documentation and the
source code to make this relatively easy.
