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
> When it comes to compression the `java.util.zip.GZIPOutputStream` is
> another buffer

> [!important]
> A `ByteArrayOutputStream` is also another buffer, it doesn't shrink in size
> when reset is called (see [javadoc](<https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/io/ByteArrayOutputStream.html#reset()>))

## Datastar SDK

### Considerations

There is too much ground to cover for a generic API. Some ring adapters are
partially compliant and provide us with other mechanisms than an
`OutputStream`. Buffer pooling isn't really part of the SDK, it would mean
adding a dependency to the SDK and I haven't found a ready made solution anyway.

The good news is since we separate the SDK's public API from the ring specific
implementations with a protocol, a user can make adapters that fit exactly
their needs.

The SDK will provide some tools to help do this as well as sensible defaults.

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

This namespace provides the tools to implement the adapters IO logic using
the persistent buffer solution or the temporary buffer one. Here a permanent
buffer is a `BufferedWriter` that will be kept around for as long as the SSE
connection is open. A temporary buffer is a `StringBuilder` that is used to
assemble the event's text and is discarded as soon as the event's text is done
being concatenated.

> [!note]
> In all cases we hold onto the `GZIPOuputStream` when using compression.
> In other words, compression means at least 1 permanent buffer per connection.

The main function used by adapters is `->write-machinery`. It takes an
`OutputStream` and a map of options. It returns a map of 2 keys:

| returned key |                                                                                            |
| ------------ | ------------------------------------------------------------------------------------------ |
| `:writer`    | the wrapped outputstream depending on the options                                          |
| `:write!`    | a function that writes a SSE event to an output stream using a specific buffering strategy |

The options of the `->write-machinery` functions are:

|                    |                                                                   |
| ------------------ | ----------------------------------------------------------------- |
| `buffer-size`      | Size of the `java.lang.Appendable` used to write the event's text |
| `hold-write-buff?` | Whether to use a persistent buffer (true) or a temp one (false)   |
| `charset`          | the encoding for the SSE stream                                   |
| `gzip?`            | whether to gzip the stream                                        |
| `gzip-buffer-size` | buffer size for the GZIPOuputStream used under the hood           |

These options are available in the `->sse-response` function for of each
adapter. Each adapter can interpret these options a bit differently though.

#### Adapters specific implementations

##### Ring

With the generic ring adapter we always work with an `OutputStream`.
The implementation is straight forward. Depending on the options we wrap the
provided `OutputStream` with:

- a `GZIPOuputStream`: if `gzip?` is `true`, the buffer size can be controlled
  with `gzip-buffer-size`
- an `OutputStreamWriter` with a UTF-8 encoding or the provided `charset`
- a `BufferedWriter` if `hold-write-buff?` is `true`, its size can be set with
  `buffer-size`

For the generated `write!` function we write to the `BufferedWriter`
if `hold-write-buff?` is true. If not we assemble the event's text in a
`StringBuilder` (initial capacity can be determined with `buffer-size`) and
write the result in the `OutputStreamWriter` directly.

> [!note]
> The wrapped `OutputStreamWriter` lives as long as the SSE generator.
> `hold-write-buff?` determines if we use a the persistent or the temporary
> buffer solution.

##### Http-kit

Http-kit doesn't use an `OutputStream` as it's IO primitive. Our adapter is
then implemented in 2 ways depending on the value of the `gzip?` option.

- when `gzip?` is `false`, we concatenate event with using `StringBuilder` and
  send it with `org.http-kit.server/send!`.
  - `charset` has no effect, jvm default charset is used
  - `hold-write-buff?` has no effect
  - `buffer-size` controls the initial capacity of the `StringBuilder`
- when `gzip?` is `true` we wrap a `ByteArrayOutputStream` with a
  `GZIPOuputStream` and a `OutputStreamWriter`. When an event is written the
  compressed data is taken from the `ByteArrayOutputStream` and sent via
  Http-kit sending function.
  Here the options work similarly to the ring adapter:
  - `buffer-size` sets the size of the `BufferedWriter` or the `StringBuilder`
  - `hold-write-buff?` dictates whether we use a `BufferedWriter` or not
  - `gzip-buffer-size` sets the size of the `GZIPOuputStream`'s buffer
  - `charset` sets the encoding used for the `OutputStreamWriter`

## Beyond the SDK

The SDK tries to provide sensible defaults. Still the strategies provided may
not be optimal for your use case. The hope is that the source code and the docs
provide sufficient material for implementing other strategies in your own
adapters.
