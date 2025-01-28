# Datastar Zig SDK

An implementation of the Datastar SDK in Zig with framework integration for http.zig and tokamak.

## Testing

Run `zig build test`.

## Usage

```zig
const datastar = @import("datastar");

// Creates a new `ServerSentEventGenerator`.
var sse = try datastar.httpz.ServerSentEventGenerator.init(res);

// Merges HTML fragments into the DOM.
try sse.mergeFragments("<div id='question'>What do you put in a toaster?</div>", .{});

// Merges signals into the signals.
try sse.mergeSignals("{response: '', answer: 'bread'}", .{});
```