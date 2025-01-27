# Datastar Zig SDK

An implementation of the Datastar SDK in Zig. Currently, this only has framework integration for http.zig.

## Testing

Run `zig build test`.

## Usage

```zig
const datastar = @import("datastar");

// Creates a new `ServerSentEventGenerator`.
var sse = datastar.ServerSentEventGenerator.init(res);

// Merges HTML fragments into the DOM.
sse.mergeFragments("<div id='question'>What do you put in a toaster?</div>", .{});

// Merges signals into the signals.
sse.mergeSignals("{response: '', answer: 'bread'}", .{})
```