# Datastar Zig SDK

An implementation of the Datastar SDK in Zig with framework integration for http.zig and tokamak.

## Testing

Run `zig build test`.

## Usage

Install with `zig fetch --save git+https://github.com/starfederation/datastar-zig`

```zig
const datastar = @import("datastar").httpz;

// Creates a new `ServerSentEventGenerator`.
var sse = try datastar.ServerSentEventGenerator.init(res);

// Merges HTML fragments into the DOM.
try sse.mergeFragments("<div id='question'>What do you put in a toaster?</div>", .{});

// Merges signals into the signals.
try sse.mergeSignals(.{ .response = "", .answer = "bread" }, .{});
```

Full examples at https://github.com/starfederation/datastar/tree/main/examples/zig