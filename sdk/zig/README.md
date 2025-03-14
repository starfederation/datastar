# Datastar Zig SDK

The Datastar SDK in Zig, with support for http.zig and tokamak.

## Installation

Install with `zig fetch --save git+https://github.com/starfederation/datastar-zig` and add datastar as a dependency.

```zig
const datastar = b.dependency("datastar", .{
    .target = target,
    .optimize = optimize,
    .framework = .httpz, // or .tokamak
}).module("datastar");

exe.root_module.addImport("datastar", datastar);
```

## Usage
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