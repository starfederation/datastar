# Datastar Rust SDK

An implementation of the [Datastar] SDK in Rust
with framework integration for [Axum], [Rocket] and [Rama].

# Usage

```rust
use datastar::prelude::*;
use async_stream::stream;

Sse(stream! {
    // Merges HTML fragments into the DOM.
    yield MergeFragments::new("<div id='question'>What do you put in a toaster?</div>").into();

    // Merges signals into the signals.
    yield MergeSignals::new("{response: '', answer: 'bread'}").into();
})
```

[Datastar]: https://data-star.dev
[Axum]: https://github.com/tokio-rs/axum
[Rocket]: https://github.com/rwf2/rocket
[Rama]: https://github.com/plabayo/rama
