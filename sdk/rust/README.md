# Datastar Rust SDK

An implementation of the [Datastar] SDK in Rust
with framework integration for [Axum], [Rocket] and [Rama].

# Usage

```rust
use datastar::prelude::*;
use async_stream::stream;
use futures_util::stream::Stream;

async fn handle() -> Sse<impl Stream<Item = DatastarEvent> + Send + 'static> {
    Sse(stream! {
        // Merges HTML fragments into the DOM.
        yield MergeFragments::new("<div id='question'>What do you put in a toaster?</div>").into();

        // Merges signals into the signals.
        yield MergeSignals::new("{response: '', answer: 'bread'}").into();
    })
}
```

More usage examples for the Rust sdk can be found in [`../../examples/rust`](../../examples/rust), where
you find examples that you can run youself for the supported
frameworks [Axum], [Rocket] and [Rama].

[Datastar]: https://data-star.dev
[Axum]: https://github.com/tokio-rs/axum
[Rocket]: https://github.com/rwf2/rocket
[Rama]: https://github.com/plabayo/rama
