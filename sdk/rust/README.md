# Datastar Rust SDK

An implementation of the Datastar SDK in Rust with framework integration for Axum and Rocket.

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