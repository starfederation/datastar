# Datastar Rust SDK

An implementation of the [Datastar] SDK in Rust
with framework integration for [Axum] and [Rocket].

[Rama](https://github.com/plabayo/rama) has its own SDK implementation defined as a [Rama module for Datastar](https://ramaproxy.org/docs/rama/http/sse/datastar/index.html) as can be seen in action in [this example](https://github.com/plabayo/rama/blob/main/examples/http_sse_datastar_hello.rs).

# Usage

Examples for the Rust sdk can be found in [`../../examples/rust`](../../examples/rust), where
you find examples that you can run youself for the supported
frameworks [Axum] and [Rocket].

[Datastar]: https://data-star.dev
[Axum]: https://github.com/tokio-rs/axum
[Rocket]: https://github.com/rwf2/rocket
