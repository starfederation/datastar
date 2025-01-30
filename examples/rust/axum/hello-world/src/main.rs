use {
    axum::{
        response::{sse::Event, Html, IntoResponse, Sse},
        routing::get,
        Router,
    },
    datastar::{
        prelude::{MergeFragments, ReadSignals},
        DatastarEvent,
    },
    futures::{stream, Stream},
    serde::Deserialize,
    std::{convert::Infallible, error::Error},
    tokio_stream::StreamExt,
    tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt},
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| {
                format!("{}=debug,tower_http=debug", env!("CARGO_CRATE_NAME")).into()
            }),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    let app = Router::new()
        .route("/", get(index))
        .route("/hello-world", get(hello_world));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .unwrap();

    tracing::debug!("listening on {}", listener.local_addr().unwrap());

    axum::serve(listener, app).await.unwrap();

    Ok(())
}

async fn index() -> Html<String> {
    Html(std::fs::read_to_string("hello-world.html").unwrap())
}

const MESSAGE: &str = "Hello, world!";

#[derive(Deserialize)]
pub struct Signals {
    pub delay: u64,
}

async fn hello_world(
    ReadSignals(signals): ReadSignals<Signals>,
) -> Sse<impl Stream<Item = Result<Event, Infallible>>> {
    let mut i = 0;

    let stream = stream::repeat_with(move || -> Result<Event, Infallible> {
        i += 1;
        Ok(MergeFragments::new(format!("<div id='message'>{}</div>", &MESSAGE[0..i])).into())
    })
    .throttle(std::time::Duration::from_millis(signals.delay))
    .take(MESSAGE.len());

    axum::response::Sse::new(stream)
}
