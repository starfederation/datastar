use {
    async_stream::stream,
    axum::{
        response::{Html, IntoResponse},
        routing::get,
        Router,
    },
    core::{error::Error, time::Duration},
    datastar::{
        prelude::{MergeFragments, ReadSignals},
        Sse,
    },
    serde::Deserialize,
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

async fn hello_world(ReadSignals(signals): ReadSignals<Signals>) -> impl IntoResponse {
    Sse(stream! {
        for i in 0..MESSAGE.len() {
            yield MergeFragments::new(format!("<div id='message'>{}</div>", &MESSAGE[0..i + 1])).into();
            tokio::time::sleep(Duration::from_millis(signals.delay)).await;
        }
    })
}
