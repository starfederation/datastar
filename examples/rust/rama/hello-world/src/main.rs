use {
    async_stream::stream,
    core::time::Duration,
    datastar::{
        Sse,
        prelude::{MergeFragments, ReadSignals},
    },
    rama::{
        error::BoxError,
        http::{
            server::HttpServer,
            service::web::{
                Router,
                response::{Html, IntoResponse},
            },
        },
        rt::Executor,
    },
    serde::Deserialize,
    tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt},
};

#[tokio::main]
async fn main() -> Result<(), BoxError> {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| format!("{}=debug", env!("CARGO_CRATE_NAME")).into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    let app = Router::new()
        .get("/", index)
        .get("/hello-world", hello_world);

    tracing::debug!("listening on 127.0.0.1:3000");
    HttpServer::auto(Executor::default())
        .listen("127.0.0.1:3000", app)
        .await?;

    Ok(())
}

async fn index() -> Html<&'static str> {
    Html(include_str!("../hello-world.html"))
}

const MESSAGE: &str = "Hello, world!";

#[derive(Deserialize)]
pub struct Signals {
    pub delay: u64,
}

async fn hello_world(ReadSignals(signals): ReadSignals<Signals>) -> impl IntoResponse {
    Sse(stream! {
        for i in 0..MESSAGE.len() {
            yield MergeFragments::new(format!("<div id='message'>{}</div>", &MESSAGE[0..i + 1]));
            tokio::time::sleep(Duration::from_millis(signals.delay)).await;
        }
    })
}
