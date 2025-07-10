use {
    async_stream::stream,
    axum::{
        Router,
        response::{IntoResponse, Sse},
        routing::{MethodFilter, on},
    },
    core::{convert::Infallible, error::Error, time::Duration},
    datastar::{
        axum::ReadSignals,
        consts,
        prelude::{ExecuteScript, PatchElements, PatchSignals},
    },
    indexmap::IndexMap,
    serde::Deserialize,
    serde_json::Value,
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

    let app = Router::new().route("/test", on(MethodFilter::GET.or(MethodFilter::POST), test));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:9200")
        .await
        .unwrap();

    tracing::debug!("listening on {}", listener.local_addr().unwrap());

    axum::serve(listener, app).await.unwrap();

    Ok(())
}

#[derive(Deserialize)]
pub struct TestCase {
    pub events: Vec<TestCaseEvent>,
}

#[derive(Deserialize)]
#[serde(tag = "type")]
pub enum TestCaseEvent {
    #[serde(alias = "executeScript")]
    ExecuteScript {
        script: String,
        #[serde(alias = "eventId")]
        event_id: Option<String>,
        #[serde(alias = "retryDuration")]
        retry_duration: Option<u64>,
        attributes: Option<IndexMap<String, Value>>,
        #[serde(alias = "autoRemove")]
        auto_remove: Option<bool>,
    },
    #[serde(rename = "patchElements")]
    PatchElements {
        elements: Option<String>,
        #[serde(alias = "eventId")]
        event_id: Option<String>,
        #[serde(alias = "retryDuration")]
        retry_duration: Option<u64>,
        selector: Option<String>,
        mode: Option<String>,
        #[serde(alias = "useViewTransition")]
        use_view_transition: Option<bool>,
    },
    #[serde(rename = "patchSignals")]
    PatchSignals {
        signals: Option<IndexMap<String, Value>>,
        #[serde(alias = "signals-raw")]
        signals_raw: Option<String>,
        #[serde(alias = "eventId")]
        event_id: Option<String>,
        #[serde(alias = "retryDuration")]
        retry_duration: Option<u64>,
        #[serde(alias = "onlyIfMissing")]
        only_if_missing: Option<bool>,
    },
}

async fn test(ReadSignals(test_case): ReadSignals<TestCase>) -> impl IntoResponse {
    Sse::new(stream! {
        for event in test_case.events {
            let sse_event = match event {
                TestCaseEvent::ExecuteScript { script, event_id, retry_duration, attributes, auto_remove } => {
                    ExecuteScript {
                        script,
                        id: event_id,
                        retry: Duration::from_millis(retry_duration.unwrap_or(consts::DEFAULT_SSE_RETRY_DURATION)),
                        auto_remove,
                        attributes: attributes.map(|attributes| {
                            attributes.into_iter().map(|(key, value)| format!("{key}=\"{}\"", value.to_string().trim_matches('"'))).collect()
                        }).unwrap_or_default(),
                    }.into_datastar_event().write_as_axum_sse_event()
                }
                TestCaseEvent::PatchElements { elements, event_id, retry_duration, mode, selector, use_view_transition } => {
                    PatchElements { id: event_id, retry: Duration::from_millis(retry_duration.unwrap_or(consts::DEFAULT_SSE_RETRY_DURATION)), elements, selector, mode: match mode.as_deref().unwrap_or_default() {
                        "outer" => consts::ElementPatchMode::Outer,
                        "inner" => consts::ElementPatchMode::Inner,
                        "remove" => consts::ElementPatchMode::Remove,
                        "replace" => consts::ElementPatchMode::Replace,
                        "prepend" => consts::ElementPatchMode::Prepend,
                        "append" => consts::ElementPatchMode::Append,
                        "before" => consts::ElementPatchMode::Before,
                        "after" => consts::ElementPatchMode::After,
                        _ => consts::ElementPatchMode::Outer,
                    }, use_view_transition: use_view_transition.unwrap_or_default() }.into_datastar_event().write_as_axum_sse_event()
                },
                TestCaseEvent::PatchSignals { signals, signals_raw, event_id, retry_duration, only_if_missing } => {
                    PatchSignals {
                        id: event_id,
                        retry: Duration::from_millis(retry_duration.unwrap_or(consts::DEFAULT_SSE_RETRY_DURATION)),
                        signals: signals_raw.unwrap_or_else(|| signals.map(|s| serde_json::to_string(&s).unwrap_or_default()).unwrap_or_default()),
                        only_if_missing: only_if_missing.unwrap_or_default(),
                    }.into_datastar_event().write_as_axum_sse_event()
                },
            };

            yield Ok::<_, Infallible>(sse_event);
        }
    })
}
