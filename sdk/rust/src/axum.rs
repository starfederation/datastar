//! Axum integration for Datastar.

use {
    crate::prelude::{
        DatastarEvent, ExecuteScript, MergeFragments, MergeSignals, RemoveFragments, RemoveSignals,
    },
    axum::{
        body::Bytes,
        extract::{FromRequest, Query, Request},
        http::{self},
        response::{sse::Event, IntoResponse, Response},
    },
    serde::{de::DeserializeOwned, Deserialize},
};

impl Into<Event> for DatastarEvent {
    fn into(self) -> Event {
        let mut event = Event::default();

        if let Some(id) = self.id {
            event = event.id(id);
        }

        event
            .event(self.event.as_str())
            .retry(self.retry)
            .data(self.data.join("\n"))
    }
}

macro_rules! impls {
    ($($type:ty),*) => {
        $(
            impl Into<Event> for $type {
                fn into(self) -> Event {
                    let event: DatastarEvent = self.into();
                    event.into()
                }
            }
        )*
    };
}

impls!(
    ExecuteScript,
    MergeFragments,
    MergeSignals,
    RemoveFragments,
    RemoveSignals
);

#[derive(Deserialize)]
struct DatastarParam {
    datastar: serde_json::Value,
}

/// [`ReadSignals`] is a request extractor that reads datastar signals from the request.
///
/// # Examples
///
/// ```
/// use datastar::prelude::ReadSignals;
/// use serde::Deserialize;
///
/// #[derive(Deserialize)]
/// struct Signals {
///     foo: String,
///     bar: i32,
/// }
///
/// async fn handler(ReadSignals(signals): ReadSignals<Signals>) {
///    println!("foo: {}", signals.foo);
///    println!("bar: {}", signals.bar);
/// }
///
/// ```
#[derive(Debug)]
pub struct ReadSignals<T: DeserializeOwned>(pub T);

impl<T: DeserializeOwned, S: Send + Sync> FromRequest<S> for ReadSignals<T>
where
    Bytes: FromRequest<S>,
{
    type Rejection = Response;

    async fn from_request(req: Request, state: &S) -> Result<Self, Self::Rejection> {
        let json = match *req.method() {
            http::Method::GET => {
                let query = Query::<DatastarParam>::from_request(req, state)
                    .await
                    .map_err(IntoResponse::into_response)?;

                let signals = query.0.datastar.as_str().ok_or(
                    (http::StatusCode::BAD_REQUEST, "Failed to parse JSON").into_response(),
                )?;

                serde_json::from_str(signals)
            }
            _ => {
                let body = Bytes::from_request(req, state)
                    .await
                    .map_err(IntoResponse::into_response)?;

                serde_json::from_slice(&body)
            }
        }
        .map_err(|_| (http::StatusCode::BAD_REQUEST, "Failed to parse JSON").into_response())?;

        Ok(Self(json))
    }
}

#[cfg(test)]
mod tests {
    use {
        crate::{
            consts,
            prelude::{
                ExecuteScript, FragmentMergeMode, MergeFragments, MergeSignals, ReadSignals,
                RemoveFragments, RemoveSignals,
            },
            testing::{Signals, TestEvent},
        },
        async_stream::try_stream,
        axum::{
            response::{sse::Event, Sse},
            routing::{get, post},
            Router,
        },
        core::{convert::Infallible, time::Duration},
        tokio::net::TcpListener,
        tokio_stream::Stream,
    };

    async fn test(
        ReadSignals(signals): ReadSignals<Signals>,
    ) -> Sse<impl Stream<Item = Result<Event, Infallible>>> {
        Sse::new(try_stream! {
            for event in signals.events {
                let event: TestEvent = serde_json::from_value(event).unwrap();

                yield match event {
                    TestEvent::ExecuteScript {
                        script,
                        event_id,
                        retry_duration,
                        attributes,
                        auto_remove,
                    } => {
                        let attributes = attributes
                            .map(|attrs| {
                                attrs
                                    .as_object()
                                    .unwrap()
                                    .iter()
                                    .map(|(name, value)| {
                                        format!("{} {}", name, value.as_str().unwrap())
                                    })
                                    .collect()
                            })
                            .unwrap_or(vec![]);

                        ExecuteScript {
                            script,
                            id: event_id,
                            retry: Duration::from_millis(retry_duration.unwrap_or(consts::DEFAULT_SSE_RETRY_DURATION)),
                            attributes,
                            auto_remove: auto_remove.unwrap_or(consts::DEFAULT_EXECUTE_SCRIPT_AUTO_REMOVE),
                        }
                        .into()
                    }
                    TestEvent::MergeFragments {
                        fragments,
                        event_id,
                        retry_duration,
                        selector,
                        merge_mode,
                        settle_duration,
                        use_view_transition,
                    } => {
                        let merge_mode = merge_mode
                            .map(|mode| match mode.as_str() {
                                "morph" => FragmentMergeMode::Morph,
                                "inner" => FragmentMergeMode::Inner,
                                "outer" => FragmentMergeMode::Outer,
                                "prepend" => FragmentMergeMode::Prepend,
                                "append" => FragmentMergeMode::Append,
                                "before" => FragmentMergeMode::Before,
                                "after" => FragmentMergeMode::After,
                                "upsertAttributes" => FragmentMergeMode::UpsertAttributes,
                                _ => unreachable!(),
                            })
                            .unwrap_or_default();

                        MergeFragments {
                            fragments,
                            id: event_id,
                            retry: Duration::from_millis(retry_duration.unwrap_or(consts::DEFAULT_SSE_RETRY_DURATION)),
                            selector,
                            merge_mode,
                            settle_duration: Duration::from_millis(settle_duration.unwrap_or(consts::DEFAULT_FRAGMENTS_SETTLE_DURATION)),
                            use_view_transition: use_view_transition.unwrap_or(consts::DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS),
                        }
                        .into()
                    }
                    TestEvent::MergeSignals {
                        signals,
                        event_id,
                        retry_duration,
                        only_if_missing,
                    } => MergeSignals {
                        signals: serde_json::to_string(&signals).unwrap(),
                        id: event_id,
                        retry: Duration::from_millis(retry_duration.unwrap_or(consts::DEFAULT_SSE_RETRY_DURATION)),
                        only_if_missing: only_if_missing.unwrap_or(consts::DEFAULT_MERGE_SIGNALS_ONLY_IF_MISSING),
                    }
                    .into(),
                    TestEvent::RemoveFragments {
                        selector,
                        event_id,
                        retry_duration,
                        settle_duration,
                        use_view_transition,
                    } => RemoveFragments {
                        selector,
                        id: event_id,
                        retry: Duration::from_millis(retry_duration.unwrap_or(consts::DEFAULT_SSE_RETRY_DURATION)),
                        settle_duration: Duration::from_millis(settle_duration.unwrap_or(consts::DEFAULT_FRAGMENTS_SETTLE_DURATION)),
                        use_view_transition: use_view_transition.unwrap_or(consts::DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS),
                    }
                    .into(),
                    TestEvent::RemoveSignals {
                        paths,
                        event_id,
                        retry_duration,
                    } => RemoveSignals {
                        paths,
                        id: event_id,
                        retry: Duration::from_millis(retry_duration.unwrap_or(consts::DEFAULT_SSE_RETRY_DURATION)),
                    }
                    .into(),
                }
            }
        })
    }

    #[tokio::test]
    async fn sdk_test() -> Result<(), Box<dyn std::error::Error>> {
        let listener = TcpListener::bind("127.0.0.1:3000").await?;
        let app = Router::new()
            .route("/test", get(test))
            .route("/test", post(test));

        axum::serve(listener, app).await?;

        Ok(())
    }
}
