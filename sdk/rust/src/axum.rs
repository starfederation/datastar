//! Axum integration for Datastar.

use {
    crate::{
        prelude::{ExecuteScript, MergeFragments, MergeSignals, RemoveFragments, RemoveSignals},
        DatastarEvent,
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
        let mut event = Event::default().event(self.event.as_str());

        if let Some(id) = self.id {
            event = event.id(id);
        }

        event.retry(self.retry).data(self.data.join("\n"))
    }
}

impl IntoResponse for DatastarEvent {
    fn into_response(self) -> Response {
        (
            [
                (http::header::CONTENT_TYPE, "text/event-stream"),
                (http::header::CACHE_CONTROL, "no-cache"),
                (http::header::CONNECTION, "keep-alive"),
            ],
            self.to_string(),
        )
            .into_response()
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

            impl IntoResponse for $type {
                fn into_response(self) -> Response {
                    let event: DatastarEvent = self.into();
                    event.into_response()
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

// #[typle(Tuple for 1..=15)]
// impl<T: Tuple> IntoResponse for DatastarEvent {
//     fn into_response(&self) -> Res[p] {
//         typle_fold!(String::new(); i in .. => |acc| acc + &self[[i]].into().to_string())
//     }
// }

// macro_rules! impl_wrapped_in_foo {
//     ($($T:ident),*) => {
//         impl<$($T),*> WrappedInFoo for ($($T,)*) {
//             type Tup = ($(Foo<$T>,)*);
//         }
//     };
// }

// all_tuples!(impl_wrapped_in_foo, 0, 15, T);

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

// TODO: Rewrite tests
// #[cfg(test)]
// mod tests {
//     use {
//         crate::{
//             prelude::{
//                 ExecuteScript, FragmentMergeMode, MergeFragments, MergeSignals, ReadSignals,
//                 RemoveFragments, RemoveSignals,
//             },
//             ServerSentEventGenerator,
//         },
//         axum::{
//             http,
//             response::{IntoResponse, Response},
//             routing::{get, post},
//             Router,
//         },
//         core::time::Duration,
//         serde::Deserialize,
//         serde_json::Value,
//         tokio::net::TcpListener,
//     };

//     #[derive(Deserialize)]
//     #[serde(tag = "type", rename_all = "camelCase")]
//     pub enum ServerSentEvent {
//         #[serde(rename_all = "camelCase")]
//         ExecuteScript {
//             script: String,
//             event_id: Option<String>,
//             retry: Option<u64>,
//             attributes: Option<Value>,
//             auto_remove: Option<bool>,
//         },
//         #[serde(rename_all = "camelCase")]
//         MergeFragments {
//             fragments: String,
//             event_id: Option<String>,
//             retry: Option<u64>,
//             selector: Option<String>,
//             merge_mode: Option<String>,
//             settle_duration: Option<u64>,
//             use_view_transition: Option<bool>,
//         },
//         #[serde(rename_all = "camelCase")]
//         MergeSignals {
//             signals: Value,
//             event_id: Option<String>,
//             retry: Option<u64>,
//             only_if_missing: Option<bool>,
//         },
//         #[serde(rename_all = "camelCase")]
//         RemoveFragments {
//             selector: String,
//             event_id: Option<String>,
//             retry: Option<u64>,
//             settle_duration: Option<u64>,
//             use_view_transition: Option<bool>,
//         },
//         #[serde(rename_all = "camelCase")]
//         RemoveSignals {
//             paths: Vec<String>,
//             event_id: Option<String>,
//             retry: Option<u64>,
//         },
//     }

//     #[derive(Deserialize)]
//     pub struct Signals {
//         events: Vec<Value>,
//     }

//     async fn test(ReadSignals(signals): ReadSignals<Signals>) -> Response {
//         let events = signals
//             .events
//             .into_iter()
//             .map(|event| {
//                 println!("input: {:#?}", event);

//                 let event = serde_json::from_value::<ServerSentEvent>(event).unwrap();

//                 match event {
//                     ServerSentEvent::ExecuteScript {
//                         script,
//                         event_id,
//                         retry,
//                         attributes,
//                         auto_remove,
//                     } => {
//                         let attributes = attributes
//                             .map(|attrs| {
//                                 attrs
//                                     .as_object()
//                                     .unwrap()
//                                     .iter()
//                                     .map(|(name, value)| {
//                                         format!("{} {}", name, value.as_str().unwrap())
//                                     })
//                                     .collect()
//                             })
//                             .unwrap_or(vec![]);

//                         ExecuteScript {
//                             script,
//                             id: event_id,
//                             retry: Duration::from_millis(retry.unwrap_or(1000)),
//                             attributes,
//                             auto_remove: auto_remove.unwrap_or(true),
//                         }
//                         .send()
//                     }
//                     ServerSentEvent::MergeFragments {
//                         fragments,
//                         event_id,
//                         retry,
//                         selector,
//                         merge_mode,
//                         settle_duration,
//                         use_view_transition,
//                     } => {
//                         let merge_mode = merge_mode
//                             .map(|mode| match mode.as_str() {
//                                 "morph" => FragmentMergeMode::Morph,
//                                 "inner" => FragmentMergeMode::Inner,
//                                 "outer" => FragmentMergeMode::Outer,
//                                 "prepend" => FragmentMergeMode::Prepend,
//                                 "append" => FragmentMergeMode::Append,
//                                 "before" => FragmentMergeMode::Before,
//                                 "after" => FragmentMergeMode::After,
//                                 "upsertAttributes" => FragmentMergeMode::UpsertAttributes,
//                                 _ => unreachable!(),
//                             })
//                             .unwrap_or(FragmentMergeMode::Morph);

//                         MergeFragments {
//                             fragments,
//                             id: event_id,
//                             retry: Duration::from_millis(retry.unwrap_or(1000)),
//                             selector,
//                             merge_mode,
//                             settle_duration: Duration::from_millis(settle_duration.unwrap_or(300)),
//                             use_view_transition: use_view_transition.unwrap_or(false),
//                         }
//                         .send()
//                     }
//                     ServerSentEvent::MergeSignals {
//                         signals,
//                         event_id,
//                         retry,
//                         only_if_missing,
//                     } => MergeSignals {
//                         signals: serde_json::to_string(&signals).unwrap(),
//                         id: event_id,
//                         retry: Duration::from_millis(retry.unwrap_or(1000)),
//                         only_if_missing: only_if_missing.unwrap_or(false),
//                     }
//                     .send(),
//                     ServerSentEvent::RemoveFragments {
//                         selector,
//                         event_id,
//                         retry,
//                         settle_duration,
//                         use_view_transition,
//                     } => RemoveFragments {
//                         selector,
//                         id: event_id,
//                         retry: Duration::from_millis(retry.unwrap_or(1000)),
//                         settle_duration: Duration::from_millis(settle_duration.unwrap_or(300)),
//                         use_view_transition: use_view_transition.unwrap_or(false),
//                     }
//                     .send(),
//                     ServerSentEvent::RemoveSignals {
//                         paths,
//                         event_id,
//                         retry,
//                     } => RemoveSignals {
//                         paths,
//                         id: event_id,
//                         retry: Duration::from_millis(retry.unwrap_or(1000)),
//                     }
//                     .send(),
//                 }
//             })
//             .fold(String::new(), |acc, event| acc + &event);

//         println!("output: {}", events);

//         (
//             [
//                 (http::header::CONTENT_TYPE, "text/event-stream"),
//                 (http::header::CACHE_CONTROL, "no-cache"),
//             ],
//             events,
//         )
//             .into_response()
//     }

//     #[tokio::test]
//     async fn sdk_test() -> Result<(), Box<dyn std::error::Error>> {
//         let listener = TcpListener::bind("127.0.0.1:3000").await?;
//         let app = Router::new()
//             .route("/test", get(test))
//             .route("/test", post(test));

//         axum::serve(listener, app).await?;

//         Ok(())
//     }
// }
