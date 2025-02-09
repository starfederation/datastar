//! Axum integration for Datastar.

use {
    crate::{prelude::DatastarEvent, Sse, TrySse},
    axum::{
        body::{Body, Bytes, HttpBody},
        extract::{FromRequest, Query, Request},
        http::{self},
        response::{IntoResponse, Response},
    },
    core::{
        convert::Infallible,
        pin::Pin,
        task::{Context, Poll},
    },
    futures_util::{Stream, StreamExt},
    http_body::Frame,
    pin_project_lite::pin_project,
    serde::{de::DeserializeOwned, Deserialize},
    sync_wrapper::SyncWrapper,
};

pin_project! {
    struct SseBody<S> {
        #[pin]
        stream: SyncWrapper<S>,
    }
}

impl<S> IntoResponse for Sse<S>
where
    S: Stream<Item = DatastarEvent> + Send + 'static,
{
    fn into_response(self) -> Response {
        (
            [
                (http::header::CONTENT_TYPE, "text/event-stream"),
                (http::header::CACHE_CONTROL, "no-cache"),
                #[cfg(not(feature = "http2"))]
                (http::header::CONNECTION, "keep-alive"),
            ],
            Body::new(SseBody {
                stream: SyncWrapper::new(self.0.map(Ok::<_, Infallible>)),
            }),
        )
            .into_response()
    }
}

impl<S, E> IntoResponse for TrySse<S>
where
    S: Stream<Item = Result<DatastarEvent, E>> + Send + 'static,
    E: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    fn into_response(self) -> Response {
        (
            [
                (http::header::CONTENT_TYPE, "text/event-stream"),
                (http::header::CACHE_CONTROL, "no-cache"),
                #[cfg(not(feature = "http2"))]
                (http::header::CONNECTION, "keep-alive"),
            ],
            Body::new(SseBody {
                stream: SyncWrapper::new(self.0),
            }),
        )
            .into_response()
    }
}

impl<S, E> HttpBody for SseBody<S>
where
    S: Stream<Item = Result<DatastarEvent, E>>,
{
    type Data = Bytes;
    type Error = E;

    fn poll_frame(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Option<Result<Frame<Self::Data>, Self::Error>>> {
        let this = self.project();

        match this.stream.get_pin_mut().poll_next(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Some(Err(error))) => Poll::Ready(Some(Err(error))),
            Poll::Ready(None) => Poll::Ready(None),
            Poll::Ready(Some(Ok(event))) => {
                Poll::Ready(Some(Ok(Frame::data(event.to_string().into()))))
            }
        }
    }
}

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
        super::Sse,
        crate::{
            prelude::ReadSignals,
            testing::{self, Signals},
        },
        axum::{
            response::IntoResponse,
            routing::{get, post},
            Router,
        },
        tokio::net::TcpListener,
    };

    async fn test(ReadSignals(signals): ReadSignals<Signals>) -> impl IntoResponse {
        Sse(testing::test(signals.events))
    }

    #[tokio::test]
    async fn sdk_test() -> Result<(), Box<dyn core::error::Error>> {
        let listener = TcpListener::bind("127.0.0.1:3000").await?;
        let app = Router::new()
            .route("/test", get(test))
            .route("/test", post(test));

        axum::serve(listener, app).await?;

        Ok(())
    }
}
