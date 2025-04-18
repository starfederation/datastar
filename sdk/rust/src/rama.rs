//! Rama integration for Datastar.
//!
//! Learn more about rama at
//! <https://github.com/plabayo/rama>.

use {
    crate::{Sse, TrySse, prelude::DatastarEvent},
    bytes::Bytes,
    futures_util::{Stream, StreamExt},
    pin_project_lite::pin_project,
    rama::{
        error::BoxError,
        http::{
            Body, BodyExtractExt, IntoResponse, Method, Request, Response, StatusCode,
            dep::http_body::{Body as HttpBody, Frame},
            header,
            service::web::extract::{FromRequest, Query},
        },
    },
    serde::{Deserialize, de::DeserializeOwned},
    std::{
        convert::Infallible,
        pin::Pin,
        task::{Context, Poll},
    },
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
                (header::CONTENT_TYPE, "text/event-stream"),
                (header::CACHE_CONTROL, "no-cache"),
                (header::CONNECTION, "keep-alive"),
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
    E: Into<BoxError>,
{
    fn into_response(self) -> Response {
        (
            [
                (header::CONTENT_TYPE, "text/event-stream"),
                (header::CACHE_CONTROL, "no-cache"),
                (header::CONNECTION, "keep-alive"),
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
/// use datastar::rama::ReadSignals;
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

impl<T> FromRequest for ReadSignals<T>
where
    T: DeserializeOwned + Send + Sync + 'static,
{
    type Rejection = Response;

    async fn from_request(req: Request) -> Result<Self, Self::Rejection> {
        let json = match *req.method() {
            Method::GET => {
                let query =
                    Query::<DatastarParam>::parse_query_str(req.uri().query().unwrap_or(""))
                        .map_err(IntoResponse::into_response)?;

                let signals = query.0.datastar.as_str().ok_or_else(|| {
                    (StatusCode::BAD_REQUEST, "Failed to parse JSON").into_response()
                })?;

                serde_json::from_str(signals)
                    .map_err(|err| (StatusCode::BAD_REQUEST, err.to_string()).into_response())?
            }
            _ => req
                .into_body()
                .try_into_json()
                .await
                .map_err(|err| (StatusCode::BAD_REQUEST, err.to_string()).into_response())?,
        };

        Ok(Self(json))
    }
}

#[cfg(test)]
mod tests {
    use {
        super::Sse,
        crate::{
            rama::ReadSignals,
            testing::{self, Signals},
        },
        rama::{
            error::BoxError,
            http::{IntoResponse, server::HttpServer, service::web::Router},
            net::address::SocketAddress,
            rt::Executor,
        },
    };

    async fn test(ReadSignals(signals): ReadSignals<Signals>) -> impl IntoResponse {
        Sse(testing::test(signals.events))
    }

    #[tokio::test]
    #[ignore]
    async fn sdk_test() -> Result<(), BoxError> {
        HttpServer::auto(Executor::default())
            .listen(
                SocketAddress::local_ipv4(3000),
                Router::new().get("/test", test).post("/test", test),
            )
            .await?;

        Ok(())
    }
}
