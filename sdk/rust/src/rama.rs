//! Rama integration for Datastar.
//!
//! Learn more about rama at
//! <https://github.com/plabayo/rama>.

use {
    crate::{Sse, TrySse, consts::DATASTAR_REQ_HEADER_STR, prelude::DatastarEvent},
    bytes::Bytes,
    futures_util::{Stream, StreamExt},
    pin_project_lite::pin_project,
    rama::{
        error::BoxError,
        http::{
            Body, BodyExtractExt, IntoResponse, Method, Request, Response, StatusCode,
            dep::http_body::{Body as HttpBody, Frame},
            header,
            service::web::extract::{FromRequest, OptionalFromRequest, Query},
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

impl<S, I> IntoResponse for Sse<S>
where
    S: Stream<Item = I> + Send + 'static,
    I: Into<DatastarEvent> + Send + 'static,
{
    #[inline]
    fn into_response(self) -> Response {
        TrySse(self.0.map(Ok::<_, Infallible>)).into_response()
    }
}

impl<S, I, E> IntoResponse for TrySse<S>
where
    S: Stream<Item = Result<I, E>> + Send + 'static,
    I: Into<DatastarEvent> + Send + 'static,
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
                stream: SyncWrapper::new(self.0.map(|r| r.map(Into::into))),
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
                    tracing::debug!("failed to get datastar query value from GET request");
                    (StatusCode::BAD_REQUEST, "Failed to parse JSON").into_response()
                })?;

                serde_json::from_str(signals)
                    .map_err(|err| {
                        tracing::debug!(%err, "failed to parse datastar query json value from GET request");
                        (StatusCode::BAD_REQUEST, err.to_string()).into_response()}
                    )?
            }
            _ => req.into_body().try_into_json().await.map_err(|err| {
                tracing::debug!(%err, "failed to parse datastar json payload from POST request");
                (StatusCode::BAD_REQUEST, err.to_string()).into_response()
            })?,
        };

        Ok(Self(json))
    }
}

impl<T> OptionalFromRequest for ReadSignals<T>
where
    T: DeserializeOwned + Send + Sync + 'static,
{
    type Rejection = Response;

    async fn from_request(req: Request) -> Result<Option<Self>, Self::Rejection> {
        if req.headers().get(DATASTAR_REQ_HEADER_STR).is_none() {
            tracing::trace!(
                "no datastar request header present: returning no read signals as such"
            );
            return Ok(None);
        }
        Ok(Some(<Self as FromRequest>::from_request(req).await?))
    }
}

#[cfg(test)]
mod tests {
    use {
        super::Sse,
        crate::{
            rama::ReadSignals,
            testing::{self, Signals, base_test_server},
        },
        rama::{
            error::BoxError,
            graceful::Shutdown,
            http::{IntoResponse, response::Html, server::HttpServer, service::web::Router},
            net::address::SocketAddress,
            rt::Executor,
            tcp::server::TcpListener,
        },
        tracing_test::traced_test,
    };

    async fn base_test_endpoint_required(
        ReadSignals(signals): ReadSignals<Signals>,
    ) -> impl IntoResponse {
        Sse(testing::test(signals.events))
    }

    async fn base_test_endpoint_optional(
        signals: Option<ReadSignals<Signals>>,
    ) -> impl IntoResponse {
        match signals {
            Some(ReadSignals(signals)) => Sse(testing::test(signals.events)).into_response(),
            None => Html("<p>Hello</p>").into_response(),
        }
    }

    #[tokio::test]
    #[traced_test]
    async fn sdk_base_test() -> Result<(), BoxError> {
        let listener = TcpListener::bind(SocketAddress::local_ipv4(0)).await?;
        let local_addr = listener.local_addr()?;

        let base_url = format!("http://{local_addr}");

        let (shutdown_tx, shutdown_rx) = tokio::sync::oneshot::channel();
        let shutdown = Shutdown::new(shutdown_rx);

        let listener_guard = shutdown.guard();

        let server_task = tokio::spawn(async move {
            listener
                .serve_graceful(
                    listener_guard,
                    HttpServer::auto(Executor::default()).service(
                        Router::new()
                            .get("/base/test", base_test_endpoint_required)
                            .post("/base/test", base_test_endpoint_required)
                            .get("/base/test-opt", base_test_endpoint_optional)
                            .post("/base/test-opt", base_test_endpoint_optional),
                    ),
                )
                .await;
        });

        base_test_server(&base_url).await;

        shutdown_tx.send(()).expect("trigger shutdown signal");
        server_task.await.expect("server task to finish gracefully");

        Ok(())
    }
}
