//! Axum integration for Datastar.

use {
    crate::{Sse, TrySse, consts::DATASTAR_REQ_HEADER_STR, prelude::DatastarEvent},
    axum::{
        Json,
        body::{Body, Bytes, HttpBody},
        extract::{FromRequest, OptionalFromRequest, Query, Request},
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
    serde::{Deserialize, de::DeserializeOwned},
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

impl<T: DeserializeOwned, S: Send + Sync> OptionalFromRequest<S> for ReadSignals<T>
where
    Bytes: FromRequest<S>,
{
    type Rejection = Response;

    async fn from_request(req: Request, state: &S) -> Result<Option<Self>, Self::Rejection> {
        if req.headers().get(DATASTAR_REQ_HEADER_STR).is_none() {
            return Ok(None);
        }
        Ok(Some(
            <Self as FromRequest<S>>::from_request(req, state).await?,
        ))
    }
}

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
                    (http::StatusCode::BAD_REQUEST, "Failed to parse JSON str").into_response(),
                )?;

                serde_json::from_str(signals).map_err(
                    #[cfg_attr(not(feature = "tracing"), expect(unused_variables))]
                    |err| {
                        #[cfg(feature = "tracing")]
                        tracing::debug!(%err, "failed to parse JSON value");

                        (
                            http::StatusCode::BAD_REQUEST,
                            "Failed to parse JSON value from query",
                        )
                            .into_response()
                    },
                )
            }
            _ => {
                let Json(json) = <Json<T> as FromRequest<S>>::from_request(req, state)
                    .await
                    .map_err(
                        #[cfg_attr(not(feature = "tracing"), expect(unused_variables))]
                        |err| {
                            #[cfg(feature = "tracing")]
                            tracing::debug!(%err, "failed to parse JSON value from payload");

                            (
                                http::StatusCode::BAD_REQUEST,
                                "Failed to parse JSON value from payload",
                            )
                                .into_response()
                        },
                    )?;
                Ok(json)
            }
        }?;
        Ok(Self(json))
    }
}

#[cfg(test)]
mod tests {
    use {
        super::Sse,
        crate::{
            prelude::{MergeSignals, ReadSignals},
            testing::{self, Signals, base_test_server, test_server_merge_signal_complete},
        },
        async_stream::stream,
        axum::{
            Router,
            response::{Html, IntoResponse},
            routing::{get, post},
        },
        std::time::Duration,
        tokio::net::TcpListener,
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
    async fn sdk_base_test() -> Result<(), Box<dyn core::error::Error>> {
        let listener = TcpListener::bind("127.0.0.1:0").await?;
        let local_addr = listener.local_addr()?;

        let base_url = format!("http://{local_addr}");

        let (shutdown_tx, shutdown_rx) = tokio::sync::oneshot::channel();
        let shutdown_signal = async move {
            shutdown_rx
                .await
                .expect("to have no shutdown signal error on receival");
        };

        let app = Router::new()
            .route("/base/test", get(base_test_endpoint_required))
            .route("/base/test", post(base_test_endpoint_required))
            .route("/base/test-opt", get(base_test_endpoint_optional))
            .route("/base/test-opt", post(base_test_endpoint_optional));

        let (server_result_tx, server_result_rx) = tokio::sync::oneshot::channel();
        tokio::spawn(async move {
            server_result_tx
                .send(
                    axum::serve(listener, app)
                        .with_graceful_shutdown(shutdown_signal)
                        .await,
                )
                .expect("send axum serve result upstream over oneshot ch");
        });

        base_test_server(&base_url).await;

        shutdown_tx.send(()).expect("trigger shutdown signal");
        server_result_rx.await??;

        Ok(())
    }

    #[tokio::test]
    #[traced_test]
    async fn sdk_merge_complete_test() -> Result<(), Box<dyn core::error::Error>> {
        let listener = TcpListener::bind("127.0.0.1:0").await?;
        let local_addr = listener.local_addr()?;

        let base_url = format!("http://{local_addr}");

        let (shutdown_tx, shutdown_rx) = tokio::sync::oneshot::channel();
        let shutdown_signal = async move {
            shutdown_rx
                .await
                .expect("to have no shutdown signal error on receival");
        };

        let app = Router::new().route(
            "/rounds",
            get(async || {
                Sse(stream! {
                    for i in 0..100 {
                        tokio::time::sleep(Duration::from_millis((i % 7) * 5)).await;
                        yield MergeSignals::new(format!("{{current_round: {}}}", i*13));
                    }
                })
            }),
        );

        let (server_result_tx, server_result_rx) = tokio::sync::oneshot::channel();
        tokio::spawn(async move {
            server_result_tx
                .send(
                    axum::serve(listener, app)
                        .with_graceful_shutdown(shutdown_signal)
                        .await,
                )
                .expect("send axum serve result upstream over oneshot ch");
        });

        test_server_merge_signal_complete(&base_url).await;

        shutdown_tx.send(()).expect("trigger shutdown signal");
        server_result_rx.await??;

        Ok(())
    }
}
