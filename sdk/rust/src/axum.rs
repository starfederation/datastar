//! Axum integration for Datastar.

use {
    crate::prelude::*,
    axum::{
        body::Bytes,
        extract::{FromRequest, Query, Request},
        http::{self},
        response::{IntoResponse as AxumIntoResponse, Response},
    },
    serde::{Deserialize, de::DeserializeOwned},
};

fn into_response<T: ServerSentEventGenerator>(event: T) -> Response {
    (
        [
            (http::header::CONTENT_TYPE, mime::TEXT_EVENT_STREAM.as_ref()),
            (http::header::CACHE_CONTROL, "no-cache"),
        ],
        event.send(),
    )
        .into_response()
}

/// [`IntoResponse`] is a trait that converts a type into an Axum response. Import this instead of `axum::response::IntoResponse`.
pub trait IntoResponse {
    /// Create a response.
    fn into_response(self) -> Response;
}

impl<T: ServerSentEventGenerator> IntoResponse for T {
    fn into_response(self) -> Response {
        into_response(self)
    }
}

#[derive(Deserialize)]
struct DatastarQuery {
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
        if req.headers().get("datastar-request").is_none() {
            return Err((
                http::StatusCode::BAD_REQUEST,
                "Missing 'datastar-request' header",
            )
                .into_response());
        }

        let json = match *req.method() {
            http::Method::GET => {
                let query = Query::<DatastarQuery>::from_request(req, state)
                    .await
                    .map_err(AxumIntoResponse::into_response)?;

                let signals = query.0.datastar.as_str().ok_or(
                    (http::StatusCode::BAD_REQUEST, "Failed to parse JSON").into_response(),
                )?;

                serde_json::from_str(signals)
            }
            _ => {
                let body = Bytes::from_request(req, state)
                    .await
                    .map_err(AxumIntoResponse::into_response)?;

                serde_json::from_slice(&body)
            }
        }
        .map_err(|_| (http::StatusCode::BAD_REQUEST, "Failed to parse JSON").into_response())?;

        Ok(Self(json))
    }
}
