//! Axum integration for Datastar.

use {
    crate::{
        consts::{self, DATASTAR_REQ_HEADER_STR},
        prelude::{DatastarEvent, PatchElements, PatchSignals},
    },
    axum::{
        Json,
        body::Bytes,
        extract::{FromRequest, OptionalFromRequest, Query, Request},
        http::{self},
        response::{IntoResponse, Response, sse::Event},
    },
    serde::{Deserialize, de::DeserializeOwned},
    std::fmt::Write,
};

impl PatchElements {
    /// Write this [`PatchElements`] into an Axum SSE [`Event`].
    pub fn write_as_axum_sse_event(&self) -> Event {
        self.as_datastar_event().write_as_axum_sse_event()
    }
}

impl PatchSignals {
    /// Write this [`PatchSignals`] into an Axum SSE [`Event`].
    pub fn write_as_axum_sse_event(&self) -> Event {
        self.as_datastar_event().write_as_axum_sse_event()
    }
}

impl DatastarEvent {
    /// Turn this [`DatastarEvent`] into an Axum SSE [`Event`].
    pub fn write_as_axum_sse_event(&self) -> Event {
        let event = Event::default().event(self.event.as_str());

        let event = if self.retry.as_millis() != (consts::DEFAULT_SSE_RETRY_DURATION as u128) {
            event.retry(self.retry)
        } else {
            event
        };

        let event = match self.id.as_deref() {
            Some(id) => event.id(id),
            None => event,
        };

        let mut data = String::with_capacity(
            (self.data.iter().map(|s| s.len()).sum::<usize>() + self.data.len()).saturating_sub(1),
        );

        let mut sep = "";
        for line in self.data.iter() {
            // Assumption: std::fmt::write does not fail ever for [`String`].
            let _ = write!(&mut data, "{sep}{line}");
            sep = "\n";
        }

        event.data(data)
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
/// use datastar::axum::ReadSignals;
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
