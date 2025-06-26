//! Datastar is a Rust implementation of the [Datastar](https://data-star.dev) SDK specification.

#![forbid(missing_docs)]
#![forbid(missing_debug_implementations)]

#[cfg(feature = "axum")]
pub mod axum;
#[cfg(feature = "rocket")]
pub mod rocket;

pub mod patch_elements;
pub mod patch_signals;

#[doc = include_str!("../README.md")]
#[cfg(doctest)]
#[expect(unused)]
struct ReadmeDoctests;

pub mod consts;

/// The prelude for the `datastar` crate
pub mod prelude {
    pub use crate::{
        DatastarEvent, consts::ElementPatchMode, patch_elements::PatchElements,
        patch_signals::PatchSignals,
    };
}

use core::{fmt::Display, time::Duration};

/// [`DatastarEvent`] is a struct that represents a generic Datastar event.
/// All Datastar events implement `Into<DatastarEvent>`.
#[derive(Debug)]
pub struct DatastarEvent {
    /// `event` is the type of event.
    pub event: consts::EventType,
    /// `id` is can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see <https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id>
    pub id: Option<String>,
    /// `retry` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see <https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry>
    pub retry: Duration,
    /// `data` is the data that is sent with the event.
    pub data: Vec<String>,
}

impl Display for DatastarEvent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "event: {}", self.event.as_str())?;

        if let Some(id) = &self.id {
            writeln!(f, "id: {}", id)?;
        }

        let millis = self.retry.as_millis();
        if millis != consts::DEFAULT_SSE_RETRY_DURATION as u128 {
            writeln!(f, "retry: {}", millis)?;
        }

        for line in &self.data {
            writeln!(f, "data: {}", line)?;
        }

        write!(f, "\n\n")?;

        Ok(())
    }
}
