//! Datastar is a Rust implementation of the [Datastar](https://data-star.dev) SDK specification.

#![forbid(missing_docs)]
#![forbid(missing_debug_implementations)]

#[cfg(feature = "axum")]
pub mod axum;
#[cfg(feature = "rocket")]
pub mod rocket;

pub mod execute_script;
pub mod merge_fragments;
pub mod merge_signals;
pub mod remove_fragments;
pub mod remove_signals;

#[cfg(test)]
mod testing;

mod consts;

/// The prelude for the `datastar` crate
pub mod prelude {
    #[cfg(feature = "axum")]
    pub use crate::axum::ReadSignals;
    pub use crate::{
        consts::FragmentMergeMode, execute_script::ExecuteScript, merge_fragments::MergeFragments,
        merge_signals::MergeSignals, remove_fragments::RemoveFragments,
        remove_signals::RemoveSignals, DatastarEvent, Sse, TrySse,
    };
}

use {
    core::{error::Error, fmt::Display, time::Duration},
    futures_util::{Stream, TryStream},
};

/// [`DatastarEvent`] is a struct that represents a generic Datastar event.
/// All Datastar events implement `Into<DatastarEvent>`.
#[derive(Debug)]
pub struct DatastarEvent {
    /// `event` is the type of event.
    pub event: consts::EventType,
    /// `id` is can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    pub id: Option<String>,
    /// `retry` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
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

/// [`Sse`] is a wrapper around a stream of [`DatastarEvent`]s.
#[derive(Debug)]
pub struct Sse<S>(pub S)
where
    S: Stream<Item = DatastarEvent> + Send + 'static;

/// [`TrySse`] is a wrapper around a stream of [`DatastarEvent`]s that can fail.
#[derive(Debug)]
pub struct TrySse<S>(pub S)
where
    S: TryStream<Ok = DatastarEvent> + Send + 'static,
    S::Error: Into<Box<dyn Error + Send + Sync>>;
