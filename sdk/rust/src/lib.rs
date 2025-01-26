//! Datastar is a Rust implementation of the [Datastar](https://data-star.dev) SDK specification.

#![forbid(missing_docs)]
#![forbid(missing_debug_implementations)]

#[cfg(feature = "axum")]
pub mod axum;
mod consts;
pub mod execute_script;
pub mod merge_fragments;
pub mod merge_signals;
pub mod remove_fragments;
pub mod remove_signals;

///
pub mod prelude {
    #[cfg(feature = "axum")]
    pub use crate::axum::{IntoResponse, ReadSignals};
    pub use crate::{
        consts::FragmentMergeMode, execute_script::ExecuteScript, merge_fragments::MergeFragments,
        merge_signals::MergeSignals, remove_fragments::RemoveFragments,
        remove_signals::RemoveSignals, ServerSentEventGenerator,
    };
}

use typle::{typle, typle_fold};

/// [`ServerSentEventGenerator`] is a trait that represents a Datastar event.
pub trait ServerSentEventGenerator {
    /// Serializes the event into a Server-Sent ServerSentEventGenerator (SSE) string.
    fn send(&self) -> String;
}

#[typle(Tuple for 1..=15)]
impl<T: Tuple + ServerSentEventGenerator> ServerSentEventGenerator for T {
    fn send(&self) -> String {
        typle_fold!(String::new(); i in .. => |acc| acc + &self[[i]].send())
    }
}

impl<T: ServerSentEventGenerator> ServerSentEventGenerator for Vec<T> {
    fn send(&self) -> String {
        self.iter()
            .fold(String::new(), |acc, event| acc + &event.send())
    }
}
