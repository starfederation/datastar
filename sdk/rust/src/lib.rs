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
    pub use crate::axum::ReadSignals;
    // #[cfg(feature = "rocket")]
    // pub use crate::rocket::ReadSignals;
    pub use crate::{
        consts::FragmentMergeMode, execute_script::ExecuteScript, merge_fragments::MergeFragments,
        merge_signals::MergeSignals, remove_fragments::RemoveFragments,
        remove_signals::RemoveSignals,
    };
}

use {core::time::Duration, std::fmt::Display};

#[derive(Debug)]
/// TODO
pub struct DatastarEvent {
    /// TODO
    pub event: consts::EventType,
    /// TODO
    pub id: Option<String>,
    /// TODO
    pub retry: Duration,
    /// TODO
    pub data: Vec<String>,
}

impl Display for DatastarEvent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "event: {}", self.event.as_str())?;

        if let Some(id) = &self.id {
            writeln!(f, "id: {}", id)?;
        }

        if self.retry.as_millis() != crate::consts::DEFAULT_SSE_RETRY_DURATION as u128 {
            writeln!(f, "retry: {}", self.retry.as_millis())?;
        }

        for line in &self.data {
            writeln!(f, "data: {}", line)?;
        }

        Ok(())
    }
}
