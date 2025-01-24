//! Datastar event implementations for Server-Sent ServerSentEventGenerators (SSE)
//!
//! This crate provides strongly-typed event implementations for [Datastar](https://data-star.dev).

#![forbid(missing_docs)]
#![forbid(missing_debug_implementations)]

#[cfg(feature = "axum")]
pub mod axum;
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
        execute_script::ExecuteScript, merge_fragments::MergeFragments,
        merge_signals::MergeSignals, remove_fragments::RemoveFragments,
        remove_signals::RemoveSignals, MergeMode, ServerSentEventGenerator,
    };
}

use typle::{typle, typle_fold};

/// [`MergeMode`] specifies the merge strategy that Datastar will use.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MergeMode {
    /// Merges the fragment using [Idiomorph](https://github.com/bigskysoftware/idiomorph). This is the default merge strategy.
    #[default]
    Morph,
    /// Replaces the target’s innerHTML with the fragment.
    Inner,
    /// Replaces the target’s outerHTML with the fragment.
    Outer,
    /// Prepends the fragment to the target’s children.
    Prepend,
    /// Appends the fragment to the target’s children.
    Append,
    /// Inserts the fragment before the target as a sibling.
    Before,
    /// Inserts the fragment after the target as a sibling.
    After,
    /// Merges attributes from the fragment into the target – useful for updating a signals.
    UpsertAttributes,
}

impl MergeMode {
    /// Returns the merge mode as a string.
    pub const fn as_str(&self) -> &str {
        match self {
            MergeMode::Morph => "morph",
            MergeMode::Inner => "inner",
            MergeMode::Outer => "outer",
            MergeMode::Prepend => "prepend",
            MergeMode::Append => "append",
            MergeMode::Before => "before",
            MergeMode::After => "after",
            MergeMode::UpsertAttributes => "upsertAttributes",
        }
    }
}

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
