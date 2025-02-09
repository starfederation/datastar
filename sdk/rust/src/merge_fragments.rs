//! [`MergeFragments`] merges one or more fragments into the DOM.
//! By default, Datastar merges fragments using Idiomorph, which matches top level elements based on their ID.

use {
    crate::{
        consts::{self, FragmentMergeMode},
        DatastarEvent,
    },
    core::time::Duration,
};

/// [`MergeFragments`] merges one or more fragments into the DOM. By default,
/// Datastar merges fragments using Idiomorph, which matches top level elements based on their ID.
///
/// See the [Datastar documentation](https://data-star.dev/reference/sse_events#datastar-merge-fragments) for more information.
///
/// # Examples
///
/// ```
/// use datastar::prelude::{Sse, MergeFragments, FragmentMergeMode};
/// use async_stream::stream;
/// use core::time::Duration;
///
/// Sse(stream! {
///     yield MergeFragments::new("<h1>Hello, world!</h1>")
///         .selector("body")
///         .merge_mode(FragmentMergeMode::Append)
///         .settle_duration(Duration::from_millis(1000))
///         .use_view_transition(true)
///         .into();
/// });
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MergeFragments {
    /// `id` is can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    pub id: Option<String>,
    /// `retry` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
    pub retry: Duration,
    /// The HTML fragments to merge into the DOM.
    pub fragments: String,
    /// The CSS selector to use to insert the fragments.
    /// If not provided, Datastar will default to using the id attribute of the fragment.
    pub selector: Option<String>,
    /// The mode to use when merging the fragment into the DOM.
    /// If not provided the Datastar client side will default to [`FragmentMergeMode::Morph`].
    pub merge_mode: FragmentMergeMode,
    /// The amount of time that a fragment should take before removing any CSS related to settling.
    /// `settle_duration` is used to allow for animations in the browser via the Datastar client.
    pub settle_duration: Duration,
    /// Whether to use view transitions, if not provided the Datastar client side will default to `false`.
    pub use_view_transition: bool,
}

impl MergeFragments {
    /// Creates a new [`MergeFragments`] event with the given fragments.
    pub fn new(fragments: impl Into<String>) -> Self {
        Self {
            id: None,
            retry: Duration::from_millis(consts::DEFAULT_SSE_RETRY_DURATION),
            fragments: fragments.into(),
            selector: None,
            merge_mode: FragmentMergeMode::default(),
            settle_duration: Duration::from_millis(consts::DEFAULT_FRAGMENTS_SETTLE_DURATION),
            use_view_transition: consts::DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS,
        }
    }

    /// Sets the `id` of the [`MergeFragments`] event.
    pub fn id(mut self, id: impl Into<String>) -> Self {
        self.id = Some(id.into());
        self
    }

    /// Sets the `retry` of the [`MergeFragments`] event.
    pub fn retry(mut self, retry: Duration) -> Self {
        self.retry = retry;
        self
    }

    /// Sets the `selector` of the [`MergeFragments`] event.
    pub fn selector(mut self, selector: impl Into<String>) -> Self {
        self.selector = Some(selector.into());
        self
    }

    /// Sets the `merge_mode` of the [`MergeFragments`] event.
    pub fn merge_mode(mut self, merge_mode: FragmentMergeMode) -> Self {
        self.merge_mode = merge_mode;
        self
    }

    /// Sets the `settle_duration` of the [`MergeFragments`] event.
    pub fn settle_duration(mut self, settle_duration: Duration) -> Self {
        self.settle_duration = settle_duration;
        self
    }

    /// Sets the `use_view_transition` of the [`MergeFragments`] event.
    pub fn use_view_transition(mut self, use_view_transition: bool) -> Self {
        self.use_view_transition = use_view_transition;
        self
    }
}

impl From<MergeFragments> for DatastarEvent {
    fn from(val: MergeFragments) -> Self {
        let mut data: Vec<String> = Vec::new();

        if let Some(selector) = &val.selector {
            data.push(format!(
                "{} {}",
                consts::SELECTOR_DATALINE_LITERAL,
                selector
            ));
        }

        if val.merge_mode != FragmentMergeMode::default() {
            data.push(format!(
                "{} {}",
                consts::MERGE_MODE_DATALINE_LITERAL,
                val.merge_mode.as_str()
            ));
        }

        if val.settle_duration.as_millis() != consts::DEFAULT_FRAGMENTS_SETTLE_DURATION as u128 {
            data.push(format!(
                "{} {}",
                consts::SETTLE_DURATION_DATALINE_LITERAL,
                val.settle_duration.as_millis()
            ));
        }

        if val.use_view_transition != consts::DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS {
            data.push(format!(
                "{} {}",
                consts::USE_VIEW_TRANSITION_DATALINE_LITERAL,
                val.use_view_transition
            ));
        }

        for line in val.fragments.lines() {
            data.push(format!("{} {}", consts::FRAGMENTS_DATALINE_LITERAL, line));
        }

        DatastarEvent {
            event: consts::EventType::MergeFragments,
            id: val.id.clone(),
            retry: val.retry,
            data,
        }
    }
}
