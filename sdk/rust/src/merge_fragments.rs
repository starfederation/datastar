//! [`MergeFragments`] merges one or more fragments into the DOM.
//! By default, Datastar merges fragments using Idiomorph, which matches top level elements based on their ID.

use {
    crate::{
        consts::{self},
        prelude::{FragmentMergeMode, ServerSentEventGenerator},
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
/// use {
///     core::time::Duration,
///     datastar::prelude::{ServerSentEventGenerator, MergeFragments, FragmentMergeMode}
/// };
///
/// let merge_fragments: String = MergeFragments::new("<h1>Hello, world!</h1>")
///     .selector("body")
///     .merge_mode(FragmentMergeMode::Append)
///     .settle_duration(Duration::from_millis(1000))
///     .use_view_transition(true)
///     .send();
///
/// let expected: &str = "event: datastar-merge-fragments
/// data: selector body
/// data: mergeMode append
/// data: settleDuration 1000
/// data: useViewTransition true
/// data: fragments <h1>Hello, world!</h1>
///
/// ";
///
/// assert_eq!(merge_fragments, expected);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MergeFragments {
    /// `id` is can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    pub id: Option<String>,
    /// `retry_duration` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// Defaults to `1000ms`.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
    pub retry_duration: Duration,
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
    /// Defaults to `300ms`.
    pub settle_duration: Duration,
    /// Whether to use view transitions, if not provided the Datastar client side will default to `false`.
    pub use_view_transition: bool,
}

impl MergeFragments {
    /// Creates a new [`MergeFragments`] event with the given fragments.
    pub fn new(fragments: impl Into<String>) -> Self {
        Self {
            id: Default::default(),
            retry_duration: Duration::from_millis(consts::DEFAULT_SSE_RETRY_DURATION),
            fragments: fragments.into(),
            selector: Default::default(),
            merge_mode: Default::default(),
            settle_duration: Duration::from_millis(consts::DEFAULT_FRAGMENTS_SETTLE_DURATION),
            use_view_transition: consts::DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS,
        }
    }

    /// Sets the `id` of the [`MergeFragments`] event.
    pub fn id(mut self, id: impl Into<String>) -> Self {
        self.id = Some(id.into());
        self
    }

    /// Sets the `retry_duration` of the [`MergeFragments`] event.
    pub fn retry_duration(mut self, retry_duration: Duration) -> Self {
        self.retry_duration = retry_duration;
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

impl ServerSentEventGenerator for MergeFragments {
    fn send(&self) -> String {
        let mut result = String::new();

        result.push_str("event: ");
        result.push_str(consts::EventType::MergeFragments.as_str());
        result.push_str("\n");

        if let Some(id) = &self.id {
            result.push_str("id: ");
            result.push_str(id);
            result.push_str("\n");
        }

        result.push_str("retry: ");
        result.push_str(&self.retry_duration.as_millis().to_string());
        result.push_str("\n");

        if let Some(selector) = &self.selector {
            result.push_str("data: ");
            result.push_str(consts::SELECTOR_DATALINE_LITERAL);
            result.push_str(" ");
            result.push_str(selector);
            result.push_str("\n");
        }

        if self.merge_mode != FragmentMergeMode::Morph {
            result.push_str("data: ");
            result.push_str(consts::MERGE_MODE_DATALINE_LITERAL);
            result.push_str(" ");
            result.push_str(self.merge_mode.as_str());
            result.push_str("\n");
        }

        if self.settle_duration.as_millis() != 300 {
            result.push_str("data: ");
            result.push_str(consts::SETTLE_DURATION_DATALINE_LITERAL);
            result.push_str(" ");
            result.push_str(&self.settle_duration.as_millis().to_string());
            result.push_str("\n");
        }

        if self.use_view_transition {
            result.push_str("data: ");
            result.push_str(consts::USE_VIEW_TRANSITION_DATALINE_LITERAL);
            result.push_str(" true\n");
        }

        for line in self.fragments.lines() {
            result.push_str("data: ");
            result.push_str(consts::FRAGMENTS_DATALINE_LITERAL);
            result.push_str(" ");
            result.push_str(line);
            result.push_str("\n");
        }

        result.push_str("\n");

        result
    }
}
