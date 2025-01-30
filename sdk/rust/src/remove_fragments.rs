//! [`RemoveFragments`] sends a selector to the browser to remove HTML fragments from the DOM.

use {
    crate::{consts, DatastarEvent},
    core::time::Duration,
};

/// [`RemoveFragments`] sends a selector to the browser to remove HTML fragments from the DOM.
///
/// See the [Datastar documentation](https://data-star.dev/reference/sse_events#datastar-remove-fragments) for more information.
///
/// # Examples
///
/// ```
/// use {
///     core::time::Duration,
///     datastar::prelude::{ServerSentEventGenerator, RemoveFragments}
/// };
///
/// let remove_fragments: String = RemoveFragments::new("#foo")
///     .settle_duration(Duration::from_millis(1000))
///     .use_view_transition(true)
///     .send();
///
/// let expected: &str = "event: datastar-remove-fragments
/// retry: 1000
/// data: settleDuration 1000
/// data: useViewTransition true
/// data: selector #foo
///
///
/// ";
///
/// assert_eq!(remove_fragments, expected);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RemoveFragments {
    /// `id` can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    pub id: Option<String>,
    /// `retry` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// Defaults to `1000ms`.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
    pub retry: Duration,
    /// `selector` is a CSS selector that represents the fragments to be removed from the DOM.
    /// The selector must be a valid CSS selector.
    /// The Datastar client side will use this selector to remove the fragment from the DOM.
    pub selector: String,
    /// The amount of time that a fragment should take before removing any CSS related to settling.
    /// `settle_duration` is used to allow for animations in the browser via the Datastar client.
    /// Defaults to `300ms`.
    pub settle_duration: Duration,
    /// Whether to use view transitions, if not provided the Datastar client side will default to `false`.
    pub use_view_transition: bool,
}

impl RemoveFragments {
    /// Creates a new [`RemoveFragments`] event with the given selector.
    pub fn new(selector: impl Into<String>) -> Self {
        Self {
            id: Default::default(),
            retry: Duration::from_millis(consts::DEFAULT_SSE_RETRY_DURATION),
            selector: selector.into(),
            settle_duration: Duration::from_millis(consts::DEFAULT_FRAGMENTS_SETTLE_DURATION),
            use_view_transition: consts::DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS,
        }
    }

    /// Sets the `id` of the [`RemoveFragments`] event.
    pub fn id(mut self, id: impl Into<String>) -> Self {
        self.id = Some(id.into());
        self
    }

    /// Sets the `retry` of the [`RemoveFragments`] event.
    pub fn retry(mut self, retry: Duration) -> Self {
        self.retry = retry;
        self
    }

    /// Sets the `settle_duration` of the [`RemoveFragments`] event.
    pub fn settle_duration(mut self, settle_duration: Duration) -> Self {
        self.settle_duration = settle_duration;
        self
    }

    /// Sets the `use_view_transition` of the [`RemoveFragments`] event.
    pub fn use_view_transition(mut self, use_view_transition: bool) -> Self {
        self.use_view_transition = use_view_transition;
        self
    }
}

impl Into<DatastarEvent> for RemoveFragments {
    fn into(self) -> DatastarEvent {
        let mut data: Vec<String> = Vec::new();

        if self.settle_duration.as_millis() != consts::DEFAULT_FRAGMENTS_SETTLE_DURATION as u128 {
            data.push(format!(
                "{} {}",
                consts::SETTLE_DURATION_DATALINE_LITERAL,
                self.settle_duration.as_millis()
            ));
        }

        if self.use_view_transition != consts::DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS {
            data.push(format!(
                "{} {}",
                consts::USE_VIEW_TRANSITION_DATALINE_LITERAL,
                self.use_view_transition
            ));
        }

        data.push(format!(
            "{} {}",
            consts::SELECTOR_DATALINE_LITERAL,
            self.selector
        ));

        DatastarEvent {
            event: consts::EventType::RemoveFragments,
            id: self.id,
            retry: self.retry,
            data,
        }
    }
}
