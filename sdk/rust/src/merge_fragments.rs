//! [`MergeFragments`] event used to merge fragments of HTML into the DOM using the `datastar-merge-fragments` event.

use {
    crate::{MergeMode, ServerSentEventGenerator},
    core::time::Duration,
};

/// [`MergeFragments`] is an event used to merge fragments of HTML into the DOM
/// using the `datastar-merge-fragments` event.
///
/// See the [Datastar documentation](https://data-star.dev/reference/plugins_backend#datastar-sse-events) for more information.
///
/// # Examples
///
/// ```
/// use {
///     core::time::Duration,
///     datastar::prelude::{ServerSentEventGenerator, MergeFragments, MergeMode}
/// };
///
/// let merge_fragments: String = MergeFragments::new("<h1>Hello, world!</h1>")
///     .selector("body")
///     .merge_mode(MergeMode::Append)
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
    /// This can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    pub id: Option<String>,
    /// This is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
    pub retry_duration: Duration,
    /// The HTML fragments to merge into the DOM.
    pub fragments: String,
    /// Selects the target element of the merge process using a CSS selector.
    ///
    /// # Note
    /// You should almost never need this attribute.
    /// It is only for special cases.
    /// The default is to use idiomorph to merge the fragment by using the top level id into the DOM.
    /// Unless you are adding to a list, this is almost always the right answer!
    pub selector: Option<String>,
    /// Determines how the fragments are merged into the DOM.
    ///
    /// # Note
    /// You should almost never need this attribute.
    /// It is only for special cases.
    /// The default is to use idiomorph to merge the fragment by using the top level id into the DOM.
    /// Unless you are adding to a list, this is almost always the right answer!
    pub merge_mode: MergeMode,
    /// The duration to wait before merging the fragments.
    pub settle_duration: Duration,
    /// Whether to use view transitions when merging into the DOM.
    pub use_view_transition: bool,
}

impl MergeFragments {
    /// Creates a new [`MergeFragments`] event with the given fragments.
    pub fn new(fragments: impl Into<String>) -> Self {
        Self {
            id: Default::default(),
            retry_duration: Duration::from_millis(1000),
            fragments: fragments.into(),
            selector: Default::default(),
            merge_mode: Default::default(),
            settle_duration: Duration::from_millis(300),
            use_view_transition: false,
        }
    }

    /// Adds an id to the [`ExecuteScript`] event.
    pub fn id(mut self, id: impl Into<String>) -> Self {
        self.id = Some(id.into());
        self
    }

    /// Adds a retry duration to the [`ExecuteScript`] event.
    pub fn retry_duration(mut self, retry_duration: Duration) -> Self {
        self.retry_duration = retry_duration;
        self
    }

    /// Sets the css selector of the [`MergeFragments`] event.
    /// See [`selector`](MergeFragments::selector).
    pub fn selector(mut self, selector: impl Into<String>) -> Self {
        self.selector = Some(selector.into());
        self
    }

    /// Sets the merge mode of the [`MergeFragments`] event.
    /// See [`MergeMode`](MergeFragments::merge_mode).
    pub fn merge_mode(mut self, merge_mode: MergeMode) -> Self {
        self.merge_mode = merge_mode;
        self
    }

    /// Sets the settle duration of the [`MergeFragments`] event.
    /// See [`settle_duration`](MergeFragments::settle_duration).
    pub fn settle_duration(mut self, settle_duration: Duration) -> Self {
        self.settle_duration = settle_duration;
        self
    }

    /// Sets whether to use view transitions of the [`MergeFragments`] event.
    /// See [`use_view_transition`](MergeFragments::use_view_transition).
    pub fn use_view_transition(mut self, use_view_transition: bool) -> Self {
        self.use_view_transition = use_view_transition;
        self
    }
}

impl ServerSentEventGenerator for MergeFragments {
    fn send(&self) -> String {
        let mut result = String::new();

        result.push_str("event: datastar-merge-fragments\n");

        if let Some(id) = &self.id {
            result.push_str("id: ");
            result.push_str(id);
            result.push_str("\n");
        }

        if self.retry_duration.as_millis() != 1000 {
            result.push_str("retryDuration: ");
            result.push_str(&self.retry_duration.as_millis().to_string());
            result.push_str("\n");
        }

        if let Some(selector) = &self.selector {
            result.push_str("data: selector ");
            result.push_str(selector);
            result.push_str("\n");
        }

        if self.merge_mode != MergeMode::Morph {
            result.push_str("data: mergeMode ");
            result.push_str(self.merge_mode.as_str());
            result.push_str("\n");
        }

        if self.settle_duration.as_millis() != 300 {
            result.push_str("data: settleDuration ");
            result.push_str(&self.settle_duration.as_millis().to_string());
            result.push_str("\n");
        }

        if self.use_view_transition {
            result.push_str("data: useViewTransition ");
            result.push_str("true\n");
        }

        for line in self.fragments.lines() {
            result.push_str("data: fragments ");
            result.push_str(line);
            result.push_str("\n");
        }

        result.push_str("\n");

        result
    }
}
