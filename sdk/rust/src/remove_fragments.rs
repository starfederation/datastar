//! [`RemoveFragments`] event used to remove HTML fragments that match the provided selector from the DOM.

use {crate::ServerSentEventGenerator, std::time::Duration};

/// [`RemoveFragments`] is an event used to remove HTML fragments that match the provided selector from the DOM.
///
/// See the [Datastar documentation](https://data-star.dev/reference/plugins_backend#datastar-sse-events) for more information.
///
/// # Examples
///
/// ```
/// use datastar::prelude::{ServerSentEventGenerator, RemoveFragments};
///
/// let remove_fragments: String = RemoveFragments::new("#foo").send();
///
/// let expected: &str = "event: datastar-remove-fragments
/// data: selector #foo
///
/// ";
///
/// assert_eq!(remove_fragments, expected);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RemoveFragments {
    /// This can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    pub id: Option<String>,
    /// This is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
    pub retry_duration: Duration,
    /// Selects the fragments to remove from the DOM using a CSS selector.
    pub selector: String,
}

impl RemoveFragments {
    /// Creates a new [`RemoveFragments`] event with the given selector.
    pub fn new(selector: impl Into<String>) -> Self {
        Self {
            id: Default::default(),
            retry_duration: Duration::from_millis(1000),
            selector: selector.into(),
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
}

impl ServerSentEventGenerator for RemoveFragments {
    fn send(&self) -> String {
        let mut result = String::new();

        result.push_str("event: datastar-remove-fragments\n");

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

        result.push_str("data: selector ");
        result.push_str(&self.selector);
        result.push_str("\n\n");

        result
    }
}
