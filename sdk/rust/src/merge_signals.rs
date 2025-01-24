//! [`MergeSignals`] event used to update signals with new values.

use {crate::ServerSentEventGenerator, core::time::Duration};

/// [`MergeSignals`] is an event used to update signals with new values
/// using the `datastar-merge-signals` event.
///
/// See the [Datastar documentation](https://data-star.dev/reference/plugins_backend#datastar-sse-events) for more information.
///
/// # Examples
///
/// ```
/// use datastar::prelude::{ServerSentEventGenerator, MergeSignals};
///
/// let merge_signals: String = MergeSignals::new("{foo: 1234}")
///     .only_if_missing(true)
///     .send();
///
/// let expected: &str = "event: datastar-merge-signals
/// data: onlyIfMissing true
/// data: signals {foo: 1234}
///
/// ";
///
/// assert_eq!(merge_signals, expected);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MergeSignals {
    /// This can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    pub id: Option<String>,
    /// This is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
    pub retry_duration: Duration,
    /// The signals to merge into the DOM.
    pub signals: String,
    /// Determines whether to update the signals with new values only if the key does not exist.
    pub only_if_missing: bool,
}

impl MergeSignals {
    /// Creates a new [`MergeSignals`] event with the given signals.
    pub fn new(signals: impl Into<String>) -> Self {
        Self {
            id: Default::default(),
            retry_duration: Duration::from_millis(1000),
            signals: signals.into(),
            only_if_missing: false,
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

    /// Determines whether to update the signals with new values only if the key does not exist.
    pub fn only_if_missing(mut self, only_if_missing: bool) -> Self {
        self.only_if_missing = only_if_missing;
        self
    }
}

impl ServerSentEventGenerator for MergeSignals {
    fn send(&self) -> String {
        let mut result = String::new();

        result.push_str("event: datastar-merge-signals\n");

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

        if self.only_if_missing {
            result.push_str("data: onlyIfMissing true\n");
        }

        result.push_str("data: signals ");
        result.push_str(&self.signals);
        result.push_str("\n\n");

        result
    }
}
