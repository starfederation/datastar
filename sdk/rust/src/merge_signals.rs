//! [`MergeSignals`] sends one or more signals to the browser to be merged into the signals.

use {
    crate::{consts, ServerSentEventGenerator},
    core::time::Duration,
};

/// [`MergeSignals`] sends one or more signals to the browser to be merged into the signals.
///
/// See the [Datastar documentation](https://data-star.dev/reference/sse_events#datastar-merge-signals) for more information.
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
    /// `id` can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    pub id: Option<String>,
    /// `retry_duration` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// Defaults to `1000ms`.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
    pub retry_duration: Duration,
    /// `signals` is a JavaScript object or JSON string that will be sent to the browser to update signals in the signals.
    /// The data ***must*** evaluate to a valid JavaScript. It will be converted to signals by the Datastar client side.
    pub signals: String,
    /// Whether to merge the signal only if it does not already exist.
    /// If not provided, the Datastar client side will default to false, which will cause the data to be merged into the signals.
    pub only_if_missing: bool,
}

impl MergeSignals {
    /// Creates a new [`MergeSignals`] event with the given signals.
    pub fn new(signals: impl Into<String>) -> Self {
        Self {
            id: Default::default(),
            retry_duration: Duration::from_millis(consts::DEFAULT_SSE_RETRY_DURATION),
            signals: signals.into(),
            only_if_missing: consts::DEFAULT_MERGE_SIGNALS_ONLY_IF_MISSING,
        }
    }

    /// Sets the `id` of the [`MergeSignals`] event.
    pub fn id(mut self, id: impl Into<String>) -> Self {
        self.id = Some(id.into());
        self
    }

    /// Sets the `retry_duration` of the [`MergeSignals`] event.
    pub fn retry_duration(mut self, retry_duration: Duration) -> Self {
        self.retry_duration = retry_duration;
        self
    }

    /// Sets the `only_if_missing` of the [`MergeSignals`] event.
    pub fn only_if_missing(mut self, only_if_missing: bool) -> Self {
        self.only_if_missing = only_if_missing;
        self
    }
}

impl ServerSentEventGenerator for MergeSignals {
    fn send(&self) -> String {
        let mut result = String::new();

        result.push_str("event: ");
        result.push_str(consts::EventType::MergeSignals.as_str());
        result.push_str("\n");

        if let Some(id) = &self.id {
            result.push_str("id: ");
            result.push_str(id);
            result.push_str("\n");
        }

        result.push_str("retry: ");
        result.push_str(&self.retry_duration.as_millis().to_string());
        result.push_str("\n");

        if self.only_if_missing {
            result.push_str("data: ");
            result.push_str(consts::ONLY_IF_MISSING_DATALINE_LITERAL);
            result.push_str(" true\n");
        }

        result.push_str("data: ");
        result.push_str(consts::SIGNALS_DATALINE_LITERAL);
        result.push_str(" ");
        result.push_str(&self.signals);
        result.push_str("\n\n\n");

        result
    }
}
