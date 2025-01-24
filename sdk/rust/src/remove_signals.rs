//! [`RemoveSignals`] sends signals to the browser to be removed from the signals.

use {
    crate::{consts, ServerSentEventGenerator},
    core::time::Duration,
};

/// [`RemoveSignals`] sends signals to the browser to be removed from the signals.
///
/// See the [Datastar documentation](https://data-star.dev/reference/sse_events#datastar-remove-signals) for more information.
///
/// # Examples
///
/// ```
/// use datastar::prelude::{ServerSentEventGenerator, RemoveSignals};
///
/// let remove_signals: String = RemoveSignals::new(["foo.bar", "1234", "abc"]).send();
///
/// let expected: &str = r#"event: datastar-remove-signals
/// data: paths foo.bar
/// data: paths 1234
/// data: paths abc
///
/// "#;
///
/// assert_eq!(remove_signals, expected);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RemoveSignals {
    /// `id` can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    pub id: Option<String>,
    /// `retry_duration` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// Defaults to `1000ms`.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
    pub retry_duration: Duration,
    /// `paths` is a list of strings that represent the signal paths to be removed from the signals.
    /// The paths ***must*** be valid . delimited paths to signals within the signals.
    /// The Datastar client side will use these paths to remove the data from the signals.
    pub paths: Vec<String>,
}

impl RemoveSignals {
    /// Creates a new [`RemoveSignals`] event with the given paths.
    pub fn new(paths: impl IntoIterator<Item = impl Into<String>>) -> Self {
        Self {
            id: Default::default(),
            retry_duration: Duration::from_millis(consts::DEFAULT_SSE_RETRY_DURATION),
            paths: paths.into_iter().map(Into::into).collect(),
        }
    }

    /// Sets the `id` of the [`RemoveSignals`] event.
    pub fn id(mut self, id: impl Into<String>) -> Self {
        self.id = Some(id.into());
        self
    }

    /// Sets the `retry_duration` of the [`RemoveSignals`] event.
    pub fn retry_duration(mut self, retry_duration: Duration) -> Self {
        self.retry_duration = retry_duration;
        self
    }
}

impl ServerSentEventGenerator for RemoveSignals {
    fn send(&self) -> String {
        let mut result = String::new();

        result.push_str("event: ");
        result.push_str(consts::EventType::RemoveSignals.as_str());
        result.push_str("\n");

        if let Some(id) = &self.id {
            result.push_str("id: ");
            result.push_str(id.as_ref());
            result.push_str("\n");
        }

        if self.retry_duration.as_millis() != consts::DEFAULT_SSE_RETRY_DURATION as u128 {
            result.push_str("retryDuration: ");
            result.push_str(&self.retry_duration.as_millis().to_string());
            result.push_str("\n");
        }

        for line in &self.paths {
            result.push_str("data: ");
            result.push_str(consts::PATHS_DATALINE_LITERAL);
            result.push_str(" ");
            result.push_str(line);
            result.push_str("\n");
        }

        result.push_str("\n");

        result
    }
}
