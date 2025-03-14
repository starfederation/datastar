//! [`RemoveSignals`] sends signals to the browser to be removed from the signals.

use {
    crate::{consts, DatastarEvent},
    core::time::Duration,
};

/// [`RemoveSignals`] sends signals to the browser to be removed from the signals.
///
/// See the [Datastar documentation](https://data-star.dev/reference/sse_events#datastar-remove-signals) for more information.
///
/// # Examples
///
/// ```
/// use datastar::prelude::{Sse, RemoveSignals};
/// use async_stream::stream;
///
/// Sse(stream! {
///     yield RemoveSignals::new(["foo.bar", "1234", "abc"]).into();
/// });
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RemoveSignals {
    /// `id` can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    pub id: Option<String>,
    /// `retry` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
    pub retry: Duration,
    /// `paths` is a list of strings that represent the signal paths to be removed from the signals.
    /// The paths ***must*** be valid . delimited paths to signals within the signals.
    /// The Datastar client side will use these paths to remove the data from the signals.
    pub paths: Vec<String>,
}

impl RemoveSignals {
    /// Creates a new [`RemoveSignals`] event with the given paths.
    pub fn new(paths: impl IntoIterator<Item = impl Into<String>>) -> Self {
        Self {
            id: None,
            retry: Duration::from_millis(consts::DEFAULT_SSE_RETRY_DURATION),
            paths: paths.into_iter().map(Into::into).collect(),
        }
    }

    /// Sets the `id` of the [`RemoveSignals`] event.
    pub fn id(mut self, id: impl Into<String>) -> Self {
        self.id = Some(id.into());
        self
    }

    /// Sets the `retry` of the [`RemoveSignals`] event.
    pub fn retry(mut self, retry: Duration) -> Self {
        self.retry = retry;
        self
    }
}

impl From<RemoveSignals> for DatastarEvent {
    fn from(val: RemoveSignals) -> Self {
        let mut data: Vec<String> = Vec::new();

        for line in &val.paths {
            data.push(format!("{} {}", consts::PATHS_DATALINE_LITERAL, line));
        }

        Self {
            event: consts::EventType::RemoveSignals,
            id: val.id,
            retry: val.retry,
            data,
        }
    }
}
