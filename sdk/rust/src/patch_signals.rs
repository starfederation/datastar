//! [`PatchSignals`] patches signals into the signal store.

use {
    crate::{DatastarEvent, consts},
    core::time::Duration,
};

/// [`PatchSignals`] patches signals into the signal store.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatchSignals {
    /// `id` can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see <https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id>
    pub id: Option<String>,
    /// `retry` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see <https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry>
    pub retry: Duration,
    /// `signals` is a JavaScript object or JSON string that will be sent to the browser to update signals in the signals.
    /// The data ***must*** evaluate to a valid JavaScript. It will be converted to signals by the Datastar client side.
    pub signals: String,
    /// Whether to patch the signal only if it does not already exist.
    /// If not provided, the Datastar client side will default to false, which will cause the data to be patched into the signals.
    pub only_if_missing: bool,
}

impl PatchSignals {
    /// Creates a new [`PatchSignals`] event with the given signals.
    pub fn new(signals: impl Into<String>) -> Self {
        Self {
            id: None,
            retry: Duration::from_millis(consts::DEFAULT_SSE_RETRY_DURATION),
            signals: signals.into(),
            only_if_missing: consts::DEFAULT_PATCH_SIGNALS_ONLY_IF_MISSING,
        }
    }

    /// Sets the `id` of the [`PatchSignals`] event.
    pub fn id(mut self, id: impl Into<String>) -> Self {
        self.id = Some(id.into());
        self
    }

    /// Sets the `retry` of the [`PatchSignals`] event.
    pub fn retry(mut self, retry: Duration) -> Self {
        self.retry = retry;
        self
    }

    /// Sets the `only_if_missing` of the [`PatchSignals`] event.
    pub fn only_if_missing(mut self, only_if_missing: bool) -> Self {
        self.only_if_missing = only_if_missing;
        self
    }

    /// Converts this [`PatchSignals`] into a [`DatastarEvent`].
    #[inline]
    pub fn into_datastar_event(mut self) -> DatastarEvent {
        let id = self.id.take();
        self.convert_to_datastar_event_inner(id)
    }

    /// Copy this [`PatchSignals`] as a [`DatastarEvent`].
    #[inline]
    pub fn as_datastar_event(&self) -> DatastarEvent {
        self.convert_to_datastar_event_inner(self.id.clone())
    }

    fn convert_to_datastar_event_inner(&self, id: Option<String>) -> DatastarEvent {
        let mut data: Vec<String> = Vec::new();

        if self.only_if_missing != consts::DEFAULT_PATCH_SIGNALS_ONLY_IF_MISSING {
            data.push(format!(
                "{} {}",
                consts::ONLY_IF_MISSING_DATALINE_LITERAL,
                self.only_if_missing
            ));
        }

        for line in self.signals.lines() {
            data.push(format!("{} {line}", consts::SIGNALS_DATALINE_LITERAL));
        }

        DatastarEvent {
            event: consts::EventType::PatchSignals,
            id,
            retry: self.retry,
            data,
        }
    }
}

impl From<&PatchSignals> for DatastarEvent {
    #[inline]
    fn from(val: &PatchSignals) -> Self {
        val.as_datastar_event()
    }
}

impl From<PatchSignals> for DatastarEvent {
    #[inline]
    fn from(val: PatchSignals) -> Self {
        val.into_datastar_event()
    }
}
