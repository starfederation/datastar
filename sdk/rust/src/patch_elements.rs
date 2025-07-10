//! [`PatchElements`] patches HTML elements into the DOM.

use {
    crate::{
        DatastarEvent,
        consts::{self, ElementPatchMode},
    },
    core::time::Duration,
};

/// [`PatchElements`] patches HTML elements into the DOM.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatchElements {
    /// `id` is can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see <https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id>
    pub id: Option<String>,
    /// `retry` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see <https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry>
    pub retry: Duration,
    /// The HTML elements to patched into the DOM.
    ///
    /// In case of [`ElementPatchMode::Remove`] this attribute will be `None`.
    pub elements: Option<String>,
    /// The CSS selector to use to patch the elements.
    /// If not provided, Datastar will default to using the id attribute of the elements.
    pub selector: Option<String>,
    /// The mode to use when patching the element into the DOM.
    /// If not provided the Datastar client side will default to [`ElementPatchMode::Outer`].
    pub mode: ElementPatchMode,
    /// Whether to use view transitions, if not provided the Datastar client side will default to `false`.
    pub use_view_transition: bool,
}

impl PatchElements {
    /// Creates a new [`PatchElements`] event with the given elements.
    pub fn new(elements: impl Into<String>) -> Self {
        Self {
            id: None,
            retry: Duration::from_millis(consts::DEFAULT_SSE_RETRY_DURATION),
            elements: Some(elements.into()),
            selector: None,
            mode: ElementPatchMode::default(),
            use_view_transition: consts::DEFAULT_ELEMENTS_USE_VIEW_TRANSITIONS,
        }
    }

    /// Creates a new [`PatchElements`] to delete the elements for the given selector.
    pub fn new_remove(selector: impl Into<String>) -> Self {
        Self {
            id: None,
            retry: Duration::from_millis(consts::DEFAULT_SSE_RETRY_DURATION),
            elements: None,
            selector: Some(selector.into()),
            mode: ElementPatchMode::Remove,
            use_view_transition: consts::DEFAULT_ELEMENTS_USE_VIEW_TRANSITIONS,
        }
    }

    /// Sets the `id` of the [`PatchElements`] event.
    pub fn id(mut self, id: impl Into<String>) -> Self {
        self.id = Some(id.into());
        self
    }

    /// Sets the `retry` of the [`PatchElements`] event.
    pub fn retry(mut self, retry: Duration) -> Self {
        self.retry = retry;
        self
    }

    /// Sets the `selector` of the [`PatchElements`] event.
    pub fn selector(mut self, selector: impl Into<String>) -> Self {
        self.selector = Some(selector.into());
        self
    }

    /// Sets the `mode` of the [`PatchElements`] event.
    pub fn mode(mut self, mode: ElementPatchMode) -> Self {
        self.mode = mode;
        self
    }

    /// Sets the `use_view_transition` of the [`PatchElements`] event.
    pub fn use_view_transition(mut self, use_view_transition: bool) -> Self {
        self.use_view_transition = use_view_transition;
        self
    }

    /// Converts this [`PatchElements`] into a [`DatastarEvent`].
    #[inline]
    pub fn into_datastar_event(mut self) -> DatastarEvent {
        let id = self.id.take();
        self.convert_to_datastar_event_inner(id)
    }

    /// Copy this [`PatchElements`] as a [`DatastarEvent`].
    #[inline]
    pub fn as_datastar_event(&self) -> DatastarEvent {
        self.convert_to_datastar_event_inner(self.id.clone())
    }

    fn convert_to_datastar_event_inner(&self, id: Option<String>) -> DatastarEvent {
        let mut data: Vec<String> = Vec::new();

        if let Some(selector) = &self.selector {
            data.push(format!(
                "{} {}",
                consts::SELECTOR_DATALINE_LITERAL,
                selector
            ));
        }

        if self.mode != ElementPatchMode::default() {
            data.push(format!(
                "{} {}",
                consts::MODE_DATALINE_LITERAL,
                self.mode.as_str()
            ));
        }

        if self.use_view_transition != consts::DEFAULT_ELEMENTS_USE_VIEW_TRANSITIONS {
            data.push(format!(
                "{} {}",
                consts::USE_VIEW_TRANSITION_DATALINE_LITERAL,
                self.use_view_transition
            ));
        }

        if let Some(ref elements) = self.elements {
            for line in elements.lines() {
                data.push(format!("{} {}", consts::ELEMENTS_DATALINE_LITERAL, line));
            }
        }

        DatastarEvent {
            event: consts::EventType::PatchElements,
            id,
            retry: self.retry,
            data,
        }
    }
}

impl From<&PatchElements> for DatastarEvent {
    #[inline]
    fn from(val: &PatchElements) -> Self {
        val.as_datastar_event()
    }
}

impl From<PatchElements> for DatastarEvent {
    #[inline]
    fn from(val: PatchElements) -> Self {
        val.into_datastar_event()
    }
}
