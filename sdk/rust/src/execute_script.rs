//! [`ExecuteScript`] executes JavaScript in the browser.
//!
//! This is sugar for `PatchElements` specifically for executing scripts.

use {
    crate::{
        DatastarEvent,
        consts::{self, ElementPatchMode},
    },
    core::time::Duration,
};

/// [`ExecuteScript`] executes JavaScript in the browser
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExecuteScript {
    /// `id` can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see <https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id>
    pub id: Option<String>,
    /// `retry` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see <https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry>
    pub retry: Duration,
    /// `script` is a string that represents the JavaScript to be executed by the browser.
    pub script: String,
    /// Whether to remove the script after execution, if not provided the Datastar client side will default to `true`.
    pub auto_remove: Option<bool>,
    /// A list of attributes to add to the script element, if not provided the Datastar client side will default to `type="module"`.
    /// Each item in the array ***must*** be properly formatted.
    pub attributes: Vec<String>,
}

impl ExecuteScript {
    /// Creates a new [`ExecuteScript`] event with the given script.
    pub fn new(script: impl Into<String>) -> Self {
        Self {
            id: None,
            retry: Duration::from_millis(consts::DEFAULT_SSE_RETRY_DURATION),
            script: script.into(),
            auto_remove: Default::default(),
            attributes: Default::default(),
        }
    }

    /// Sets the `id` of the [`ExecuteScript`] event.
    pub fn id(mut self, id: impl Into<String>) -> Self {
        self.id = Some(id.into());
        self
    }

    /// Sets the `retry` of the [`ExecuteScript`] event.
    pub fn retry(mut self, retry: Duration) -> Self {
        self.retry = retry;
        self
    }

    /// Sets the `script` of the [`ExecuteScript`] event.
    pub fn auto_remove(mut self, auto_remove: bool) -> Self {
        self.auto_remove = Some(auto_remove);
        self
    }

    /// Sets the `attribute` of the [`ExecuteScript`] event.
    pub fn attributes(mut self, attributes: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.attributes = attributes.into_iter().map(Into::into).collect();
        self
    }

    /// Converts this [`ExecuteScript`] into a [`DatastarEvent`].
    #[inline]
    pub fn into_datastar_event(mut self) -> DatastarEvent {
        let id = self.id.take();
        self.convert_to_datastar_event_inner(id)
    }

    /// Copy this [`ExecuteScript`] as a [`DatastarEvent`].
    #[inline]
    pub fn as_datastar_event(&self) -> DatastarEvent {
        self.convert_to_datastar_event_inner(self.id.clone())
    }

    fn convert_to_datastar_event_inner(&self, id: Option<String>) -> DatastarEvent {
        let mut data: Vec<String> = Vec::new();

        data.push(format!("{} body", consts::SELECTOR_DATALINE_LITERAL));

        data.push(format!(
            "{} {}",
            consts::MODE_DATALINE_LITERAL,
            ElementPatchMode::Append.as_str(),
        ));

        let mut s = format!("{} <script", consts::ELEMENTS_DATALINE_LITERAL);

        if !self.auto_remove.unwrap_or(true) {
            s.push_str(r##" data-effect="el.remove()""##);
        }

        for attribute in &self.attributes {
            s.push(' ');
            s.push_str(attribute.as_str());
        }

        s.push('>');
        data.push(s);

        for line in self.script.lines() {
            data.push(format!("{} {}", consts::ELEMENTS_DATALINE_LITERAL, line));
        }

        data.push(format!("{} </script>", consts::ELEMENTS_DATALINE_LITERAL));

        DatastarEvent {
            event: consts::EventType::PatchElements,
            id,
            retry: self.retry,
            data,
        }
    }
}

impl From<&ExecuteScript> for DatastarEvent {
    #[inline]
    fn from(val: &ExecuteScript) -> Self {
        val.as_datastar_event()
    }
}

impl From<ExecuteScript> for DatastarEvent {
    #[inline]
    fn from(val: ExecuteScript) -> Self {
        val.into_datastar_event()
    }
}
