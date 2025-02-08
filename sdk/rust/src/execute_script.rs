//! [`ExecuteScript`] executes JavaScript in the browser.

use {
    crate::{consts, DatastarEvent},
    core::time::Duration,
};

/// [`ExecuteScript`] executes JavaScript in the browser
///
/// See the [Datastar documentation](https://data-star.dev/reference/sse_events#datastar-execute-script).
///
/// # Examples
///
/// ```
/// use datastar::prelude::{Sse, ExecuteScript};
/// use async_stream::stream;
///
/// Sse(stream! {
///     yield ExecuteScript::new("console.log('Hello, world!')")
///         .auto_remove(false)
///         .attributes(["type text/javascript"])
///         .into();
/// });
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]

pub struct ExecuteScript {
    /// `id` can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    pub id: Option<String>,
    /// `retry` is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
    pub retry: Duration,
    /// `script` is a string that represents the JavaScript to be executed by the browser.
    pub script: String,
    /// Whether to remove the script after execution, if not provided the Datastar client side will default to `true`.
    pub auto_remove: bool,
    /// A list of attributes to add to the script element, if not provided the Datastar client side will default to `type module`.
    /// Each item in the array ***must*** be a string in the format `key value`.
    pub attributes: Vec<String>,
}

impl ExecuteScript {
    /// Creates a new [`ExecuteScript`] event with the given script.
    pub fn new(script: impl Into<String>) -> Self {
        Self {
            id: None,
            retry: Duration::from_millis(consts::DEFAULT_SSE_RETRY_DURATION),
            script: script.into(),
            auto_remove: consts::DEFAULT_EXECUTE_SCRIPT_AUTO_REMOVE,
            attributes: vec![consts::DEFAULT_EXECUTE_SCRIPT_ATTRIBUTES.to_string()],
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
        self.auto_remove = auto_remove;
        self
    }

    /// Sets the `attribute` of the [`ExecuteScript`] event.
    pub fn attributes(mut self, attributes: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.attributes = attributes.into_iter().map(Into::into).collect();
        self
    }
}

impl From<ExecuteScript> for DatastarEvent {
    fn from(val: ExecuteScript) -> Self {
        let mut data: Vec<String> = Vec::new();

        if val.auto_remove != consts::DEFAULT_EXECUTE_SCRIPT_AUTO_REMOVE {
            data.push(format!(
                "{} {}",
                consts::AUTO_REMOVE_DATALINE_LITERAL,
                val.auto_remove
            ));
        }

        if val.attributes.len() != 1
            || val.attributes[0] != consts::DEFAULT_EXECUTE_SCRIPT_ATTRIBUTES
        {
            for attribute in &val.attributes {
                data.push(format!(
                    "{} {}",
                    consts::ATTRIBUTES_DATALINE_LITERAL,
                    attribute
                ));
            }
        }

        for line in val.script.lines() {
            data.push(format!("{} {}", consts::SCRIPT_DATALINE_LITERAL, line));
        }

        Self {
            event: consts::EventType::ExecuteScript,
            id: val.id,
            retry: val.retry,
            data,
        }
    }
}
