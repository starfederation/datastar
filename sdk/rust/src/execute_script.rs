//! [`ExecuteScript`] is used to execute JavaScript in the browser using the `datastar-execute-script` event.

use {crate::ServerSentEventGenerator, core::time::Duration};

/// [`ExecuteScript`] is used to execute JavaScript in the browser
/// using the `datastar-execute-script` event.
///
/// See the [Datastar documentation](https://data-star.dev/reference/plugins_backend#datastar-sse-events).
///
/// # Examples
///
/// ```
/// use datastar::prelude::{ServerSentEventGenerator, ExecuteScript};
///
/// let execute_script: String = ExecuteScript::new("console.log('Hello, world!')")
///     .auto_remove(false)
///     .attributes(["type text/javascript"])
///     .send();
///
/// let expected: &str = "event: datastar-execute-script
/// data: autoRemove false
/// data: attributes type text/javascript
/// data: script console.log('Hello, world!')
///
/// ";
///
/// assert_eq!(execute_script, expected);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]

pub struct ExecuteScript {
    /// This can be used by the backend to replay events.
    /// This is part of the SSE spec and is used to tell the browser how to handle the event.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#id
    pub id: Option<String>,
    /// This is part of the SSE spec and is used to tell the browser how long to wait before reconnecting if the connection is lost.
    /// For more details see https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#retry
    pub retry_duration: Duration,
    /// Each element contains JavaScript to be executed by the browser
    pub script: String,
    /// Determines whether to remove the script after execution.
    pub auto_remove: bool,
    /// Each element adds an attribute (in the format `name value`) to the `script` tag
    pub attributes: Vec<String>,
}

impl ExecuteScript {
    /// Creates a new [`ExecuteScript`] event with the given script.
    pub fn new(script: impl Into<String>) -> Self {
        Self {
            id: Default::default(),
            retry_duration: Duration::from_millis(1000),
            script: script.into(),
            auto_remove: true,
            attributes: vec!["type module".into()],
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

    /// Adds a script to the [`ExecuteScript`] event.
    pub fn auto_remove(mut self, auto_remove: bool) -> Self {
        self.auto_remove = auto_remove;
        self
    }

    /// Adds an attribute to the [`ExecuteScript`] event.
    pub fn attributes(mut self, attributes: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.attributes = attributes.into_iter().map(Into::into).collect();
        self
    }
}

impl ServerSentEventGenerator for ExecuteScript {
    fn send(&self) -> String {
        let mut result = String::new();

        result.push_str("event: datastar-execute-script\n");

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

        if !self.auto_remove {
            result.push_str("data: autoRemove false\n");
        }

        for attribute in &self.attributes {
            result.push_str("data: attributes ");
            result.push_str(attribute);
            result.push_str("\n");
        }

        for line in self.script.lines() {
            result.push_str("data: script ");
            result.push_str(line);
            result.push_str("\n");
        }

        result.push_str("\n");

        result
    }
}
