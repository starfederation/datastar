//! Rocket integration for Datastar.

use {
    crate::{
        DatastarEvent,
        prelude::{PatchElements, PatchSignals},
    },
    rocket::response::stream::Event,
    std::fmt::Write,
};

impl PatchElements {
    /// Write this [`PatchElements`] into a Rocket SSE [`Event`].
    pub fn write_as_rocket_sse_event(&self) -> Event {
        self.as_datastar_event().write_as_rocket_sse_event()
    }
}

impl PatchSignals {
    /// Write this [`PatchSignals`] into a Rocket SSE [`Event`].
    pub fn write_as_rocket_sse_event(&self) -> Event {
        self.as_datastar_event().write_as_rocket_sse_event()
    }
}

impl DatastarEvent {
    /// Turn this [`DatastarEvent`] into a Rocket SSE [`Event`].
    pub fn write_as_rocket_sse_event(&self) -> Event {
        let mut data = String::with_capacity(
            self.data.iter().map(|s| s.len()).sum::<usize>() + self.data.len() - 1,
        );

        let mut sep = "";
        for line in self.data.iter() {
            // Assumption: std::fmt::write does not fail ever for [`String`].
            let _ = write!(&mut data, "{sep}{line}");
            sep = "\n";
        }

        let event = Event::data(data)
            .event(self.event.as_str().to_owned())
            .with_retry(self.retry);

        match self.id.as_deref() {
            Some(id) => event.id(id.to_owned()),
            None => event,
        }
    }
}
