//! Rocket integration for Datastar.

use {
    crate::prelude::{
        DatastarEvent, ExecuteScript, MergeFragments, MergeSignals, RemoveFragments, RemoveSignals,
    },
    rocket::response::stream::Event,
};

impl Into<Event> for DatastarEvent {
    fn into(self) -> Event {
        let mut event = Event::empty();

        if let Some(id) = self.id {
            event = event.id(id);
        }

        event
            .event(self.event.as_str().to_owned())
            .with_retry(self.retry)
            .with_data(self.data.join("\n"))
    }
}

macro_rules! impls {
    ($($type:ty),*) => {
        $(
            impl Into<Event> for $type {
                fn into(self) -> Event {
                    let event: DatastarEvent = self.into();
                    event.into()
                }
            }
        )*
    };
}

impls!(
    ExecuteScript,
    MergeFragments,
    MergeSignals,
    RemoveFragments,
    RemoveSignals
);
