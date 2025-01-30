//! Rocket integration for Datastar.

use {
    crate::{
        prelude::{ExecuteScript, MergeFragments, MergeSignals, RemoveFragments, RemoveSignals},
        DatastarEvent,
    },
    rocket::{
        http::ContentType,
        response::{self, stream::Event, Responder},
        Request, Response,
    },
    std::io::Cursor,
};

impl Into<Event> for DatastarEvent {
    fn into(self) -> Event {
        let mut event = Event::empty().event(self.event.as_str().to_owned());

        if let Some(id) = self.id {
            event = event.id(id);
        }

        if self.retry.as_millis() != crate::consts::DEFAULT_SSE_RETRY_DURATION as u128 {
            event = event.with_retry(self.retry);
        }

        event.with_data(self.data.join("\n"))
    }
}

#[rocket::async_trait]
impl<'r> Responder<'r, 'static> for DatastarEvent {
    fn respond_to(self, _: &'r Request<'_>) -> response::Result<'static> {
        let body = self.to_string();

        Response::build()
            .header(ContentType::EventStream)
            .raw_header("Cache-Control", "no-cache")
            .raw_header("Connection", "keep-alive")
            .sized_body(body.len(), Cursor::new(body))
            .ok()
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

            #[rocket::async_trait]
            impl<'r> Responder<'r, 'static> for $type {
                fn respond_to(self, req: &'r Request<'_>) -> response::Result<'static> {
                    let event: DatastarEvent = self.into();
                    event.respond_to(req)
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
