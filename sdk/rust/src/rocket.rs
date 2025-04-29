//! Rocket integration for Datastar.

use {
    crate::{Sse, TrySse, prelude::DatastarEvent},
    core::error::Error,
    futures_util::{Stream, StreamExt},
    rocket::{
        Request, Response,
        http::ContentType,
        response::{self, Responder, stream::ReaderStream},
    },
    std::io::Cursor,
};

impl<'r, S, I> Responder<'r, 'r> for Sse<S>
where
    S: Stream<Item = I> + Send + 'static,
    I: Into<DatastarEvent> + Send + 'static,
{
    fn respond_to(self, _: &'r Request<'_>) -> response::Result<'r> {
        let stream = self.0.map(|event| Cursor::new(event.into().to_string()));

        let mut response = Response::build();

        #[cfg(not(feature = "http2"))]
        response.raw_header("Connection", "keep-alive");

        response
            .header(ContentType::EventStream)
            .raw_header("Cache-Control", "no-cache")
            .streamed_body(ReaderStream::from(stream))
            .ok()
    }
}

impl<'r, S, I, E> Responder<'r, 'r> for TrySse<S>
where
    E: Into<Box<dyn Error + Send + Sync>> + Send + 'r,
    S: Stream<Item = Result<I, E>> + Send + 'static,
    I: Into<DatastarEvent> + Send + 'static,
{
    fn respond_to(self, _: &'r Request<'_>) -> response::Result<'r> {
        // we just ignore errors because rocket doesn't support them in streams!
        let stream = self.0.filter_map(|event| async {
            match event {
                Ok(event) => Some(Cursor::new(event.into().to_string())),
                _ => None,
            }
        });

        let mut response = Response::build();

        #[cfg(not(feature = "http2"))]
        response.raw_header("Connection", "keep-alive");

        response
            .header(ContentType::EventStream)
            .raw_header("Cache-Control", "no-cache")
            .streamed_body(ReaderStream::from(stream))
            .ok()
    }
}

#[cfg(test)]
mod tests {
    use {
        crate::{
            DatastarEvent, Sse,
            testing::{self, Signals},
        },
        futures_util::Stream,
        rocket::{get, post, routes, serde::json::Json},
    };

    #[tokio::test]
    #[ignore]
    async fn sdk_test() {
        rocket::build()
            .mount("/", routes![get_test, post_test])
            .launch()
            .await
            .unwrap();
    }

    #[get("/test?<datastar>")]
    fn get_test(datastar: Json<Signals>) -> Sse<impl Stream<Item = DatastarEvent>> {
        Sse(testing::test(datastar.into_inner().events))
    }

    #[post("/test", data = "<datastar>")]
    fn post_test(datastar: Json<Signals>) -> Sse<impl Stream<Item = DatastarEvent>> {
        Sse(testing::test(datastar.into_inner().events))
    }
}
