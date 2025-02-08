//! Rocket integration for Datastar.

use {
    crate::{prelude::DatastarEvent, Sse, TrySse},
    core::error::Error,
    futures_util::{Stream, StreamExt},
    rocket::{
        http::ContentType,
        response::{self, stream::ReaderStream, Responder},
        Request, Response,
    },
    std::io::Cursor,
};

impl<'r, S: Stream<Item = DatastarEvent> + Send + 'static> Responder<'r, 'r> for Sse<S> {
    fn respond_to(self, _: &'r Request<'_>) -> response::Result<'r> {
        let stream = self.0.map(|event| Cursor::new(event.to_string()));

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

impl<'r, S, E> Responder<'r, 'r> for TrySse<S>
where
    E: Into<Box<dyn Error + Send + Sync>> + Send + 'r,
    S: Stream<Item = Result<DatastarEvent, E>> + Send + 'static,
{
    fn respond_to(self, _: &'r Request<'_>) -> response::Result<'r> {
        // we just ignore errors because rocket doesn't support them in streams!
        let stream = self.0.filter_map(|event| async {
            match event {
                Ok(event) => Some(Cursor::new(event.to_string())),
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
            testing::{self, Signals},
            DatastarEvent, Sse,
        },
        futures_util::Stream,
        rocket::{get, post, routes, serde::json::Json},
    };

    #[tokio::test]
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
