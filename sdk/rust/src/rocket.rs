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
            testing::{self, Signals, base_test_server},
        },
        futures_util::Stream,
        rocket::{
            Config, Responder, get, post, response::content::RawHtml, routes, serde::json::Json,
        },
        std::time::Duration,
        tracing_test::traced_test,
    };

    #[tokio::test]
    #[traced_test]
    async fn sdk_base_test() -> Result<(), Box<dyn core::error::Error>> {
        fn get_available_port() -> Option<u16> {
            fn port_is_available(port: u16) -> bool {
                std::net::TcpListener::bind(("127.0.0.1", port)).is_ok()
            }
            (8000..9000).find(|port| port_is_available(*port))
        }

        let available_port = get_available_port().expect("find available tcp port");
        let base_url = format!("http://127.0.0.1:{available_port}");

        println!("base url: {base_url}");

        tokio::time::sleep(Duration::from_secs(1)).await;

        let server = rocket::build()
            .mount(
                "/base",
                routes![
                    base_test_endpoint_required_get,
                    base_test_endpoint_required_post,
                    base_test_endpoint_optional_get,
                    base_test_endpoint_optional_post,
                ],
            )
            .configure(Config {
                port: available_port,
                address: std::net::Ipv4Addr::new(127, 0, 0, 1).into(),
                ..Config::debug_default()
            })
            .ignite()
            .await
            .expect("ignite the rocket server");

        let rocket_shutdown = server.shutdown();

        let (server_result_tx, server_result_rx) = tokio::sync::oneshot::channel();
        tokio::spawn(async move {
            server_result_tx
                .send(server.launch().await)
                .expect("send rocket serve result upstream over oneshot ch");
        });

        tokio::time::sleep(Duration::from_secs(1)).await;

        base_test_server(&base_url).await;

        rocket_shutdown.notify();
        server_result_rx.await??;

        Ok(())
    }

    #[get("/test?<datastar>")]
    fn base_test_endpoint_required_get(
        datastar: Json<Signals>,
    ) -> Sse<impl Stream<Item = DatastarEvent>> {
        Sse(testing::test(datastar.into_inner().events))
    }

    #[post("/test", data = "<datastar>")]
    fn base_test_endpoint_required_post(
        datastar: Json<Signals>,
    ) -> Sse<impl Stream<Item = DatastarEvent>> {
        Sse(testing::test(datastar.into_inner().events))
    }

    #[derive(Responder)]
    enum PageOrEvents<S: Stream<Item = DatastarEvent>> {
        Html(RawHtml<&'static str>),
        Events(Sse<S>),
    }

    #[get("/test-opt?<datastar>")]
    fn base_test_endpoint_optional_get(
        datastar: Option<Json<Signals>>,
    ) -> PageOrEvents<impl Stream<Item = DatastarEvent>> {
        match datastar {
            Some(datastar) => {
                PageOrEvents::Events(Sse(testing::test(datastar.into_inner().events)))
            }
            None => PageOrEvents::Html(RawHtml("<p>Hello</p>")),
        }
    }

    #[post("/test-opt", data = "<datastar>")]
    fn base_test_endpoint_optional_post(
        datastar: Option<Json<Signals>>,
    ) -> PageOrEvents<impl Stream<Item = DatastarEvent>> {
        match datastar {
            Some(datastar) => {
                PageOrEvents::Events(Sse(testing::test(datastar.into_inner().events)))
            }
            None => PageOrEvents::Html(RawHtml("<p>Hello</p>")),
        }
    }
}
