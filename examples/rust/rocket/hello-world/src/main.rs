use {
    core::time::Duration,
    datastar::{prelude::MergeFragments, DatastarEvent, Sse},
    rocket::{
        futures::Stream,
        get, launch,
        response::{content::RawHtml, stream::stream},
        routes,
        serde::{json::Json, Deserialize},
    },
};

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/", routes![index, hello_world])
}

#[get("/")]
fn index() -> RawHtml<String> {
    RawHtml(std::fs::read_to_string("hello-world.html").unwrap())
}

const MESSAGE: &str = "Hello, world!";

#[derive(Deserialize)]
#[serde(crate = "rocket::serde")]
struct Signals {
    delay: u64,
}

#[get("/hello-world?<datastar>")]
fn hello_world(datastar: Json<Signals>) -> Sse<impl Stream<Item = DatastarEvent>> {
    Sse(stream! {
        for i in 0..MESSAGE.len() {
            yield MergeFragments::new(format!("<div id='message'>{}</div>", &MESSAGE[0..i+1])).into();
            rocket::tokio::time::sleep(Duration::from_millis(datastar.delay)).await;
        }
    })
}
