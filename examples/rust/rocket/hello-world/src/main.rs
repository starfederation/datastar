use {
    core::time::Duration,
    datastar::prelude::PatchElements,
    rocket::{
        get, launch,
        response::{content::RawHtml, stream::EventStream},
        routes,
        serde::{Deserialize, json::Json},
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
fn hello_world(datastar: Json<Signals>) -> EventStream![] {
    EventStream! {
        for i in 0..MESSAGE.len() {
            let elements = format!("<div id='message'>{}</div>", &MESSAGE[0..i + 1]);
            let patch = PatchElements::new(elements);
            let sse_event = patch.write_as_rocket_sse_event();

            yield sse_event;

            rocket::tokio::time::sleep(Duration::from_millis(datastar.delay)).await;
        }
    }
}
