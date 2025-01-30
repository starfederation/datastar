use {
    datastar::prelude::MergeFragments,
    rocket::{
        get, launch,
        response::{content::RawHtml, stream::EventStream},
        routes,
    },
    std::time::Duration,
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

#[get("/hello-world")]
fn hello_world() -> EventStream![] {
    EventStream! {
        let mut i = 0;

        while i < MESSAGE.len() {
            i += 1;
            yield MergeFragments::new(format!("<div id='message'>{}</div>", &MESSAGE[0..i])).into();
            std::thread::sleep(Duration::from_millis(100));
        }
    }
}
