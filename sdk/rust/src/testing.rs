use {
    crate::{
        consts::{self, FragmentMergeMode},
        prelude::*,
    },
    async_stream::stream,
    core::time::Duration,
    futures_util::Stream,
    reqwest::{Method, StatusCode},
    serde::{Deserialize, Serialize},
    serde_json::{Value, json},
    tokio_stream::StreamExt,
};

#[derive(Serialize, Deserialize, Clone)]
#[serde(tag = "type", rename_all = "camelCase")]
pub(crate) enum TestEvent {
    #[serde(rename_all = "camelCase")]
    ExecuteScript {
        script: String,
        event_id: Option<String>,
        retry_duration: Option<u64>,
        attributes: Option<Value>,
        auto_remove: Option<bool>,
    },
    #[serde(rename_all = "camelCase")]
    MergeFragments {
        fragments: String,
        event_id: Option<String>,
        retry_duration: Option<u64>,
        selector: Option<String>,
        merge_mode: Option<String>,
        use_view_transition: Option<bool>,
    },
    #[serde(rename_all = "camelCase")]
    MergeSignals {
        signals: Value,
        event_id: Option<String>,
        retry_duration: Option<u64>,
        only_if_missing: Option<bool>,
    },
    #[serde(rename_all = "camelCase")]
    RemoveFragments {
        selector: String,
        event_id: Option<String>,
        retry_duration: Option<u64>,
        use_view_transition: Option<bool>,
    },
    #[serde(rename_all = "camelCase")]
    RemoveSignals {
        paths: Vec<String>,
        event_id: Option<String>,
        retry_duration: Option<u64>,
    },
}

#[derive(Deserialize)]
#[allow(unused)]
pub(crate) struct Signals {
    pub(crate) events: Vec<TestEvent>,
}

impl TestEvent {
    #[allow(unused)]
    pub(crate) fn test_events() -> Vec<TestEvent> {
        vec![
            TestEvent::ExecuteScript {
                script: "console.log('Hello, World!');".to_owned(),
                event_id: Some("exec-1".to_owned()),
                retry_duration: Some(1000),
                attributes: Some(serde_json::json!({
                    "data-test": "true",
                    "role": "script"
                })),
                auto_remove: Some(true),
            },
            TestEvent::MergeFragments {
                fragments: "<div>New Content</div>".to_owned(),
                event_id: Some("merge-1".to_owned()),
                retry_duration: Some(1500),
                selector: Some("#target".to_owned()),
                merge_mode: Some("append".to_owned()),
                use_view_transition: Some(false),
            },
            TestEvent::MergeSignals {
                signals: serde_json::json!({
                    "user": {
                        "name": "Alice",
                        "loggedIn": true
                    }
                }),
                event_id: Some("signal-1".to_owned()),
                retry_duration: Some(1200),
                only_if_missing: Some(false),
            },
            TestEvent::RemoveFragments {
                selector: ".to-remove".to_owned(),
                event_id: Some("remove-frag-1".to_owned()),
                retry_duration: Some(800),
                use_view_transition: Some(true),
            },
            TestEvent::RemoveSignals {
                paths: vec!["user.name".to_owned(), "user.loggedIn".to_owned()],
                event_id: Some("remove-signal-1".to_owned()),
                retry_duration: Some(700),
            },
        ]
    }
}

pub(crate) fn test(events: Vec<TestEvent>) -> impl Stream<Item = DatastarEvent> + Send + 'static {
    stream! {
        for event in events {
            yield match event {
                TestEvent::ExecuteScript {
                    script,
                    event_id,
                    retry_duration,
                    attributes,
                    auto_remove,
                } => {
                    let attributes = attributes
                        .map(|attrs| {
                            attrs
                                .as_object()
                                .unwrap()
                                .iter()
                                .map(|(name, value)| {
                                    format!("{} {}", name, value.as_str().unwrap())
                                })
                                .collect()
                        })
                        .unwrap_or(vec![]);

                    ExecuteScript {
                        script,
                        id: event_id,
                        retry: Duration::from_millis(retry_duration.unwrap_or(consts::DEFAULT_SSE_RETRY_DURATION)),
                        attributes,
                        auto_remove: auto_remove.unwrap_or(consts::DEFAULT_EXECUTE_SCRIPT_AUTO_REMOVE),
                    }.into_event()
                },
                TestEvent::MergeFragments {
                    fragments,
                    event_id,
                    retry_duration,
                    selector,
                    merge_mode,
                    use_view_transition,
                } => {
                    let merge_mode = merge_mode
                        .map(|mode| match mode.as_str() {
                            "morph" => FragmentMergeMode::Morph,
                            "inner" => FragmentMergeMode::Inner,
                            "outer" => FragmentMergeMode::Outer,
                            "prepend" => FragmentMergeMode::Prepend,
                            "append" => FragmentMergeMode::Append,
                            "before" => FragmentMergeMode::Before,
                            "after" => FragmentMergeMode::After,
                            "upsertAttributes" => FragmentMergeMode::UpsertAttributes,
                            _ => unreachable!(),
                        })
                        .unwrap_or_default();

                    MergeFragments {
                        fragments,
                        id: event_id,
                        retry: Duration::from_millis(retry_duration.unwrap_or(consts::DEFAULT_SSE_RETRY_DURATION)),
                        selector,
                        merge_mode,
                        use_view_transition: use_view_transition.unwrap_or(consts::DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS),
                    }.into_event()
                },
                TestEvent::MergeSignals {
                    signals,
                    event_id,
                    retry_duration,
                    only_if_missing,
                } => MergeSignals {
                    signals: serde_json::to_string(&signals).unwrap(),
                    id: event_id,
                    retry: Duration::from_millis(retry_duration.unwrap_or(consts::DEFAULT_SSE_RETRY_DURATION)),
                    only_if_missing: only_if_missing.unwrap_or(consts::DEFAULT_MERGE_SIGNALS_ONLY_IF_MISSING),
                }.into_event(),
                TestEvent::RemoveFragments {
                    selector,
                    event_id,
                    retry_duration,
                    use_view_transition,
                } => RemoveFragments {
                    selector,
                    id: event_id,
                    retry: Duration::from_millis(retry_duration.unwrap_or(consts::DEFAULT_SSE_RETRY_DURATION)),
                    use_view_transition: use_view_transition.unwrap_or(consts::DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS),
                }.into_event(),
                TestEvent::RemoveSignals {
                    paths,
                    event_id,
                    retry_duration,
                } => RemoveSignals {
                    paths,
                    id: event_id,
                    retry: Duration::from_millis(retry_duration.unwrap_or(consts::DEFAULT_SSE_RETRY_DURATION)),
                }.into_event(),
            }
        }
    }
}

#[test]
fn test_deserialize_get_json_events() {
    let _signals: Signals = serde_json::from_str(r##"{"events":[{"attributes":{"data-test":"true","role":"script"},"autoRemove":true,"eventId":"exec-1","retryDuration":1000,"script":"console.log('Hello, World!');","type":"executeScript"},{"eventId":"merge-1","fragments":"<div>New Content</div>","mergeMode":"append","retryDuration":1500,"selector":"#target","type":"mergeFragments","useViewTransition":false},{"eventId":"signal-1","onlyIfMissing":false,"retryDuration":1200,"signals":{"user":{"loggedIn":true,"name":"Alice"}},"type":"mergeSignals"},{"eventId":"remove-frag-1","retryDuration":800,"selector":".to-remove","type":"removeFragments","useViewTransition":true},{"eventId":"remove-signal-1","paths":["user.name","user.loggedIn"],"retryDuration":700,"type":"removeSignals"}]}"##).unwrap();
}

#[allow(unused)]
pub(crate) async fn base_test_server(base_url: &str) {
    let client = reqwest::Client::new();
    let events = TestEvent::test_events();

    // Helper functions
    async fn do_get(
        client: &reqwest::Client,
        url: &str,
        body: Option<&Vec<TestEvent>>,
    ) -> reqwest::Response {
        match body {
            Some(events) => {
                let json_string = serde_json::to_string(&json!({
                    "events": events,
                }))
                .expect("Failed to serialize events");
                client
                    .get(url)
                    .query(&[("datastar", json_string)])
                    .header(
                        if body.is_some() {
                            "datastar-request"
                        } else {
                            "normal-request"
                        },
                        "1",
                    )
                    .send()
                    .await
                    .expect("GET request failed")
            }
            None => client.get(url).send().await.expect("GET request failed"),
        }
    }

    async fn do_post(
        client: &reqwest::Client,
        url: &str,
        body: Option<&Vec<TestEvent>>,
    ) -> reqwest::Response {
        match body {
            Some(events) => client
                .post(url)
                .header(
                    if body.is_some() {
                        "datastar-request"
                    } else {
                        "normal-request"
                    },
                    "1",
                )
                .json(&json!({
                    "events": events,
                }))
                .send()
                .await
                .expect("POST request failed"),
            None => client.post(url).send().await.expect("POST request failed"),
        }
    }

    // Test broken endpoint (404)
    let res = do_get(&client, &format!("{}/base/broken", base_url), None).await;
    assert_eq!(res.status(), StatusCode::NOT_FOUND);
    println!("✔️ Broken endpoint returns 404");

    // Define tests with events
    let eventful_tests = vec![
        ("/base/test", Method::GET, Some(&events)),
        ("/base/test", Method::POST, Some(&events)),
        ("/base/test-opt", Method::GET, Some(&events)),
        ("/base/test-opt", Method::POST, Some(&events)),
    ];

    for (path, method, body) in eventful_tests {
        let url = format!("{}{}", base_url, path);
        let response = match path {
            "/base/test" | "/base/test-opt" if method == Method::GET => {
                do_get(&client, &url, body).await
            }
            _ => do_post(&client, &url, body).await,
        };

        assert_eq!(
            response.status(),
            StatusCode::OK,
            "{method} {path} — with body?: {}",
            body.is_some()
        );

        // Expect streaming response
        let mut stream = response.bytes_stream();
        let mut event_count = 0;

        while let Some(Ok(_chunk)) = stream.next().await {
            event_count += 1;
            if event_count >= 2 {
                break; // We only need to validate 2 events streamed
            }
        }

        assert!(event_count >= 2, "Expected at least 2 events streamed");
        println!("✔️ {} {} streams events", method, path);
    }

    // Define tests without events
    let no_event_tests_html = vec![
        ("/base/test-opt", Method::GET),
        ("/base/test-opt", Method::POST),
    ];

    for (path, method) in no_event_tests_html {
        let url = format!("{}{}", base_url, path);
        let response = match path {
            _ if method == Method::GET => do_get(&client, &url, None).await,
            _ => do_post(&client, &url, None).await,
        };

        assert_eq!(response.status(), StatusCode::OK, "{method} {path}",);

        let body = response.text().await.expect("Failed to read body");
        assert!(body.contains("Hello"), "Expected body to contain 'Hello'");
        println!("✔️ {} {} returns HTML page with Hello", method, path);
    }

    let no_event_tests_400 = vec![("/base/test", Method::GET), ("/base/test", Method::POST)];

    for (path, method) in no_event_tests_400 {
        let url = format!("{}{}", base_url, path);
        let response = match path {
            _ if method == Method::GET => do_get(&client, &url, None).await,
            _ => do_post(&client, &url, None).await,
        };

        assert_eq!(response.status(), StatusCode::BAD_REQUEST);
        println!("✔️ {} {} returns 400 Bad Request", method, path);
    }
}
