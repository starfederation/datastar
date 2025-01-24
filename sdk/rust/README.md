# Datastar Rust SDK

An implementation of the Datastar SDK in Rust. Currently, this only has framework integration for `axum`.

# Usage

## Execute Script

```rust
use datastar::prelude::{ServerSentEventGenerator, ExecuteScript};

let execute_script: String = ExecuteScript::new("console.log('Hello, world!')")
    .auto_remove(false)
    .attributes(["type text/javascript"])
    .send();

let expected: &str = "event: datastar-execute-script
data: autoRemove false
data: attributes type text/javascript
data: script console.log('Hello, world!')

";

assert_eq!(execute_script, expected);
```

## Merge Fragments

```rust
use {
    core::time::Duration,
    datastar::prelude::{ServerSentEventGenerator, MergeFragments, MergeMode}
};

let merge_fragments: String = MergeFragments::new("<h1>Hello, world!</h1>")
    .selector("body")
    .merge_mode(MergeMode::Append)
    .settle_duration(Duration::from_millis(1000))
    .use_view_transition(true)
    .send();

let expected: &str = "event: datastar-merge-fragments
data: selector body
data: mergeMode append
data: settleDuration 1000
data: useViewTransition true
data: fragments <h1>Hello, world!</h1>

";

assert_eq!(merge_fragments, expected);
```

## Merge Signals

 ```rust
use datastar::prelude::{ServerSentEventGenerator, MergeSignals};

let merge_signals: String = MergeSignals::new("{foo: 1234}")
    .only_if_missing(true)
    .send();

let expected: &str = "event: datastar-merge-signals
data: onlyIfMissing true
data: signals {foo: 1234}

";

assert_eq!(merge_signals, expected);
```

## Remove Fragments

```rust
use datastar::prelude::{ServerSentEventGenerator, RemoveFragments};

let remove_fragments: String = RemoveFragments::new("#foo").send();

let expected: &str = "event: datastar-remove-fragments
data: selector #foo

";

assert_eq!(remove_fragments, expected);
```

## Remove Signals

```rust
use datastar::prelude::{ServerSentEventGenerator, RemoveSignals};

let remove_signals: String = RemoveSignals::new(["foo.bar", "1234", "abc"]).send();

let expected: &str = r#"event: datastar-remove-signals
data: paths foo.bar
data: paths 1234
data: paths abc

"#;

assert_eq!(remove_signals, expected);
```

## Axum Infinite Scroll Example

```rust
pub async fn infinite_scroll(ReadSignals(mut signals): ReadSignals<Signals>) -> Response {
    signals.limit = signals.limit.clamp(1, 100);

    match signals.offset {
        0 => {
            let agents = (0..signals.limit).map(|i| AgentPartial { i }).collect();

            let partial = AgentsPartial {
                agents,
                more: MorePartial {
                    offset: signals.offset + signals.limit,
                    limit: signals.limit,
                },
                signals,
            };

            MergeFragments::new(partial.render().unwrap()).into_response()
        }
        offset if offset < 100 => {
            let more = MergeFragments::new(
                MorePartial {
                    offset: offset + signals.limit,
                    limit: signals.limit,
                }
                .render()
                .unwrap(),
            );

            let agents = (0..signals.limit)
                .map(|i| {
                    MergeFragments::new(AgentPartial { i: offset + i }.render().unwrap())
                        .selector("#click_to_load_rows")
                        .merge_mode(MergeMode::Append)
                })
                .collect::<Vec<_>>();

            (more, agents).into_response()
        }
        _ => MergeFragments::new(RickRollPartial.render().unwrap()).into_response(),
    }
}
```