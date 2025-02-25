use {
    crate::{
        consts::{self, FragmentMergeMode},
        prelude::*,
    },
    async_stream::stream,
    core::time::Duration,
    futures_util::Stream,
    serde::Deserialize,
    serde_json::Value,
};

#[derive(Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum TestEvent {
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
        settle_duration: Option<u64>,
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
        settle_duration: Option<u64>,
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
pub struct Signals {
    pub events: Vec<TestEvent>,
}

pub fn test(events: Vec<TestEvent>) -> impl Stream<Item = DatastarEvent> {
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
                    }.into()
                },
                TestEvent::MergeFragments {
                    fragments,
                    event_id,
                    retry_duration,
                    selector,
                    merge_mode,
                    settle_duration,
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
                        settle_duration: Duration::from_millis(settle_duration.unwrap_or(consts::DEFAULT_FRAGMENTS_SETTLE_DURATION)),
                        use_view_transition: use_view_transition.unwrap_or(consts::DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS),
                    }.into()
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
                }.into(),
                TestEvent::RemoveFragments {
                    selector,
                    event_id,
                    retry_duration,
                    settle_duration,
                    use_view_transition,
                } => RemoveFragments {
                    selector,
                    id: event_id,
                    retry: Duration::from_millis(retry_duration.unwrap_or(consts::DEFAULT_SSE_RETRY_DURATION)),
                    settle_duration: Duration::from_millis(settle_duration.unwrap_or(consts::DEFAULT_FRAGMENTS_SETTLE_DURATION)),
                    use_view_transition: use_view_transition.unwrap_or(consts::DEFAULT_FRAGMENTS_USE_VIEW_TRANSITIONS),
                }.into(),
                TestEvent::RemoveSignals {
                    paths,
                    event_id,
                    retry_duration,
                } => RemoveSignals {
                    paths,
                    id: event_id,
                    retry: Duration::from_millis(retry_duration.unwrap_or(consts::DEFAULT_SSE_RETRY_DURATION)),
                }.into(),
            }
        }
    }
}
