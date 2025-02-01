use {serde::Deserialize, serde_json::Value};

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
    pub events: Vec<Value>,
}
