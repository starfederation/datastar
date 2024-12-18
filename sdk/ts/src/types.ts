import {
    FragmentMergeModes,
    EventTypes,
    DatastarDatalineSelector,
    DatastarDatalineUseViewTransition,
    DatastarDatalineSettleDuration,
    DatastarDatalineMergeMode,
    DatastarDatalineOnlyIfMissing,
    DatastarDatalineSignals,
    DatastarDatalineAutoRemove,
    DatastarDatalineAttributes
} from "./consts.ts";

export type FragmentMergeMode = typeof FragmentMergeModes[number];
export type EventType = typeof EventTypes[number];

export interface DatastarEventOptions {
    id?: string;
    retryDuration?: number;
};

export interface DatastarEvent {
    event: EventType;
};

export interface FragmentOptions extends DatastarEventOptions {
    [DatastarDatalineUseViewTransition]?: boolean;
}

export interface MergeFragmentsOptions extends FragmentOptions {
    [DatastarDatalineSettleDuration]?: number;
    [DatastarDatalineMergeMode]?: FragmentMergeMode;
    [DatastarDatalineSelector]?: string;
};

export interface MergeFragmentsEvent extends DatastarEvent, MergeFragmentsOptions {
    event: "datastar-merge-fragments";
};

export interface RemoveFragmentsEvent extends DatastarEvent, FragmentOptions {
    [DatastarDatalineSelector]: string;
    event: "datastar-remove-fragments";
};

export interface MergeSignalsOptions extends DatastarEventOptions {
    [DatastarDatalineOnlyIfMissing]?: boolean;
};

export interface MergeSignalsEvent extends DatastarEvent, MergeSignalsOptions {
    event: "datastar-merge-signals";
    [DatastarDatalineSignals]: string;
};

export interface RemoveSignalsEvent extends DatastarEvent, DatastarEventOptions  {
    event: "datastar-remove-signals";
}

export interface ExecuteScriptOptions extends DatastarEventOptions  {
    [DatastarDatalineAutoRemove]?: boolean;
    [DatastarDatalineAttributes]?: string;
}

export interface ExecuteScriptEvent extends DatastarEvent, ExecuteScriptOptions {
    event: "datastar-execute-script";
}

export const sseHeaders = {
    "Cache-Control": "no-cache",
    "Connection": "keep-alive",
    "Content-Type": "text/event-stream"
} as const;

export type SseHeader = typeof sseHeaders;