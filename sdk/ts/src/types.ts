import {
    FragmentMergeModes,
    EventTypes,
    DatastarDatalineFragments,
    DatastarDatalinePaths,
    DatastarDatalineScript,
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

export interface FragmentOptions extends DatastarEventOptions {
    [DatastarDatalineUseViewTransition]?: boolean;
}

export interface MergeFragmentsOptions extends FragmentOptions {
    [DatastarDatalineSettleDuration]?: number;
    [DatastarDatalineMergeMode]?: FragmentMergeMode;
    [DatastarDatalineSelector]?: string;
};

export interface MergeFragmentsEvent {
    event: "datastar-merge-fragments";
    options: MergeFragmentsOptions;
    [DatastarDatalineFragments]: string;
};

export interface RemoveFragmentsEvent {
    event: "datastar-remove-fragments";
    options: FragmentOptions;
    [DatastarDatalineSelector]: string;
};

export interface MergeSignalsOptions extends DatastarEventOptions {
    [DatastarDatalineOnlyIfMissing]?: boolean;
};

export interface MergeSignalsEvent {
    event: "datastar-merge-signals";
    options: MergeSignalsOptions;
    [DatastarDatalineSignals]: string;
};

export interface RemoveSignalsEvent {
    event: "datastar-remove-signals";
    options: DatastarEventOptions;
    [DatastarDatalinePaths]: string;
}

export interface ExecuteScriptOptions extends DatastarEventOptions {
    [DatastarDatalineAutoRemove]?: boolean;
    [DatastarDatalineAttributes]?: string;
}

export interface ExecuteScriptEvent {
    event: "datastar-execute-script";
    options: ExecuteScriptOptions;
    [DatastarDatalineScript]: string;
}

export const sseHeaders = {
    "Cache-Control": "no-cache",
    "Connection": "keep-alive",
    "Content-Type": "text/event-stream"
} as const;

export type MultilineDatalinePrefix = typeof DatastarDatalineScript | typeof DatastarDatalineFragments | typeof DatastarDatalineSignals;
export type DatastarEventOptionsUnion = MergeFragmentsOptions | FragmentOptions | MergeSignalsOptions | DatastarEventOptions | ExecuteScriptOptions;
export type DatastarEvent = MergeFragmentsEvent | RemoveFragmentsEvent | MergeSignalsEvent | RemoveSignalsEvent | ExecuteScriptEvent;
