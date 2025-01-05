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
} from "./consts";

export type FragmentMergeMode = typeof FragmentMergeModes[number];
export type EventType = typeof EventTypes[number];

export interface DatastarEventOptions {
    eventId?: string;
    retryDuration?: number;
};

export interface FragmentOptions extends DatastarEventOptions {
    [DatastarDatalineUseViewTransition]?: boolean;
    [DatastarDatalineSettleDuration]?: number;
}

export interface MergeFragmentsOptions extends FragmentOptions {
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
    [DatastarDatalineSignals]: Record<string, any>;
};

export interface RemoveSignalsEvent {
    event: "datastar-remove-signals";
    options: DatastarEventOptions;
    [DatastarDatalinePaths]: string[];
}
type ScriptAttributes = {
    type?: "module" | "importmap" | "speculationrules" | "text/javascript";
    refererpolicy: "no-referrer"
        | "no-referrer-when-downgrade"
        | "origin"
        | "origin-when-cross-origin"
        | "same-origin"
        | "strict-origin"
        | "strict-origin-when-cross-origin"
        | "unsafe-url";
    nonce?: string;
    nomodule?: boolean;
    integrity?: string;
    fetchpriority?: "high" | "low" | "auto";
    crossorigin?: "anonymous" | "use-credentials";
    blocking?: boolean;
    attributionsrc?: boolean | string;
    src?: string;
} & {
    src: string;
    defer: true;
} & {
    src: string;
    async: true;
}

export interface ExecuteScriptOptions extends DatastarEventOptions {
    [DatastarDatalineAutoRemove]?: boolean;
    [DatastarDatalineAttributes]?: ScriptAttributes;
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
