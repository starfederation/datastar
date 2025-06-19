import {
  DatastarDatalineAttributes,
  DatastarDatalineAutoRemove,
  DatastarDatalineElements,
  DatastarDatalinePatchMode,
  DatastarDatalineOnlyIfMissing,
  DatastarDatalinePaths,
  DatastarDatalineScript,
  DatastarDatalineSelector,
  DatastarDatalineSignals,
  DatastarDatalineUseViewTransition,
  DefaultExecuteScriptAttributes,
  DefaultExecuteScriptAutoRemove,
  DefaultElementPatchMode,
  DefaultElementsUseViewTransitions,
  DefaultPatchSignalsOnlyIfMissing,
  EventTypes,
  ElementPatchModes,
} from "./consts.ts";

// Simple Jsonifiable type definition to replace npm:type-fest dependency
export type Jsonifiable = 
  | string 
  | number 
  | boolean 
  | null 
  | undefined
  | Jsonifiable[] 
  | { [key: string]: Jsonifiable };

export type ElementPatchMode = typeof ElementPatchModes[number];
export type EventType = typeof EventTypes[number];

export type StreamOptions = Partial<{
  onError: (error: unknown) => Promise<void> | void;
  onAbort: (reason?: string) => Promise<void> | void;
  responseInit: ResponseInit;
  keepalive: boolean;
}>

export interface DatastarEventOptions {
  eventId?: string;
  retryDuration?: number;
}

export interface ElementOptions extends DatastarEventOptions {
  [DatastarDatalineUseViewTransition]?: boolean;
}

export interface PatchElementsOptions extends ElementOptions {
  [DatastarDatalinePatchMode]?: ElementPatchMode;
  [DatastarDatalineSelector]?: string;
}

export interface PatchElementsEvent {
  event: "datastar-patch-elements";
  options: PatchElementsOptions;
  [DatastarDatalineElements]: string;
}

export interface PatchSignalsOptions extends DatastarEventOptions {
  [DatastarDatalineOnlyIfMissing]?: boolean;
}

export interface PatchSignalsEvent {
  event: "datastar-patch-signals";
  options: PatchSignalsOptions;
  [DatastarDatalineSignals]: Record<string, Jsonifiable>;
}

type ScriptAttributes = {
  type?: "module" | "importmap" | "speculationrules" | "text/javascript";
  refererpolicy:
    | "no-referrer"
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
};

export interface ExecuteScriptOptions extends DatastarEventOptions {
  [DatastarDatalineAutoRemove]?: boolean;
  [DatastarDatalineAttributes]?: ScriptAttributes | string[];
}

export interface ExecuteScriptEvent {
  event: "datastar-patch-elements";
  options: ExecuteScriptOptions;
  [DatastarDatalineScript]: string;
}

export const sseHeaders = {
  "Cache-Control": "no-cache",
  "Connection": "keep-alive",
  "Content-Type": "text/event-stream",
} as const;

export type MultilineDatalinePrefix =
  | typeof DatastarDatalineScript
  | typeof DatastarDatalineElements
  | typeof DatastarDatalineSignals;

export type DatastarEventOptionsUnion =
  | PatchElementsOptions
  | ElementOptions
  | PatchSignalsOptions
  | DatastarEventOptions
  | ExecuteScriptOptions;

export type DatastarEvent =
  | PatchElementsEvent
  | PatchSignalsEvent
  | ExecuteScriptEvent;

const defaultScriptAttributes: Record<string, string> = {};
const scriptParts = DefaultExecuteScriptAttributes.split(" ");
if (scriptParts.length >= 2 && scriptParts[0] && scriptParts[1]) {
  defaultScriptAttributes[scriptParts[0]] = scriptParts[1];
}

export const DefaultMapping = {
  [DatastarDatalinePatchMode]: DefaultElementPatchMode,
  [DatastarDatalineUseViewTransition]: DefaultElementsUseViewTransitions,
  [DatastarDatalineOnlyIfMissing]: DefaultPatchSignalsOnlyIfMissing,
  [DatastarDatalineAttributes]: defaultScriptAttributes,
  [DatastarDatalineAutoRemove]: DefaultExecuteScriptAutoRemove,
} as const;
