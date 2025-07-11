import {
  DatastarDatalineElements,
  DatastarDatalinePatchMode,
  DatastarDatalineOnlyIfMissing,
  DatastarDatalineSelector,
  DatastarDatalineSignals,
  DatastarDatalineUseViewTransition,
  DefaultElementPatchMode,
  DefaultElementsUseViewTransitions,
  DefaultpatchSignalsOnlyIfMissing,
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
  responseInit: Record<string, unknown>;
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

export interface patchElementsEvent {
  event: "datastar-patch-elements";
  options: PatchElementsOptions;
  [DatastarDatalineElements]: string;
}

export interface PatchSignalsOptions extends DatastarEventOptions {
  [DatastarDatalineOnlyIfMissing]?: boolean;
}

export interface patchSignalsEvent {
  event: "datastar-patch-signals";
  options: PatchSignalsOptions;
  [DatastarDatalineSignals]: Record<string, Jsonifiable>;
}

export const sseHeaders = {
  "Cache-Control": "no-cache",
  "Connection": "keep-alive",
  "Content-Type": "text/event-stream",
} as const;

export type MultilineDatalinePrefix =
  | typeof DatastarDatalineElements
  | typeof DatastarDatalineSignals;

export type DatastarEventOptionsUnion =
  | PatchElementsOptions
  | ElementOptions
  | PatchSignalsOptions
  | DatastarEventOptions;

export type DatastarEvent =
  | patchElementsEvent
  | patchSignalsEvent;

export const DefaultMapping = {
  [DatastarDatalinePatchMode]: DefaultElementPatchMode,
  [DatastarDatalineUseViewTransition]: DefaultElementsUseViewTransitions,
  [DatastarDatalineOnlyIfMissing]: DefaultpatchSignalsOnlyIfMissing,
} as const;
