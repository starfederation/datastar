import { DATASTAR } from './consts'
export type PluginType = 'attribute' | 'watcher' | 'action'
export type Requirement = 'allowed' | 'must' | 'denied' | 'exclusive'

// export type ReactiveNode = Signal | Computed | Effect
// export type Subscriber = Computed | Effect
export type OnRemovalFn = () => void

export type DatastarPlugin = AttributePlugin | WatcherPlugin | ActionPlugin

export const DATASTAR_SIGNAL_PATCH_EVENT = `${DATASTAR}-signal-patch`
export type JSONPatch = Record<string, any> & { length?: never }
export type Paths = [string, any][]

export interface CustomEventMap {
  [DATASTAR_SIGNAL_PATCH_EVENT]: CustomEvent<JSONPatch>
}
export type WatcherFn<K extends keyof CustomEventMap> = (
  this: Document,
  ev: CustomEventMap[K],
) => void
declare global {
  interface Document {
    dispatchEvent<K extends keyof CustomEventMap>(ev: CustomEventMap[K]): void
    addEventListener<K extends keyof CustomEventMap>(
      type: K,
      listener: WatcherFn<K>,
    ): void
    removeEventListener<K extends keyof CustomEventMap>(
      type: K,
      listener: WatcherFn<K>,
    ): void
  }
}

// A plugin accessible via a `data-${name}` attribute on an element
export type AttributePlugin = {
  type: 'attribute'
  name: string // The name of the plugin
  onGlobalInit?: (ctx: InitContext) => void // Called once on registration of the plugin
  onLoad: (ctx: RuntimeContext) => OnRemovalFn | void // Return a function to be called on removal
  keyReq?: Requirement // The rules for the key requirements
  valReq?: Requirement // The rules for the value requirements
  returnsValue?: boolean // If the expression returns a value
  shouldEvaluate?: boolean // If the value should be evaluated
  argNames?: string[] // argument names for the reactive expression
}

// A plugin that runs on the global scope of the Datastar instance
export type WatcherPlugin = {
  type: 'watcher'
  name: string // The name of the plugin
  onGlobalInit?: (ctx: InitContext) => void
}

export type ActionPlugins = Record<string, ActionPlugin>
export type ActionMethod = (ctx: RuntimeContext, ...args: any[]) => any

export type ActionPlugin = {
  type: 'action'
  name: string // The name of the plugin
  fn: ActionMethod
}

export type GlobalInitializer = (ctx: InitContext) => void

export type MergePatchArgs = {
  ifMissing?: boolean
}

export type InitContext = {
  plugin: DatastarPlugin // The plugin instance
  actions: Readonly<ActionPlugins> // All registered actions
  root: Record<string, any> // global signals and computed signals
  filtered: (opts?: SignalFilterOptions, obj?: JSONPatch) => Record<string, any>
  signal<T>(initialValue?: T | undefined): Signal<T> // creates a signal
  computed<T>(getter: (previousValue?: T) => T): Computed<T> // creates a computed signal
  effect(fn: (...args: any[]) => void): OnRemovalFn // creates an effect
  mergePatch: (patch: JSONPatch, args?: MergePatchArgs) => void
  mergePaths: (paths: Paths, args?: MergePatchArgs) => void
  peek: <T>(fn: () => T) => T // returns the current state of the signal without subscribing
  getPath: <T = any>(path: string) => T | undefined // get a value from the root
  startBatch: () => void // starts a signal batch
  endBatch: () => void // ends a signal batch
  initErr: (reason: string, metadata?: object) => Error
}

export type HTMLOrSVG = Element & (HTMLElement | SVGElement)
export type Modifiers = Map<string, Set<string>> // mod name -> tags
export type ReactiveExpressionFn = <T>(...argsThenDeps: any[]) => T // a reactive expression

export type RuntimeContext = InitContext & {
  el: HTMLOrSVG // The element the attribute is on
  rawKey: Readonly<string> // no parsing data-* key
  key: Readonly<string> // data-* key without the prefix or tags
  value: Readonly<string> // value of data-* attribute
  mods: Modifiers // the modifiers and their tags
  rx: ReactiveExpressionFn // function to generate a reactive expression
  fnContent?: string // the content of the function
  evt?: Event // the event that triggered the plugin
  runtimeErr: (reason: string, metadata?: object) => Error
}

export type RuntimeExpressionFunction = (
  ctx: RuntimeContext,
  ...args: any[]
) => any

export type EventCallbackHandler = (...args: any[]) => void

export type SignalFilter = RegExp
export type SignalFilterOptions = {
  include?: RegExp | string
  exclude?: RegExp | string
}

export type Signal<T = any> = {
  (): T
  (value: T): boolean
}

export type Computed<T = any> = () => T

export type Effect = () => void
