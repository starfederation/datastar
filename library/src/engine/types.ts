import type { EffectFn, Signal } from '../vendored/preact-core'
import type { SignalsRoot } from './nestedSignals'

export type OnRemovalFn = () => void

export enum PluginType {
  Macro = 0,
  Attribute = 1,
  Watcher = 2,
  Action = 3,
}

export interface DatastarPlugin {
  type: PluginType // The type of plugin
  name: string // The name of the plugin
}

export interface MacroPlugin extends DatastarPlugin {
  type: PluginType.Macro
  fn: (ctx: RuntimeContext, original: string) => string
}

export enum Requirement {
  Allowed = 0,
  Must = 1,
  Denied = 2,
  Exclusive = 3,
}

// A plugin accesible via a `data-${name}` attribute on an element
export interface AttributePlugin extends DatastarPlugin {
  type: PluginType.Attribute
  onGlobalInit?: (ctx: InitContext) => void // Called once on registration of the plugin
  onLoad: (ctx: RuntimeContext) => OnRemovalFn | void // Return a function to be called on removal
  mods?: Set<string> // If not provided, all modifiers are allowed
  keyReq?: Requirement // The rules for the key requirements
  valReq?: Requirement // The rules for the value requirements
  removeOnLoad?: boolean // If true, the attribute is removed after onLoad (useful for plugins you don’t want reapplied)
  macros?: {
    pre?: MacroPlugin[]
    post?: MacroPlugin[]
  }
  argNames?: string[] // argument names for the reactive expression
}

// A plugin that runs on the global scope of the DastaStar instance
export interface WatcherPlugin extends DatastarPlugin {
  type: PluginType.Watcher
  onGlobalInit?: (ctx: InitContext) => void
}

export type ActionPlugins = Record<string, ActionPlugin>
export type ActionMethod = (ctx: RuntimeContext, ...args: any[]) => any

export interface ActionPlugin extends DatastarPlugin {
  type: PluginType.Action
  fn: ActionMethod
}

export type GlobalInitializer = (ctx: InitContext) => void
export type RemovalEntry = { id: string; set: Set<OnRemovalFn> }

export type InitContext = {
  signals: SignalsRoot
  effect: (fn: EffectFn) => OnRemovalFn
  actions: Readonly<ActionPlugins>
  apply: (target: Element) => void
  cleanup: (el: Element) => void
}

export type HTMLorSVGElement = Element & (HTMLElement | SVGElement)
export type Modifiers = Map<string, Set<string>> // mod name -> tags

export type RuntimeContext = InitContext & {
  el: HTMLorSVGElement // The element the attribute is on
  rawKey: Readonly<string> // no parsing data-* key
  rawValue: Readonly<string> // no parsing data-* value
  value: Readonly<string> // what the user wrote after any macros run
  key: Readonly<string> // data-* key without the prefix or tags
  mods: Modifiers // the tags and their arguments
  genRX: () => <T>(...args: any[]) => T // a reactive expression
}

export type NestedValues = { [key: string]: NestedValues | any }
export type NestedSignal = {
  [key: string]: NestedSignal | Signal<any>
}

export type RuntimeExpressionFunction = (
  ctx: RuntimeContext,
  ...args: any[]
) => any
