import type { EffectFn, Signal } from '../vendored/preact-core'
import { DATASTAR } from './consts'
import type { SignalsRoot } from './signals'

export type OnRemovalFn = () => void

export enum PluginType {
  Attribute = 1,
  Watcher = 2,
  Action = 3,
}

export interface DatastarPlugin {
  type: PluginType // The type of plugin
  name: string // The name of the plugin
}

export enum Requirement {
  Allowed = 0,
  Must = 1,
  Denied = 2,
  Exclusive = 3,
}

export interface DatastarSignalEvent {
  added: Array<string>
  removed: Array<string>
  updated: Array<string>
}
export const DATASTAR_SIGNAL_EVENT = `${DATASTAR}-signals`
export interface CustomEventMap {
  [DATASTAR_SIGNAL_EVENT]: CustomEvent<DatastarSignalEvent>
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

// A plugin accesible via a `data-${name}` attribute on an element
export interface AttributePlugin extends DatastarPlugin {
  type: PluginType.Attribute
  onGlobalInit?: (ctx: InitContext) => void // Called once on registration of the plugin
  onLoad: (ctx: RuntimeContext) => OnRemovalFn | void // Return a function to be called on removal
  removeOnLoad?: (rawKey: string) => boolean // Return whether the attribute key should be removed after onLoad (useful for plugin attributes you don’t want reapplied)
  mods?: Set<string> // If not provided, all modifiers are allowed
  keyReq?: Requirement // The rules for the key requirements
  valReq?: Requirement // The rules for the value requirements
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

export type InitContext = {
  plugin: DatastarPlugin
  signals: SignalsRoot
  effect: (fn: EffectFn) => OnRemovalFn
  actions: Readonly<ActionPlugins>
  removals: Map<string, Map<number, OnRemovalFn>>
  applyToElement: (el: HTMLorSVGElement) => void
}

export type HTMLorSVGElement = Element & (HTMLElement | SVGElement)
export type Modifiers = Map<string, Set<string>> // mod name -> tags

export type RuntimeContext = InitContext & {
  plugin: DatastarPlugin // The name of the plugin
  el: HTMLorSVGElement // The element the attribute is on
  rawKey: Readonly<string> // no parsing data-* key
  key: Readonly<string> // data-* key without the prefix or tags
  value: Readonly<string> // value of data-* attribute
  mods: Modifiers // the tags and their arguments
  genRX: () => <T>(...args: any[]) => T // a reactive expression
  fnContent?: string // the content of the function
}

export type NestedValues = { [key: string]: NestedValues | any }
export type NestedSignal = {
  [key: string]: NestedSignal | Signal<any>
}

export type RuntimeExpressionFunction = (
  ctx: RuntimeContext,
  ...args: any[]
) => any
