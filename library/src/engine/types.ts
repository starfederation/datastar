import { DATASTAR_SIGNAL_PATCH_EVENT } from '@engine/consts'

export type JSONPatch = Record<string, any> & { length?: never }
export type Paths = [string, any][]

export type DatastarFetchEvent = {
  type: string
  el: HTMLOrSVG
  argsRaw: Record<string, string>
}

export type CustomEventMap = {
  [DATASTAR_SIGNAL_PATCH_EVENT]: CustomEvent<JSONPatch>
}
export type WatcherFn<K extends keyof CustomEventMap> = (
  this: Document,
  ev: CustomEventMap[K],
) => void

export type ErrorFn = (name: string, ctx?: Record<string, any>) => void

export type ActionContext = {
  el: HTMLOrSVG
  evt?: Event
  error: ErrorFn
  cleanups: Map<string, () => void>
}

export type RequirementType = 'allowed' | 'must' | 'denied' | 'exclusive'

export type Requirement =
  | RequirementType
  | {
      key: Exclude<RequirementType, 'exclusive'>
      value?: Exclude<RequirementType, 'exclusive'>
    }
  | {
      key?: Exclude<RequirementType, 'exclusive'>
      value: Exclude<RequirementType, 'exclusive'>
    }

type Rx<B extends boolean> = (...args: any[]) => B extends true ? unknown : void

type ReqField<R, K extends 'key' | 'value', Return> = R extends
  | 'must'
  | { [P in K]: 'must' }
  ? Return
  : R extends 'denied' | { [P in K]: 'denied' }
    ? undefined
    : R extends
          | 'allowed'
          | { [P in K]: 'allowed' }
          | (K extends keyof R ? never : R)
      ? Return | undefined
      : never

type ReqFields<R extends Requirement, B extends boolean> = R extends 'exclusive'
  ?
      | { key: string; value: undefined; rx: undefined }
      | { key: undefined; value: string; rx: Rx<B> }
  : {
      key: ReqField<R, 'key', string>
      value: ReqField<R, 'value', string>
      rx: ReqField<R, 'value', Rx<B>>
    }

export type AttributeContext<
  R extends Requirement = Requirement,
  RxReturn extends boolean = boolean,
> = {
  el: HTMLOrSVG
  mods: Modifiers
  rawKey: string
  evt?: Event
  error: ErrorFn
  loadedPluginNames: {
    actions: Readonly<Set<string>>
    attributes: Readonly<Set<string>>
  }
} & ReqFields<R, RxReturn>

export type AttributePlugin<
  R extends Requirement = Requirement,
  RxReturn extends boolean = boolean,
> = {
  name: string
  apply: (ctx: AttributeContext<R, RxReturn>) => void | (() => void)
  requirement?: R
  returnsValue?: RxReturn
  argNames?: string[]
}

export type WatcherContext = {
  error: ErrorFn
}

// A plugin that runs on the global scope of the Datastar instance
export type WatcherPlugin = {
  name: string
  apply: (ctx: WatcherContext, args: Record<string, string | undefined>) => void
}

export type ActionPlugins = Record<string, ActionPlugin>

export type ActionPlugin<T = any> = {
  name: string // The name of the plugin
  apply: (ctx: ActionContext, ...args: any[]) => T
}

export type MergePatchArgs = {
  ifMissing?: boolean
}

export type HTMLOrSVG = HTMLElement | SVGElement | MathMLElement
export type Modifiers = Map<string, Set<string>> // mod name -> tags

export type EventCallbackHandler = (...args: any[]) => void

export type SignalFilter = RegExp
export type SignalFilterOptions = {
  include?: RegExp | string
  exclude?: RegExp | string
}

export type Signal<T> = {
  (): T
  (value: T): boolean
}

export type Computed<T> = () => T

export type Effect = () => void
