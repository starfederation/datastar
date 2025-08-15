import { isHTMLOrSVG } from '../utils/dom'
import { isPojo, pathToObj } from '../utils/paths'
import { camel, snake } from '../utils/text'
import { DATASTAR, DSP, DSS } from './consts'
import { initErr, runtimeErr } from './errors'
import type {
  ActionPlugins,
  AttributePlugin,
  Computed,
  DatastarPlugin,
  Effect,
  HTMLOrSVG,
  InitContext,
  JSONPatch,
  MergePatchArgs,
  OnRemovalFn,
  Paths,
  RuntimeContext,
  RuntimeExpressionFunction,
  Signal,
  SignalFilterOptions,
} from './types'
import { DATASTAR_SIGNAL_PATCH_EVENT } from './types'

/**
 * Custom signals implementation based on Alien Signals
 */

interface ReactiveNode {
  deps_?: Link
  depsTail_?: Link
  subs_?: Link
  subsTail_?: Link
  flags_: ReactiveFlags
}

interface Link {
  dep_: ReactiveNode
  sub_: ReactiveNode
  prevSub_?: Link
  nextSub_?: Link
  prevDep_?: Link
  nextDep_?: Link
}

interface Stack<T> {
  value_: T
  prev_?: Stack<T>
}

enum ReactiveFlags {
  None = 0,
  Mutable = 1 << 0,
  Watching = 1 << 1,
  RecursedCheck = 1 << 2,
  Recursed = 1 << 3,
  Dirty = 1 << 4,
  Pending = 1 << 5,
}

enum EffectFlags {
  Queued = 1 << 6,
}

interface AlienEffect extends ReactiveNode {
  fn_(): void
}

interface AlienComputed<T = any> extends ReactiveNode {
  value_?: T
  getter(previousValue?: T): T
}

interface AlienSignal<T = any> extends ReactiveNode {
  previousValue: T
  value_: T
}

const currentPatch: Paths = []
const queuedEffects: (AlienEffect | undefined)[] = []
let batchDepth = 0
let notifyIndex = 0
let queuedEffectsLength = 0
let activeSub: ReactiveNode | undefined

const startBatch = (): void => {
  batchDepth++
}

const endBatch = (): void => {
  if (!--batchDepth) {
    flush()
    dispatch()
  }
}

const signal = <T>(initialValue?: T): Signal<T> => {
  return signalOper.bind(0, {
    previousValue: initialValue,
    value_: initialValue,
    flags_: 1 satisfies ReactiveFlags.Mutable,
  }) as Signal<T>
}

const computedSymbol = Symbol('computed')
const computed = <T>(getter: (previousValue?: T) => T): Computed<T> => {
  const c = computedOper.bind(0, {
    flags_: 17 as ReactiveFlags.Mutable | ReactiveFlags.Dirty,
    getter,
  }) as Computed<T>
  // @ts-ignore
  c[computedSymbol] = 1
  return c
}

const effect = (fn: () => void): Effect => {
  const e: AlienEffect = {
    fn_: fn,
    flags_: 2 satisfies ReactiveFlags.Watching,
  }
  if (activeSub) {
    link(e, activeSub)
  }
  const prev = setCurrentSub(e)
  startBatch()
  try {
    e.fn_()
  } finally {
    endBatch()
    setCurrentSub(prev)
  }
  return effectOper.bind(0, e)
}

const peek = <T>(fn: () => T): T => {
  const prev = setCurrentSub(undefined)
  try {
    return fn()
  } finally {
    setCurrentSub(prev)
  }
}

const flush = () => {
  while (notifyIndex < queuedEffectsLength) {
    const effect = queuedEffects[notifyIndex]!
    queuedEffects[notifyIndex++] = undefined
    run(effect, (effect.flags_ &= ~EffectFlags.Queued))
  }
  notifyIndex = 0
  queuedEffectsLength = 0
}

const update = (signal: AlienSignal | AlienComputed): boolean => {
  if ('getter' in signal) {
    return updateComputed(signal)
  }
  return updateSignal(signal, signal.value_)
}

const setCurrentSub = (sub?: ReactiveNode): ReactiveNode | undefined => {
  const prevSub = activeSub
  activeSub = sub
  return prevSub
}

const updateComputed = (c: AlienComputed): boolean => {
  const prevSub = setCurrentSub(c)
  startTracking(c)
  try {
    const oldValue = c.value_
    return oldValue !== (c.value_ = c.getter(oldValue))
  } finally {
    setCurrentSub(prevSub)
    endTracking(c)
  }
}

const updateSignal = (s: AlienSignal, value: any): boolean => {
  s.flags_ = 1 satisfies ReactiveFlags.Mutable
  return s.previousValue !== (s.previousValue = value)
}

const notify = (e: AlienEffect): void => {
  const flags = e.flags_
  if (!(flags & EffectFlags.Queued)) {
    e.flags_ = flags | EffectFlags.Queued
    const subs = e.subs_
    if (subs) {
      notify(subs.sub_ as AlienEffect)
    } else {
      queuedEffects[queuedEffectsLength++] = e
    }
  }
}

const run = (e: AlienEffect, flags: ReactiveFlags): void => {
  if (
    flags & (16 satisfies ReactiveFlags.Dirty) ||
    (flags & (32 satisfies ReactiveFlags.Pending) && checkDirty(e.deps_!, e))
  ) {
    const prev = setCurrentSub(e)
    startTracking(e)
    startBatch()
    try {
      e.fn_()
    } finally {
      endBatch()
      setCurrentSub(prev)
      endTracking(e)
    }
    return
  }
  if (flags & (32 satisfies ReactiveFlags.Pending)) {
    e.flags_ = flags & ~(32 satisfies ReactiveFlags.Pending)
  }
  let link = e.deps_
  while (link) {
    const dep = link.dep_
    const depFlags = dep.flags_
    if (depFlags & EffectFlags.Queued) {
      run(dep as AlienEffect, (dep.flags_ = depFlags & ~EffectFlags.Queued))
    }
    link = link.nextDep_
  }
}

const computedOper = <T>(c: AlienComputed<T>): T => {
  const flags = c.flags_
  if (
    flags & (16 satisfies ReactiveFlags.Dirty) ||
    (flags & (32 satisfies ReactiveFlags.Pending) && checkDirty(c.deps_!, c))
  ) {
    if (updateComputed(c)) {
      const subs = c.subs_
      if (subs) {
        shallowPropagate(subs)
      }
    }
  } else if (flags & (32 satisfies ReactiveFlags.Pending)) {
    c.flags_ = flags & ~(32 satisfies ReactiveFlags.Pending)
  }
  if (activeSub) {
    link(c, activeSub)
  }
  return c.value_!
}

const signalOper = <T>(s: AlienSignal<T>, ...value: [T]): T | boolean => {
  if (value.length) {
    const newValue = value[0]
    if (s.value_ !== (s.value_ = newValue)) {
      s.flags_ = 17 as ReactiveFlags.Mutable | ReactiveFlags.Dirty
      const subs = s.subs_
      if (subs) {
        propagate(subs)
        if (!batchDepth) {
          flush()
        }
      }
      return true
    }
    return false
  }
  const currentValue = s.value_
  if (s.flags_ & (16 satisfies ReactiveFlags.Dirty)) {
    if (updateSignal(s, currentValue)) {
      const subs_ = s.subs_
      if (subs_) {
        shallowPropagate(subs_)
      }
    }
  }
  if (activeSub) {
    link(s, activeSub)
  }
  return currentValue
}

const effectOper = (e: AlienEffect): void => {
  let dep = e.deps_
  while (dep) {
    dep = unlink(dep, e)
  }
  const sub = e.subs_
  if (sub) {
    unlink(sub)
  }
  e.flags_ = 0 satisfies ReactiveFlags.None
}

const link = (dep: ReactiveNode, sub: ReactiveNode): void => {
  const prevDep = sub.depsTail_
  if (prevDep && prevDep.dep_ === dep) {
    return
  }
  let nextDep: Link | undefined
  const recursedCheck = sub.flags_ & (4 satisfies ReactiveFlags.RecursedCheck)
  if (recursedCheck) {
    nextDep = prevDep ? prevDep.nextDep_ : sub.deps_
    if (nextDep && nextDep.dep_ === dep) {
      sub.depsTail_ = nextDep
      return
    }
  }
  const prevSub = dep.subsTail_
  if (
    prevSub &&
    prevSub.sub_ === sub &&
    (!recursedCheck || isValidLink(prevSub, sub))
  ) {
    return
  }
  const newLink =
    (sub.depsTail_ =
    dep.subsTail_ =
      {
        dep_: dep,
        sub_: sub,
        prevDep_: prevDep,
        nextDep_: nextDep,
        prevSub_: prevSub,
      })
  if (nextDep) {
    nextDep.prevDep_ = newLink
  }
  if (prevDep) {
    prevDep.nextDep_ = newLink
  } else {
    sub.deps_ = newLink
  }
  if (prevSub) {
    prevSub.nextSub_ = newLink
  } else {
    dep.subs_ = newLink
  }
}

const unlink = (link: Link, sub_ = link.sub_): Link | undefined => {
  const dep_ = link.dep_
  const prevDep_ = link.prevDep_
  const nextDep_ = link.nextDep_
  const nextSub_ = link.nextSub_
  const prevSub_ = link.prevSub_
  if (nextDep_) {
    nextDep_.prevDep_ = prevDep_
  } else {
    sub_.depsTail_ = prevDep_
  }
  if (prevDep_) {
    prevDep_.nextDep_ = nextDep_
  } else {
    sub_.deps_ = nextDep_
  }
  if (nextSub_) {
    nextSub_.prevSub_ = prevSub_
  } else {
    dep_.subsTail_ = prevSub_
  }
  if (prevSub_) {
    prevSub_.nextSub_ = nextSub_
  } else if (!(dep_.subs_ = nextSub_)) {
    if ('getter' in dep_) {
      let toRemove = dep_.deps_
      if (toRemove) {
        dep_.flags_ = 17 as ReactiveFlags.Mutable | ReactiveFlags.Dirty
        do {
          toRemove = unlink(toRemove, dep_)
        } while (toRemove)
      }
    } else if (!('previousValue' in dep_)) {
      effectOper(dep_ as AlienEffect)
    }
  }
  return nextDep_
}

const propagate = (link: Link): void => {
  let next = link.nextSub_
  let stack: Stack<Link | undefined> | undefined

  top: while (true) {
    const sub = link.sub_

    let flags = sub.flags_

    if (flags & (3 as ReactiveFlags.Mutable | ReactiveFlags.Watching)) {
      if (
        !(
          flags &
          (60 as
            | ReactiveFlags.RecursedCheck
            | ReactiveFlags.Recursed
            | ReactiveFlags.Dirty
            | ReactiveFlags.Pending)
        )
      ) {
        sub.flags_ = flags | (32 satisfies ReactiveFlags.Pending)
      } else if (
        !(flags & (12 as ReactiveFlags.RecursedCheck | ReactiveFlags.Recursed))
      ) {
        flags = 0 satisfies ReactiveFlags.None
      } else if (!(flags & (4 satisfies ReactiveFlags.RecursedCheck))) {
        sub.flags_ =
          (flags & ~(8 satisfies ReactiveFlags.Recursed)) |
          (32 satisfies ReactiveFlags.Pending)
      } else if (
        !(flags & (48 as ReactiveFlags.Dirty | ReactiveFlags.Pending)) &&
        isValidLink(link, sub)
      ) {
        sub.flags_ =
          flags | (40 as ReactiveFlags.Recursed | ReactiveFlags.Pending)
        flags &= 1 satisfies ReactiveFlags.Mutable
      } else {
        flags = 0 satisfies ReactiveFlags.None
      }

      if (flags & (2 satisfies ReactiveFlags.Watching)) {
        notify(sub as AlienEffect)
      }

      if (flags & (1 satisfies ReactiveFlags.Mutable)) {
        const subSubs = sub.subs_
        if (subSubs) {
          link = subSubs
          if (subSubs.nextSub_) {
            stack = { value_: next, prev_: stack }
            next = link.nextSub_
          }
          continue
        }
      }
    }

    if ((link = next!)) {
      next = link.nextSub_
      continue
    }

    while (stack) {
      link = stack.value_!
      stack = stack.prev_
      if (link) {
        next = link.nextSub_
        continue top
      }
    }

    break
  }
}

const startTracking = (sub: ReactiveNode): void => {
  sub.depsTail_ = undefined
  sub.flags_ =
    (sub.flags_ &
      ~(56 as
        | ReactiveFlags.Recursed
        | ReactiveFlags.Dirty
        | ReactiveFlags.Pending)) |
    (4 satisfies ReactiveFlags.RecursedCheck)
}

const endTracking = (sub: ReactiveNode): void => {
  const depsTail_ = sub.depsTail_
  let toRemove = depsTail_ ? depsTail_.nextDep_ : sub.deps_
  while (toRemove) {
    toRemove = unlink(toRemove, sub)
  }
  sub.flags_ &= ~(4 satisfies ReactiveFlags.RecursedCheck)
}

const checkDirty = (link: Link, sub: ReactiveNode): boolean => {
  let stack: Stack<Link> | undefined
  let checkDepth = 0

  top: while (true) {
    const dep = link.dep_
    const depFlags = dep.flags_

    let dirty = false

    if (sub.flags_ & (16 satisfies ReactiveFlags.Dirty)) {
      dirty = true
    } else if (
      (depFlags & (17 as ReactiveFlags.Mutable | ReactiveFlags.Dirty)) ===
      (17 as ReactiveFlags.Mutable | ReactiveFlags.Dirty)
    ) {
      if (update(dep as AlienSignal | AlienComputed)) {
        const subs = dep.subs_!
        if (subs.nextSub_) {
          shallowPropagate(subs)
        }
        dirty = true
      }
    } else if (
      (depFlags & (33 as ReactiveFlags.Mutable | ReactiveFlags.Pending)) ===
      (33 as ReactiveFlags.Mutable | ReactiveFlags.Pending)
    ) {
      if (link.nextSub_ || link.prevSub_) {
        stack = { value_: link, prev_: stack }
      }
      link = dep.deps_!
      sub = dep
      ++checkDepth
      continue
    }

    if (!dirty && link.nextDep_) {
      link = link.nextDep_
      continue
    }

    while (checkDepth) {
      --checkDepth
      const firstSub = sub.subs_!
      const hasMultipleSubs = firstSub.nextSub_
      if (hasMultipleSubs) {
        link = stack!.value_
        stack = stack!.prev_
      } else {
        link = firstSub
      }
      if (dirty) {
        if (update(sub as AlienSignal | AlienComputed)) {
          if (hasMultipleSubs) {
            shallowPropagate(firstSub)
          }
          sub = link.sub_
          continue
        }
      } else {
        sub.flags_ &= ~(32 satisfies ReactiveFlags.Pending)
      }
      sub = link.sub_
      if (link.nextDep_) {
        link = link.nextDep_
        continue top
      }
      dirty = false
    }

    return dirty
  }
}

const shallowPropagate = (link: Link): void => {
  do {
    const sub = link.sub_
    const nextSub = link.nextSub_
    const subFlags = sub.flags_
    if (
      (subFlags & (48 as ReactiveFlags.Pending | ReactiveFlags.Dirty)) ===
      (32 satisfies ReactiveFlags.Pending)
    ) {
      sub.flags_ = subFlags | (16 satisfies ReactiveFlags.Dirty)
      if (subFlags & (2 satisfies ReactiveFlags.Watching)) {
        notify(sub as AlienEffect)
      }
    }
    link = nextSub!
  } while (link)
}

const isValidLink = (checkLink: Link, sub: ReactiveNode): boolean => {
  const depsTail = sub.depsTail_
  if (depsTail) {
    let link = sub.deps_!
    do {
      if (link === checkLink) {
        return true
      }
      if (link === depsTail) {
        break
      }
      link = link.nextDep_!
    } while (link)
  }
  return false
}

const getPath = <T = any>(path: string): T | undefined => {
  let result = root
  const split = path.split('.')
  for (const path of split) {
    if (result == null || !Object.hasOwn(result, path)) {
      return
    }
    result = result[path]
  }
  return result as T
}

export const DELETE = Symbol('delete')
const deep = (value: any, prefix = ''): any => {
  const isArr = Array.isArray(value)
  if (isArr || isPojo(value)) {
    const deepObj = (isArr ? [] : {}) as Record<string, Signal>
    for (const key in value) {
      deepObj[key] = signal(
        deep((value as Record<string, Signal>)[key], `${prefix + key}.`),
      )
    }
    const keys = signal(0)
    return new Proxy(deepObj, {
      get(_, prop: string) {
        if (!(prop === 'toJSON' && !Object.hasOwn(deepObj, prop))) {
          if (isArr && prop in Array.prototype) {
            keys()
            return deepObj[prop]
          } else {
            if (typeof prop === 'symbol') {
              return deepObj[prop]
            }
            if (!Object.hasOwn(deepObj, prop) || deepObj[prop]() == null) {
              deepObj[prop] = signal('')
              dispatch(prefix + prop, '')
              keys(keys() + 1)
            }
            return deepObj[prop]()
          }
        }
      },
      set(_, prop: string, newValue) {
        const path = prefix + prop
        if (newValue === DELETE) {
          if (Object.hasOwn(deepObj, prop)) {
            delete deepObj[prop]
            dispatch(path, DELETE)
            keys(keys() + 1)
          }
        } else {
          if (isArr && prop === 'length') {
            const diff = (deepObj[prop] as unknown as number) - newValue
            deepObj[prop] = newValue
            if (diff > 0) {
              const patch: Record<string, any> = {}
              for (let i = newValue; i < deepObj[prop]; i++) {
                patch[i] = null
              }
              dispatch(prefix.slice(0, -1), patch)
              keys(keys() + 1)
            }
          } else {
            if (Object.hasOwn(deepObj, prop)) {
              if (newValue == null) {
                if (deepObj[prop](null)) {
                  dispatch(path, null)
                }
              } else {
                if (Object.hasOwn(newValue, computedSymbol)) {
                  deepObj[prop] = newValue
                  dispatch(path, '')
                } else {
                  if (deepObj[prop](deep(newValue, `${path}.`))) {
                    dispatch(path, newValue)
                  }
                }
              }
            } else {
              if (newValue != null) {
                if (Object.hasOwn(newValue, computedSymbol)) {
                  deepObj[prop] = newValue
                  dispatch(path, '')
                } else {
                  deepObj[prop] = signal(deep(newValue, `${path}.`))
                  dispatch(path, newValue)
                }
                keys(keys() + 1)
              }
            }
          }
        }

        return true
      },
      deleteProperty(_, prop: string) {
        if (Object.hasOwn(deepObj, prop)) {
          if (deepObj[prop](null)) {
            dispatch(prefix + prop, null)
          }
        }

        return true
      },
      ownKeys() {
        keys()
        return Reflect.ownKeys(deepObj)
      },
      has(_, prop) {
        keys()
        return prop in deepObj
      },
    })
  }
  return value
}

const dispatch = (path?: string, value?: any) => {
  if (path !== undefined && value !== undefined) {
    currentPatch.push([path, value])
  }
  if (!batchDepth && currentPatch.length) {
    const detail = pathToObj(currentPatch)
    currentPatch.length = 0
    document.dispatchEvent(
      new CustomEvent<JSONPatch>(DATASTAR_SIGNAL_PATCH_EVENT, {
        detail,
      }),
    )
  }
}

const mergePatch = (
  patch: JSONPatch,
  { ifMissing }: MergePatchArgs = {},
): void => {
  startBatch()
  for (const key in patch) {
    if (patch[key] == null) {
      if (!ifMissing) {
        delete root[key]
      }
    } else {
      mergeInner(patch[key], key, root, '', ifMissing)
    }
  }
  endBatch()
}

const mergePaths = (paths: Paths, options: MergePatchArgs = {}): void =>
  mergePatch(pathToObj(paths), options)

const mergeInner = (
  patch: any,
  target: string,
  targetParent: Record<string, any>,
  prefix: string,
  ifMissing: boolean | undefined,
): void => {
  if (isPojo(patch)) {
    if (
      !(
        Object.hasOwn(targetParent, target) &&
        (isPojo(targetParent[target]) || Array.isArray(targetParent[target]))
      )
    ) {
      targetParent[target] = {}
    }

    for (const key in patch) {
      if (patch[key] == null) {
        if (!ifMissing) {
          delete targetParent[target][key]
        }
      } else {
        mergeInner(
          patch[key],
          key,
          targetParent[target],
          `${prefix + target}.`,
          ifMissing,
        )
      }
    }
  } else if (!(ifMissing && Object.hasOwn(targetParent, target))) {
    targetParent[target] = patch
  }
}

function filtered(
  { include = /.*/, exclude = /(?!)/ }: SignalFilterOptions = {},
  obj: JSONPatch = root,
): Record<string, any> {
  // We need to find all valid signal paths in the object
  const paths: Paths = []
  const stack: [any, string][] = [[obj, '']]

  while (stack.length) {
    const [node, prefix] = stack.pop()!

    for (const key in node) {
      const path = prefix + key
      if (isPojo(node[key])) {
        stack.push([node[key], `${path}.`])
      } else if (
        toRegExp(include).test(path) &&
        !toRegExp(exclude).test(path)
      ) {
        paths.push([path, getPath(path)])
      }
    }
  }

  return pathToObj(paths)
}

function toRegExp(val: string | RegExp): RegExp {
  if (typeof val === 'string') {
    return RegExp(val.replace(/^\/|\/$/g, ''))
  }

  return val
}

const root: Record<string, any> = deep({})

/**
 * Turn data-* attributes into reactive expressions
 * This is the core of the Datastar
 */

const actions: ActionPlugins = {}
const plugins: AttributePlugin[] = []
let pluginRegexs: RegExp[] = []

// Map of cleanup functions by element, keyed by a dataset key-value hash
const removals = new Map<HTMLOrSVG, Map<string, OnRemovalFn>>()

let mutationObserver: MutationObserver | null = null

let alias = ''
export function setAlias(value: string) {
  alias = value
}
export function aliasify(name: string) {
  return alias ? `data-${alias}-${name}` : `data-${name}`
}

export function load(...pluginsToLoad: DatastarPlugin[]) {
  for (const plugin of pluginsToLoad) {
    const ctx: InitContext = {
      plugin,
      actions,
      root,
      filtered,
      signal,
      computed,
      effect,
      mergePatch,
      mergePaths,
      peek,
      getPath,
      startBatch,
      endBatch,
      initErr: 0 as any,
    }
    ctx.initErr = initErr.bind(0, ctx)

    if (plugin.type === 'action') {
      actions[plugin.name] = plugin
    } else if (plugin.type === 'attribute') {
      plugins.push(plugin)
      plugin.onGlobalInit?.(ctx)
    } else if (plugin.type === 'watcher') {
      plugin.onGlobalInit?.(ctx)
    } else {
      throw ctx.initErr('InvalidPluginType')
    }
  }

  // Sort attribute plugins by descending length then alphabetically
  plugins.sort((a, b) => {
    const lenDiff = b.name.length - a.name.length
    if (lenDiff !== 0) return lenDiff
    return a.name.localeCompare(b.name)
  })

  pluginRegexs = plugins.map((plugin) => RegExp(`^${plugin.name}([A-Z]|_|$)`))
}

function applyEls(els: Iterable<HTMLOrSVG>): void {
  const ignore = `[${aliasify('ignore')}]`
  for (const el of els) {
    if (!el.closest(ignore)) {
      for (const key in el.dataset) {
        applyAttributePlugin(el, key, el.dataset[key]!)
      }
    }
  }
}

function cleanupEls(els: Iterable<HTMLOrSVG>): void {
  for (const el of els) {
    const cleanups = removals.get(el)
    // If removals has el, delete it and run all cleanup functions
    if (removals.delete(el)) {
      for (const cleanup of cleanups!.values()) {
        cleanup()
      }
      cleanups!.clear()
    }
  }
}

// Apply all plugins to the entire DOM or a provided element
export function apply(root: HTMLOrSVG = document.body) {
  // Delay applying plugins to give custom plugins a chance to load
  queueMicrotask(() => {
    applyEls([root])
    applyEls(root.querySelectorAll<HTMLOrSVG>('*'))

    // Monitor the entire document body or a provided element for changes
    // https://developer.mozilla.org/en-US/docs/Web/API/MutationObserver/observe
    if (!mutationObserver) {
      mutationObserver = new MutationObserver(observe)
      mutationObserver.observe(root, {
        subtree: true,
        childList: true,
        attributes: true,
      })
    }
  })
}

function applyAttributePlugin(
  el: HTMLOrSVG,
  attrKey: string,
  value: string,
): void {
  if (attrKey.startsWith(alias)) {
    const rawKey = camel(alias ? attrKey.slice(alias.length) : attrKey)
    const plugin = plugins.find((_, i) => pluginRegexs[i].test(rawKey))
    if (plugin) {
      // Extract the key and modifiers
      let [key, ...rawModifiers] = rawKey.slice(plugin.name.length).split(/__+/)

      const hasKey = !!key
      if (hasKey) {
        key = camel(key)
      }
      const hasValue = !!value

      // Create the runtime context
      const ctx: RuntimeContext = {
        plugin,
        actions,
        root,
        filtered,
        signal,
        computed,
        effect,
        mergePatch,
        mergePaths,
        peek,
        getPath,
        startBatch,
        endBatch,
        initErr: 0 as any,
        el,
        rawKey,
        key,
        value,
        mods: new Map(),
        runtimeErr: 0 as any,
        rx: 0 as any,
      }
      ctx.initErr = initErr.bind(0, ctx)
      ctx.runtimeErr = runtimeErr.bind(0, ctx)
      if (
        plugin.shouldEvaluate === undefined ||
        plugin.shouldEvaluate === true
      ) {
        ctx.rx = generateReactiveExpression(ctx)
      }

      // Check the requirements
      const keyReq = plugin.keyReq || 'allowed'
      if (hasKey) {
        if (keyReq === 'denied') {
          throw ctx.runtimeErr(`${plugin.name}KeyNotAllowed`)
        }
      } else if (keyReq === 'must') {
        throw ctx.runtimeErr(`${plugin.name}KeyRequired`)
      }

      const valReq = plugin.valReq || 'allowed'
      if (hasValue) {
        if (valReq === 'denied') {
          throw ctx.runtimeErr(`${plugin.name}ValueNotAllowed`)
        }
      } else if (valReq === 'must') {
        throw ctx.runtimeErr(`${plugin.name}ValueRequired`)
      }

      // Check for exclusive requirements
      if (keyReq === 'exclusive' || valReq === 'exclusive') {
        if (hasKey && hasValue) {
          throw ctx.runtimeErr(`${plugin.name}KeyAndValueProvided`)
        }
        if (!hasKey && !hasValue) {
          throw ctx.runtimeErr(`${plugin.name}KeyOrValueRequired`)
        }
      }

      for (const rawMod of rawModifiers) {
        const [label, ...mod] = rawMod.split('.')
        ctx.mods.set(camel(label), new Set(mod.map((t) => t.toLowerCase())))
      }

      const cleanup = plugin.onLoad(ctx)
      if (cleanup) {
        let cleanups = removals.get(el)
        if (cleanups) {
          cleanups.get(rawKey)?.()
        } else {
          cleanups = new Map()
          removals.set(el, cleanups)
        }
        cleanups.set(rawKey, cleanup)
      }
    }
  }
}

// Set up a mutation observer to run plugin removal and apply functions
function observe(mutations: MutationRecord[]) {
  const ignore = `[${aliasify('ignore')}]`

  for (const {
    target,
    type,
    attributeName,
    addedNodes,
    removedNodes,
  } of mutations) {
    if (type === 'childList') {
      for (const node of removedNodes) {
        if (isHTMLOrSVG(node)) {
          cleanupEls([node])
          cleanupEls(node.querySelectorAll<HTMLOrSVG>('*'))
        }
      }

      for (const node of addedNodes) {
        if (isHTMLOrSVG(node)) {
          applyEls([node])
          applyEls(node.querySelectorAll<HTMLOrSVG>('*'))
        }
      }
    } else if (type === 'attributes') {
      // If el has a parent with data-ignore, skip it
      if (isHTMLOrSVG(target) && !target.closest(ignore)) {
        const key = camel(attributeName!.slice(5))
        const value = target.getAttribute(attributeName!)
        if (value === null) {
          const cleanups = removals.get(target)
          if (cleanups) {
            cleanups.get(key)?.()
            cleanups.delete(key)
          }
        } else {
          applyAttributePlugin(target, key, value)
        }
      }
    }
  }
}

function generateReactiveExpression(
  ctx: RuntimeContext,
): RuntimeExpressionFunction {
  let expr = ''

  const attrPlugin = (ctx.plugin as AttributePlugin) || undefined

  // plugin is guaranteed to be an attribute plugin
  if (attrPlugin?.returnsValue) {
    // This regex allows Datastar expressions to support nested
    // regex and strings that contain ; without breaking.
    //
    // Each of these regex defines a block type we want to match
    // (importantly we ignore the content within these blocks):
    //
    // regex            \/(\\\/|[^\/])*\/
    // double quotes      "(\\"|[^\"])*"
    // single quotes      '(\\'|[^'])*'
    // ticks              `(\\`|[^`])*`
    // iife               \(\s*((function)\s*\(\s*\)|(\(\s*\))\s*=>)\s*(?:\{[\s\S]*?\}|[^;)\{]*)\s*\)\s*\(\s*\)
    //
    // The iife support is (intentionally) limited. It only supports
    // function and arrow syntax with no arguments, and no nested IIFEs.
    //
    // We also want to match the non delimiter part of statements
    // note we only support ; statement delimiters:
    //
    // [^;]
    //
    const statementRe =
      /(\/(\\\/|[^/])*\/|"(\\"|[^"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|\(\s*((function)\s*\(\s*\)|(\(\s*\))\s*=>)\s*(?:\{[\s\S]*?\}|[^;){]*)\s*\)\s*\(\s*\)|[^;])+/gm
    const statements = ctx.value.trim().match(statementRe)
    if (statements) {
      const lastIdx = statements.length - 1
      const last = statements[lastIdx].trim()
      if (!last.startsWith('return')) {
        statements[lastIdx] = `return (${last});`
      }
      expr = statements.join(';\n')
    }
  } else {
    expr = ctx.value.trim()
  }

  // Replace signal references with bracket notation
  // Examples:
  //   $count          → $['count']
  //   $count--        → $['count']--
  //   $count++        → $['count']++
  //   $count += 5     → $['count'] += 5
  //   $foo = 5        → $['foo'] = 5
  //   $foo.bar        → $['foo']['bar']
  //   $foo-bar        → $['foo-bar']
  //   $foo.bar-baz    → $['foo']['bar-baz']
  //   $foo-$bar       → $['foo']-$['bar']
  //   $arr[$index]    → $['arr'][$['index']]
  //   $['foo']        → $['foo']
  //   $foo[obj.bar]   → $['foo'][obj.bar]
  //   $foo['bar.baz'] → $['foo']['bar.baz']
  //   $1              → $['1']
  //   $123            → $['123']
  //   $foo.0.name     → $['foo']['0']['name']
  //   $foo.0.1.2.bar.0 → $['foo']['0']['1']['2']['bar']['0']

  // Transform all signal patterns
  expr = expr
    // $['x'] → $x (normalize existing bracket notation)
    .replace(/\$\['([a-zA-Z_$\d][\w$]*)'\]/g, '$$$1')
    // $x → $['x'] (including dots and hyphens)
    .replace(/\$([a-zA-Z_\d]\w*(?:[.-]\w+)*)/g, (_, signalName) => {
      const parts = signalName.split('.')
      return parts.reduce(
        (acc: string, part: string) => `${acc}['${part}']`,
        '$',
      )
    })
    // $ inside brackets: [$x] → [$['x']]
    .replace(
      /\[(\$[a-zA-Z_\d]\w*)\]/g,
      (_, varName) => `[$['${varName.slice(1)}']]`,
    )

  // Ignore any escaped values
  const escaped = new Map<string, string>()
  const escapeRe = RegExp(`(?:${DSP})(.*?)(?:${DSS})`, 'gm')
  let counter = 0
  for (const match of expr.matchAll(escapeRe)) {
    const k = match[1]
    const v = `dsEscaped${counter++}`
    escaped.set(v, k)
    expr = expr.replace(DSP + k + DSS, v)
  }

  const nameGen = (prefix: string, name: string) => {
    return `${prefix}${snake(name).replaceAll(/\./g, '_')}`
  }

  // Replace any action calls
  const actionsCalled = new Set<string>()
  const actionsRe = RegExp(`@(${Object.keys(actions).join('|')})\\(`, 'gm')
  const actionMatches = [...expr.matchAll(actionsRe)]
  const actionNames = new Set<string>()
  const actionFns = new Set<(...args: any[]) => any>()
  if (actionMatches.length) {
    const actionPrefix = `${DATASTAR}Act_`
    for (const match of actionMatches) {
      const actionName = match[1]
      const action = actions[actionName]
      if (!action) {
        continue
      }
      actionsCalled.add(actionName)

      const name = nameGen(actionPrefix, actionName)

      // Add ctx to action calls
      expr = expr.replace(`@${actionName}(`, `${name}(`)
      actionNames.add(name)
      actionFns.add((...args: any[]) => action.fn(ctx, ...args))
    }
  }

  // Replace any escaped values
  for (const [k, v] of escaped) {
    expr = expr.replace(k, v)
  }

  ctx.fnContent = expr

  try {
    const fn = Function(
      'el',
      '$',
      ...(attrPlugin?.argNames || []),
      ...actionNames,
      expr,
    )
    return (...args: any[]) => {
      try {
        return fn(ctx.el, root, ...args, ...actionFns)
      } catch (e: any) {
        throw ctx.runtimeErr('ExecuteExpression', {
          error: e.message,
        })
      }
    }
  } catch (error: any) {
    throw ctx.runtimeErr('GenerateExpression', {
      error: error.message,
    })
  }
}
