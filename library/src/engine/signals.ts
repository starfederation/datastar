import { DATASTAR_SIGNAL_PATCH_EVENT } from '@engine/consts'
import type {
  Computed,
  Effect,
  JSONPatch,
  MergePatchArgs,
  Paths,
  Signal,
  SignalFilterOptions,
} from '@engine/types'
import { isPojo, pathToObj } from '@utils/paths'
import { hasOwn } from '@utils/polyfills'

interface ReactiveNode {
  deps_?: Link
  depsTail_?: Link
  subs_?: Link
  subsTail_?: Link
  flags_: ReactiveFlags
}

interface Link {
  version_: number
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

interface AlienComputed<T = unknown> extends ReactiveNode {
  value_?: T
  getter(previousValue?: T): T
}

interface AlienSignal<T = unknown> extends ReactiveNode {
  previousValue: T
  value_: T
}

const currentPatch: Paths = []
const queuedEffects: (AlienEffect | undefined)[] = []
let batchDepth = 0
let notifyIndex = 0
let queuedEffectsLength = 0
let prevSub: ReactiveNode | undefined
let activeSub: ReactiveNode | undefined
let version = 0

export const beginBatch = (): void => {
  batchDepth++
}

export const endBatch = (): void => {
  if (!--batchDepth) {
    flush()
    dispatch()
  }
}

export const startPeeking = (sub?: ReactiveNode): void => {
  prevSub = activeSub
  activeSub = sub
}

export const stopPeeking = (): void => {
  activeSub = prevSub
  prevSub = undefined
}

export const signal = <T>(initialValue?: T): Signal<T> => {
  return signalOper.bind(0, {
    previousValue: initialValue,
    value_: initialValue,
    flags_: 1 satisfies ReactiveFlags.Mutable,
  }) as Signal<T>
}

const computedSymbol = Symbol('computed')
export const computed = <T>(getter: (previousValue?: T) => T): Computed<T> => {
  const c = computedOper.bind(0, {
    flags_: 17 as ReactiveFlags.Mutable | ReactiveFlags.Dirty,
    getter,
  }) as Computed<T>
  // @ts-expect-error
  c[computedSymbol] = 1
  return c
}

export const effect = (fn: () => void): Effect => {
  const e: AlienEffect = {
    fn_: fn,
    flags_: 2 satisfies ReactiveFlags.Watching,
  }
  if (activeSub) {
    link(e, activeSub)
  }
  startPeeking(e)
  beginBatch()
  try {
    e.fn_()
  } finally {
    endBatch()
    stopPeeking()
  }
  return effectOper.bind(0, e)
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

const updateComputed = (c: AlienComputed): boolean => {
  startPeeking(c)
  startTracking(c)
  try {
    const oldValue = c.value_
    return oldValue !== (c.value_ = c.getter(oldValue))
  } finally {
    stopPeeking()
    endTracking(c)
  }
}

const updateSignal = <T>(s: AlienSignal<T>, value: T): boolean => {
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
    startPeeking(e)
    startTracking(e)
    beginBatch()
    try {
      e.fn_()
    } finally {
      endBatch()
      stopPeeking()
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

const signalOper = <T>(s: AlienSignal<T>, ...value: [T]): T | boolean => {
  if (value.length) {
    if (s.value_ !== (s.value_ = value[0])) {
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
  const nextDep = prevDep ? prevDep.nextDep_ : sub.deps_
  if (nextDep && nextDep.dep_ === dep) {
    nextDep.version_ = version
    sub.depsTail_ = nextDep
    return
  }
  const prevSub = dep.subsTail_
  if (prevSub && prevSub.version_ === version && prevSub.sub_ === sub) {
    return
  }
  const newLink =
    (sub.depsTail_ =
    dep.subsTail_ =
      {
        version_: version,
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

const unlink = (link: Link, sub = link.sub_): Link | undefined => {
  const dep_ = link.dep_
  const prevDep_ = link.prevDep_
  const nextDep_ = link.nextDep_
  const nextSub_ = link.nextSub_
  const prevSub_ = link.prevSub_
  if (nextDep_) {
    nextDep_.prevDep_ = prevDep_
  } else {
    sub.depsTail_ = prevDep_
  }
  if (prevDep_) {
    prevDep_.nextDep_ = nextDep_
  } else {
    sub.deps_ = nextDep_
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
        const nextSub = (link = subSubs).nextSub_
        if (nextSub) {
          stack = { value_: next, prev_: stack }
          next = nextSub
        }
        continue
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
  version++
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
  let dirty = false

  top: while (true) {
    const dep = link.dep_
    const flags = dep.flags_

    if (sub.flags_ & (16 satisfies ReactiveFlags.Dirty)) {
      dirty = true
    } else if (
      (flags & (17 as ReactiveFlags.Mutable | ReactiveFlags.Dirty)) ===
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
      (flags & (33 as ReactiveFlags.Mutable | ReactiveFlags.Pending)) ===
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

    if (!dirty) {
      const nextDep = link.nextDep_
      if (nextDep) {
        link = nextDep
        continue
      }
    }

    while (checkDepth--) {
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
        dirty = false
      } else {
        sub.flags_ &= ~(32 satisfies ReactiveFlags.Pending)
      }
      sub = link.sub_
      if (link.nextDep_) {
        link = link.nextDep_
        continue top
      }
    }

    return dirty
  }
}

const shallowPropagate = (link: Link): void => {
  do {
    const sub = link.sub_
    const flags = sub.flags_
    if (
      (flags & (48 as ReactiveFlags.Pending | ReactiveFlags.Dirty)) ===
      (32 satisfies ReactiveFlags.Pending)
    ) {
      sub.flags_ = flags | (16 satisfies ReactiveFlags.Dirty)
      if (flags & (2 satisfies ReactiveFlags.Watching)) {
        notify(sub as AlienEffect)
      }
    }
  } while ((link = link.nextSub_!))
}

const isValidLink = (checkLink: Link, sub: ReactiveNode): boolean => {
  let link = sub.depsTail_
  while (link) {
    if (link === checkLink) {
      return true
    }
    link = link.prevDep_
  }
  return false
}

export const getPath = <T = any>(path: string): T | undefined => {
  let result = root
  const split = path.split('.')
  for (const path of split) {
    if (result == null || !hasOwn(result, path)) {
      return
    }
    result = result[path]
  }
  return result as T
}

const deep = (value: any, prefix = ''): any => {
  const isArr = Array.isArray(value)
  if (isArr || isPojo(value)) {
    const deepObj = (isArr ? [] : {}) as Record<string, Signal<any>>
    for (const key in value) {
      deepObj[key] = signal(
        deep((value as Record<string, Signal<any>>)[key], `${prefix + key}.`),
      )
    }
    const keys = signal(0)
    return new Proxy(deepObj, {
      get(_, prop: string) {
        // JSON.stringify calls `.toJSON()` directly instead of checking if it exists on the object
        // so we have to check if `toJSON` is being called and prevent a signal from automatically
        // being made so JSON.stringify can fallback to the default stringify
        if (!(prop === 'toJSON' && !hasOwn(deepObj, prop))) {
          // special case for when prop is an array function because every array function needs to
          // be reactive to when the keys change
          if (isArr && prop in Array.prototype) {
            keys()
            return deepObj[prop]
          }
          // if prop is a symbol just return the symbol because we don't want to make up that theres
          // an iterator symbol on an object or not
          if (typeof prop === 'symbol') {
            return deepObj[prop]
          }
          // if obj doesnt have prop OR prop is null or undefined then create a signal and default
          // to an empty string
          if (!hasOwn(deepObj, prop) || deepObj[prop]() == null) {
            deepObj[prop] = signal('')
            dispatch(prefix + prop, '')
            keys(keys() + 1)
          }
          return deepObj[prop]()
        }
      },
      set(_, prop: string, newValue) {
        const path = prefix + prop
        // special case for when setting length so we can make a diff patch
        if (isArr && prop === 'length') {
          const diff = (deepObj[prop] as unknown as number) - newValue
          deepObj[prop] = newValue
          // manually make a diff patch for now, shouldnt have to do this when object diffing is
          // implemented. see https://github.com/starfederation/datastar-dev/issues/274
          if (diff > 0) {
            const patch: Record<string, any> = {}
            for (let i = newValue; i < deepObj[prop]; i++) {
              patch[i] = null
            }
            dispatch(prefix.slice(0, -1), patch)
            keys(keys() + 1)
          }
        } else if (hasOwn(deepObj, prop)) {
          if (newValue == null) {
            delete deepObj[prop]
            // if newValue is a computed set the computed directly instead of wrapping in signal
          } else if (hasOwn(newValue, computedSymbol)) {
            deepObj[prop] = newValue
            dispatch(path, '')
            // if prop changed after setting it then dispatch
          } else {
            const currentValue = deepObj[prop]()
            const pathStr = `${path}.`
            if (isPojo(currentValue) && isPojo(newValue)) {
              for (const key in currentValue) {
                if (!hasOwn(newValue, key)) {
                  delete currentValue[key]
                  dispatch(pathStr + key, null)
                }
              }
              for (const key in newValue) {
                const nextVal = newValue[key]
                if (currentValue[key] !== nextVal) {
                  currentValue[key] = nextVal
                }
              }
            } else if (deepObj[prop](deep(newValue, pathStr))) {
              dispatch(path, newValue)
            }
          }
          // if newValue is null or undefined then noop
        } else if (newValue != null) {
          // if newValue is a computed set the computed directly instead of wrapping in signal
          if (hasOwn(newValue, computedSymbol)) {
            deepObj[prop] = newValue
            dispatch(path, '')
          } else {
            deepObj[prop] = signal(deep(newValue, `${path}.`))
            dispatch(path, newValue)
          }
          keys(keys() + 1)
        }

        return true
      },
      deleteProperty(_, prop: string) {
        delete deepObj[prop]
        keys(keys() + 1)
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

export const mergePatch = (
  patch: JSONPatch,
  { ifMissing }: MergePatchArgs = {},
): void => {
  beginBatch()
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

export const mergePaths = (paths: Paths, options?: MergePatchArgs): void =>
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
        hasOwn(targetParent, target) &&
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
  } else if (!(ifMissing && hasOwn(targetParent, target))) {
    targetParent[target] = patch
  }
}

const toRegExp = (val: string | RegExp): RegExp =>
  typeof val === 'string' ? RegExp(val.replace(/^\/|\/$/g, '')) : val

/**
 * Filters the root store based on an include and exclude RegExp
 *
 * @returns The filtered object
 */
export const filtered = (
  { include = /.*/, exclude = /(?!)/ }: SignalFilterOptions = {},
  obj: JSONPatch = root,
): Record<string, any> => {
  const includeRe = toRegExp(include)
  const excludeRe = toRegExp(exclude)
  const paths: Paths = []
  const stack: [any, string][] = [[obj, '']]

  while (stack.length) {
    const [node, prefix] = stack.pop()!

    for (const key in node) {
      const path = prefix + key
      if (isPojo(node[key])) {
        stack.push([node[key], `${path}.`])
      } else if (includeRe.test(path) && !excludeRe.test(path)) {
        paths.push([path, getPath(path)])
      }
    }
  }

  return pathToObj(paths)
}

export const root: Record<string, any> = deep({})
