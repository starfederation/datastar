import { type Computed, Signal, computed } from '../vendored/preact-core'
import { dsErr } from './errors'
import type { NestedSignal, NestedValues } from './types'

// If onlyPublic is true, only signals not starting with an underscore are included
function nestedValues(
  signal: NestedSignal,
  onlyPublic = false,
): Record<string, any> {
  const kv: Record<string, any> = {}
  for (const key in signal) {
    if (Object.hasOwn(signal, key)) {
      if (onlyPublic && key.startsWith('_')) {
        continue
      }
      const value = signal[key]
      if (value instanceof Signal) {
        kv[key] = value.value
      } else {
        kv[key] = nestedValues(value)
      }
    }
  }
  return kv
}

function mergeNested(
  target: NestedValues,
  values: NestedValues,
  onlyIfMissing = false,
): void {
  for (const key in values) {
    if (Object.hasOwn(values, key)) {
      if (key.match(/\_\_+/)) {
        throw dsErr('InvalidSignalKey', { key })
      }

      const value = values[key]
      if (value instanceof Object && !Array.isArray(value)) {
        if (!target[key]) {
          target[key] = {}
        }
        mergeNested(
          target[key] as NestedValues,
          value as NestedValues,
          onlyIfMissing,
        )
      } else {
        const hasKey = Object.hasOwn(target, key)
        if (hasKey) {
          if (onlyIfMissing) continue
          const t = target[key]
          if (t instanceof Signal) {
            t.value = value
            continue
          }
        }
        target[key] = new Signal(value)
      }
    }
  }
}

function walkNestedSignal(
  signal: NestedSignal,
  cb: (dotDeliminatedPath: string, signal: Signal<any>) => void,
): void {
  for (const key in signal) {
    if (Object.hasOwn(signal, key)) {
      const value = signal[key]
      if (value instanceof Signal) {
        cb(key, value)
      } else {
        walkNestedSignal(value, (path, value) => {
          cb(`${key}.${path}`, value)
        })
      }
    }
  }
}

// Recursive function to subset a nested object, each key is a dot-delimited path
function nestedSubset(original: NestedValues, ...keys: string[]): NestedValues {
  const subset: NestedValues = {}
  for (const key of keys) {
    const parts = key.split('.')
    let subOriginal = original
    let subSubset = subset
    for (let i = 0; i < parts.length - 1; i++) {
      const part = parts[i]
      if (!subOriginal[part]) {
        return {}
      }
      if (!subSubset[part]) {
        subSubset[part] = {}
      }
      subOriginal = subOriginal[part] as NestedValues
      subSubset = subSubset[part] as NestedValues
    }
    const last = parts[parts.length - 1]
    subSubset[last] = subOriginal[last]
  }
  return subset
}

// Recursively walk a NestedValue with a callback and dot-delimited path
export function walkNestedValues(
  nv: NestedValues,
  cb: (path: string, value: any) => void,
) {
  for (const key in nv) {
    if (Object.hasOwn(nv, key)) {
      const value = nv[key]
      if (value instanceof Object && !Array.isArray(value)) {
        walkNestedValues(value, (path, value) => {
          cb(`${key}.${path}`, value)
        })
      } else {
        cb(key, value)
      }
    }
  }
}

export class SignalsRoot {
  #signals: NestedSignal = {}

  exists(dotDelimitedPath: string): boolean {
    return !!this.signal(dotDelimitedPath)
  }

  signal<T>(dotDelimitedPath: string): Signal<T> | null {
    const parts = dotDelimitedPath.split('.')
    let subSignals = this.#signals
    for (let i = 0; i < parts.length - 1; i++) {
      const part = parts[i]
      if (!subSignals[part]) {
        return null
      }
      subSignals = subSignals[part] as NestedSignal
    }
    const last = parts[parts.length - 1]
    const signal = subSignals[last]
    if (!signal) throw dsErr('SignalNotFound', { path: dotDelimitedPath })
    return signal as Signal<T>
  }

  setSignal<T extends Signal<T>>(dotDelimitedPath: string, signal: T) {
    const parts = dotDelimitedPath.split('.')
    let subSignals = this.#signals
    for (let i = 0; i < parts.length - 1; i++) {
      const part = parts[i]
      if (!subSignals[part]) {
        subSignals[part] = {}
      }
      subSignals = subSignals[part] as NestedSignal
    }
    const last = parts[parts.length - 1]
    subSignals[last] = signal
  }

  setComputed<T>(dotDelimitedPath: string, fn: () => T) {
    const signal = computed(() => fn()) as Computed
    this.setSignal(dotDelimitedPath, signal)
  }

  value<T>(dotDelimitedPath: string): T {
    const signal = this.signal(dotDelimitedPath) as Signal<T>
    return signal?.value
  }

  setValue<T>(dotDelimitedPath: string, value: T) {
    const s = this.upsertIfMissing(dotDelimitedPath, value)
    s.value = value
  }

  upsertIfMissing<T>(dotDelimitedPath: string, defaultValue: T) {
    const parts = dotDelimitedPath.split('.')
    let subSignals = this.#signals
    for (let i = 0; i < parts.length - 1; i++) {
      const part = parts[i]
      if (!subSignals[part]) {
        subSignals[part] = {}
      }
      subSignals = subSignals[part] as NestedSignal
    }
    const last = parts[parts.length - 1]

    const current = subSignals[last]
    if (current instanceof Signal) {
      return current as Signal<T>
    }

    const signal = new Signal(defaultValue)
    subSignals[last] = signal

    return signal
  }

  remove(...dotDelimitedPaths: string[]) {
    for (const path of dotDelimitedPaths) {
      const parts = path.split('.')
      let subSignals = this.#signals
      for (let i = 0; i < parts.length - 1; i++) {
        const part = parts[i]
        if (!subSignals[part]) {
          return
        }
        subSignals = subSignals[part] as NestedSignal
      }
      const last = parts[parts.length - 1]
      delete subSignals[last]
    }
  }

  merge(other: NestedValues, onlyIfMissing = false) {
    mergeNested(this.#signals, other, onlyIfMissing)
  }

  subset(...keys: string[]): NestedValues {
    return nestedSubset(this.values(), ...keys)
  }

  walk(cb: (name: string, signal: Signal<any>) => void) {
    walkNestedSignal(this.#signals, cb)
  }

  values(onlyPublic = false): NestedValues {
    return nestedValues(this.#signals, onlyPublic)
  }

  rootProps(onlyPublic = false): Array<string> {
    const props: Array<string> = Object.keys(this.#signals)
    if (onlyPublic) {
      return props.filter((prop) => !prop.startsWith("_"))
    }
    return props
  }

  JSON(shouldIndent = true, onlyPublic = false) {
    const values = this.values(onlyPublic)
    if (!shouldIndent) {
      return JSON.stringify(values)
    }
    return JSON.stringify(values, null, 2)
  }

  public toString() {
    return this.JSON()
  }
}
