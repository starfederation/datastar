import { Signal } from "../vendored";
import { Engine } from "./engine";
import { NestedSignal, NestedValues } from "./types";

// If onlyPublic is true, only signals not starting with an underscore are included
function nestedValues(
  signal: NestedSignal,
  onlyPublic = false
): Record<string, any> {
  const kv: Record<string, any> = {};
  for (const key in signal) {
    if (signal.hasOwnProperty(key)) {
      const value = signal[key];
      if (value instanceof Signal) {
        if (onlyPublic && key.startsWith("_")) {
          continue;
        }
        kv[key] = value.value;
      } else {
        kv[key] = nestedValues(value);
      }
    }
  }
  return kv;
}

function mergeNested(
  target: NestedValues,
  values: NestedValues,
  onlyIfMissing = false
): void {
  for (const key in values) {
    if (values.hasOwnProperty(key)) {
      const value = values[key];
      if (value instanceof Object && !Array.isArray(value)) {
        if (!target[key]) {
          target[key] = {};
        }
        mergeNested(
          target[key] as NestedValues,
          value as NestedValues,
          onlyIfMissing
        );
      } else {
        if (onlyIfMissing && target[key]) {
          continue;
        }
        target[key] = new Signal(value);
      }
    }
  }
}

function walkNested(
  signal: NestedSignal,
  cb: (name: string, signal: Signal<any>) => void
): void {
  for (const key in signal) {
    if (signal.hasOwnProperty(key)) {
      const value = signal[key];
      if (value instanceof Signal) {
        cb(key, value);
      } else {
        walkNested(value as NestedSignal, cb);
      }
    }
  }
}

// Recursive function to subset a nested object, each key is a dot-delimited path
function nestedSubset(original: NestedValues, ...keys: string[]): NestedValues {
  const subset: NestedValues = {};
  for (const key of keys) {
    const parts = key.split(".");
    let subOriginal = original;
    let subSubset = subset;
    for (let i = 0; i < parts.length - 1; i++) {
      const part = parts[i];
      if (!subOriginal[part]) {
        return {};
      }
      if (!subSubset[part]) {
        subSubset[part] = {};
      }
      subOriginal = subOriginal[part] as NestedValues;
      subSubset = subSubset[part] as NestedValues;
    }
    const last = parts[parts.length - 1];
    subSubset[last] = subOriginal[last];
  }
  return subset;
}

export class SignalsRoot {
  private _signals: NestedSignal = {};

  constructor(private engine: Engine) {}

  exists(dotDelimitedPath: string): boolean {
    return !!this.signal(dotDelimitedPath);
  }

  signal(dotDelimitedPath: string): Signal<any> | null {
    const parts = dotDelimitedPath.split(".");
    let subSignals = this._signals;
    for (let i = 0; i < parts.length - 1; i++) {
      const part = parts[i];
      if (!subSignals[part]) {
        return null;
      }
      subSignals = subSignals[part] as NestedSignal;
    }
    const last = parts[parts.length - 1];
    return subSignals[last] as Signal<any>;
  }

  add<T extends Signal>(dotDelimitedPath: string, signal: T) {
    const parts = dotDelimitedPath.split(".");
    let subSignals = this._signals;
    for (let i = 0; i < parts.length - 1; i++) {
      const part = parts[i];
      if (!subSignals[part]) {
        subSignals[part] = {};
      }
      subSignals = subSignals[part] as NestedSignal;
    }
    const last = parts[parts.length - 1];
    subSignals[last] = signal;
  }

  value<T>(dotDelimitedPath: string): T {
    const signal = this.signal(dotDelimitedPath);
    return signal?.value;
  }

  set<T>(dotDelimitedPath: string, value: T) {
    const s = this.upsert(dotDelimitedPath, value);
    s.value = value;
  }

  upsert<T>(dotDelimitedPath: string, value: T) {
    const parts = dotDelimitedPath.split(".");
    let subSignals = this._signals;
    for (let i = 0; i < parts.length - 1; i++) {
      const part = parts[i];
      if (!subSignals[part]) {
        subSignals[part] = {};
      }
      subSignals = subSignals[part] as NestedSignal;
    }
    const last = parts[parts.length - 1];

    const current = subSignals[last];
    if (!!current) return current as Signal<T>;

    const signal = new Signal(value);
    subSignals[last] = signal;

    return signal;
  }

  remove(...dotDelimitedPaths: string[]) {
    let hadChanges = false;
    for (const path of dotDelimitedPaths) {
      const parts = path.split(".");
      let subSignals = this._signals;
      for (let i = 0; i < parts.length - 1; i++) {
        const part = parts[i];
        if (!subSignals[part]) {
          return;
        }
        subSignals = subSignals[part] as NestedSignal;
      }
      const last = parts[parts.length - 1];
      delete subSignals[last];
      hadChanges = true;
    }

    if (hadChanges) {
      this.engine.applyPlugins(document.body);
    }
  }

  merge(other: NestedValues, onlyIfMissing = false) {
    mergeNested(this._signals, other, onlyIfMissing);
  }

  subset(...keys: string[]): NestedValues {
    return nestedSubset(this.values(), ...keys);
  }

  walk(cb: (name: string, signal: Signal<any>) => void) {
    walkNested(this._signals, cb);
  }

  values(onlyPublic = false): NestedValues {
    return nestedValues(this._signals, onlyPublic);
  }

  JSON(shouldIndent = true, onlyPublic = false) {
    const values = this.values(onlyPublic);
    if (!shouldIndent) {
      return JSON.stringify(values);
    }
    return JSON.stringify(values, null, 2);
  }

  public toString() {
    return this.JSON();
  }
}
