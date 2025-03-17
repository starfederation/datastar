abstract class Subscriber {
  abstract markDirty(): void
}

export abstract class Dependency {
  abstract readonly value: unknown
  abstract version(): number
  abstract addSubscribers(...subscribers: Subscriber[]): void
  abstract removeSubscribers(...subscribers: Subscriber[]): void
}

export class Signal<T> extends Dependency implements Subscriber {
  subs = new Set<Subscriber>()
  ver = 1

  constructor(
    private val: T,
    public onChange?: (val: T) => void,
  ) {
    super()
  }

  set value(val: T) {
    if (this.val === val) {
      return
    }
    this.val = val
    this.ver++
    this.markDirty()
    this.onChange?.(val)
  }

  markDirty() {
    for (const sub of this.subs) {
      sub.markDirty()
    }
  }

  get value() {
    return this.val
  }

  version() {
    return this.ver
  }

  addSubscribers(...subscribers: Subscriber[]) {
    for (const sub of subscribers) {
      this.subs.add(sub)
    }
  }

  removeSubscribers(...subscribers: Subscriber[]) {
    for (const sub of subscribers) {
      this.subs.delete(sub)
    }
  }
}

export function signal<T>(initialValue: T) {
  return new Signal(initialValue)
}

export type DerivedFn<O> = (...args: unknown[]) => O

export class Derived<O> extends Dependency implements Subscriber {
  subs = new Set<Subscriber>()
  isDirty = true
  ver = 1
  val!: O
  versionSum = 0

  constructor(
    private deps: Dependency[],
    private fn: DerivedFn<O>,
  ) {
    super()
    for (const dep of deps) {
      dep.addSubscribers(this)
    }
  }

  get value() {
    if (!this.isDirty) {
      return this.val
    }
    this.isDirty = false
    let depsVersionSum = 0
    for (const dep of this.deps) {
      depsVersionSum += dep.version()
    }
    if (depsVersionSum === this.versionSum) {
      return this.val
    }

    this.versionSum = depsVersionSum
    const args = this.deps.map((dep) => dep.value)
    const currentVal = this.fn(...args)
    if (this.val === currentVal) {
      return this.val
    }
    this.val = currentVal
    this.ver++
    return this.val
  }

  version() {
    return this.ver
  }

  markDirty() {
    this.isDirty = true
    for (const sub of this.subs) {
      sub.markDirty()
    }
  }

  addSubscribers(...subscribers: Subscriber[]) {
    for (const sub of subscribers) {
      this.subs.add(sub)
    }
  }

  removeSubscribers(...subscribers: Subscriber[]) {
    for (const sub of subscribers) {
      this.subs.delete(sub)
    }
  }
}

export function computed<O>(deps: Dependency[], fn: (...args: unknown[]) => O) {
  return new Derived(deps, fn)
}

export class Effect implements Subscriber {
  depsVersionSum = -1

  constructor(
    private deps: Dependency[],
    private fn: (...args: unknown[]) => void,
  ) {
    for (const dep of deps) {
      dep.addSubscribers(this)
    }
  }

  markDirty() {
    let depsVersionSum = 0
    for (const dep of this.deps) {
      depsVersionSum += dep.version()
    }
    if (depsVersionSum === this.depsVersionSum) {
      return
    }
    this.depsVersionSum = depsVersionSum
    const args = this.deps.map((dep) => dep.value)
    this.fn(...args)
  }
}

export type OnRemovalFn = () => void
export type EffectFn = (
  deps: Dependency[],
  fn: (...args: unknown[]) => void,
) => OnRemovalFn

export function effect(
  deps: Dependency[],
  fn: (...args: unknown[]) => void,
): OnRemovalFn {
  const e = new Effect(deps, fn)
  e.markDirty()
  return () => {
    for (const dep of deps) {
      dep.removeSubscribers(e)
    }
  }
}
