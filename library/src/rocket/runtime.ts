import {
  actions,
  apply,
  applyElement,
  applyMissing,
  cleanupTree,
  forgetRoot,
  hasAttributePlugin,
  isDocumentObserverActive,
  parseAttributeKey,
  setBindingPreprocessor,
  action as registerAction,
} from '@engine'
import {
  DATASTAR_READY_EVENT,
  DATASTAR_SCOPE_CHILDREN_EVENT,
} from '@engine/consts'
import { createHTML } from '@engine/csp'
import { createError } from '@engine/errors'
import {
  computed as datastarComputed,
  effect,
  getPath,
  mergePaths,
  root,
  signal,
} from '@engine/signals'
import type { HTMLOrSVG } from '@engine/types'
import { morph, setMorphOwnership } from '@plugins/watchers/patchElements'
import { aliasify, kebab } from '@utils/text'
import {
  type CodecRegistry,
  codecRegistry,
  decodeCodec,
  getCodecDefault,
  getCodecManifest,
  normalizeProperty,
  type InferProps,
  type PropDefs,
} from './codecs'
import { initRocketConditionals } from './conditional'
import { initRocketFors } from './for'
import {
  appendComposedTemplateValue,
  encodeSignalKey,
  hasRocketScopedExpression,
  hypertext,
  rewriteDataAttributes,
  rocketDispatchActionName,
  rocketRefAttr,
  type TaggedLiteral,
  type TaggedLiteralScope,
} from './template'

export const rocketHostAttr = 'data-rocket-host'
type AnyRecord = {
  [key: string]: any
}
type SetupSignal = (<T>(name: string, initialValue: T) => T) & AnyRecord
type RefRecord = Record<string, Element | undefined>
export type RefCtors = Record<string, abstract new (...args: any[]) => Element>
export type InstancesOf<C extends RefCtors> = {
  [K in keyof C]?: InstanceType<C[K]>
}
type StateRecord = AnyRecord
type AdoptedStyle = {
  root: ShadowRoot | HTMLElement
  text: string
  element?: HTMLStyleElement
  sheet?: CSSStyleSheet
}
type PropOverrideGetter<
  Props extends Record<string, any>,
  Name extends keyof Props & string,
> = (getDefault: () => Props[Name]) => any
type PropOverrideSetter<
  Props extends Record<string, any>,
  Name extends keyof Props & string,
> = (value: any, setDefault: (value: Props[Name]) => void) => void
type HostPropDescriptor = Omit<PropertyDescriptor, 'configurable'>
export type RocketHost = HTMLElement & {
  dispatchRocketAction(
    name: string,
    el: Element | null,
    evt: Event | undefined,
    cleanups: Map<string, () => void>,
    ...args: any[]
  ): any
}

export type RocketHostWithProps<Props extends Record<string, any>> =
  RocketHost & Props

type SetupEmit = {
  (type: string): void
  (...types: [string, ...string[]]): void
  <Detail>(
    type: string,
    detail: Detail,
    options?: Omit<CustomEventInit<Detail>, 'detail'>,
  ): void
}

type SetupEmitCancellable = {
  (type: string): boolean
  <Detail>(
    type: string,
    detail: Detail,
    options?: Omit<CustomEventInit<Detail>, 'detail' | 'cancelable'>,
  ): boolean
}

type PropObserver<Props extends Record<string, any>> =
  | (() => void)
  | ((props: Props, changes: Partial<Props>) => void)

type SetupContext<Props extends Record<string, any>> = {
  props: Props
  readonly internals: ElementInternals

  // Global Datastar signal store exposed directly to setup code.
  //
  // This is the reactive root used by `$foo`; Rocket-local `$$.foo` state still lives under the component's private `_rocket...` path.
  $: Record<string, any>

  // Declares and accesses instance-local Rocket state through a callable proxy.
  //
  // The callable `$$` proxy supports imperative local signal access because JavaScript cannot return a live reference to a primitive value.
  $$: SetupSignal
  effect(fn: () => void): () => void
  apply(root: HTMLOrSVG | ShadowRoot, merge?: boolean): void
  adoptStyles(host: HTMLElement, ...styles: string[]): void
  cleanup(fn: () => void): void
  emit: SetupEmit
  emitCancellable: SetupEmitCancellable

  // Global Datastar actions exposed as imperative functions for setup code.
  //
  // Unlike `action(name, fn)`, `actions` calls the existing global registry rather than registering component-local actions.
  actions: Record<string, (...args: any[]) => any>
  action(name: string, fn: RocketAction<Props>): void
  observeProps(
    fn: PropObserver<Props>,
    ...propNames: Array<keyof Props & string>
  ): () => void
  overrideProp<Name extends keyof Props & string>(
    name: Name,
    getter?: PropOverrideGetter<Props, Name>,
    setter?: PropOverrideSetter<Props, Name>,
  ): void
  defineHostProp(name: string, descriptor: HostPropDescriptor): void
  render: SetupRender<Props>
  host: RocketHostWithProps<Props>
}

type FirstUpdateContext<
  Props extends Record<string, any>,
  Refs extends RefCtors = RefCtors,
> = SetupContext<Props> & {
  refs: InstancesOf<Refs>
}

type RenderContext<Props extends Record<string, any>> = {
  html: TaggedLiteral
  svg: TaggedLiteral
  props: Props
  host: RocketHostWithProps<Props>
}

type RenderContextOverrides<Props extends Record<string, any>> = Partial<
  RenderContext<Props>
>

type RocketPrimitiveRenderValue =
  | string
  | number
  | boolean
  | bigint
  | Date
  | null
  | undefined

type RocketComposedRenderValue =
  | RocketPrimitiveRenderValue
  | Node
  | Iterable<RocketComposedRenderValue>

type RocketRenderValue =
  | DocumentFragment
  | RocketPrimitiveRenderValue
  | Iterable<RocketComposedRenderValue>

type RocketRender<Props extends Record<string, any>> = (
  context: RenderContext<Props>,
  ...args: any[]
) => RocketRenderValue

type SetupRender<Props extends Record<string, any>> = (
  context: RenderContextOverrides<Props>,
  ...args: any[]
) => void

type RocketAction<Props extends Record<string, any>> = (
  context: {
    host: RocketHostWithProps<Props>
    props: Props
    state: StateRecord
    el: Element | null
    evt: Event | undefined
  },
  ...args: any[]
) => any

type RocketDefinition<
  Defs extends PropDefs = PropDefs,
  Refs extends RefCtors = RefCtors,
> = {
  refs?: Refs
  props?: (codecs: CodecRegistry) => Defs
  manifest?: RocketManifestMeta
  setup?: (context: SetupContext<InferProps<Defs>>) => void
  onFirstRender?: (context: FirstUpdateContext<InferProps<Defs>, Refs>) => void
  render?: RocketRender<InferProps<Defs>>
  mode?: 'open' | 'closed' | 'light'
  ignoredByMorph?: boolean | 'children'
  formAssociated?: boolean
  delegatesFocus?: boolean
  formValue?:
    | false
    | ((
        context: SetupContext<InferProps<Defs>>,
      ) => string | File | FormData | null)
  onFormAssociated?: (
    context: SetupContext<InferProps<Defs>>,
    form: HTMLFormElement | null,
  ) => void
  onFormReset?: (context: SetupContext<InferProps<Defs>>) => void
  onFormDisabled?: (
    context: SetupContext<InferProps<Defs>>,
    disabled: boolean,
  ) => void
  onFormStateRestore?: (
    context: SetupContext<InferProps<Defs>>,
    state: string | File | FormData | null,
    mode: 'restore' | 'autocomplete',
  ) => void
  renderOnPropChange?:
    | boolean
    | ((context: {
        host: RocketHostWithProps<InferProps<Defs>>
        props: InferProps<Defs>
        changes: Partial<InferProps<Defs>>
      }) => boolean)
}

type RocketManifestMeta = {
  slots?: RocketManifestSlot[]
  events?: RocketManifestEvent[]
}

type RocketManifestSlot = {
  name: string
  description?: string
}

type RocketManifestEvent = {
  name: string
  kind?: 'event' | 'custom-event'
  bubbles?: boolean
  composed?: boolean
  description?: string
}

type RocketManifestProp = {
  name: string
  attribute: string
  type: string
  default: unknown
  required: boolean
  values?: readonly unknown[]
  docs?: {
    description?: string
    label?: string
    control?: 'auto' | 'text' | 'textarea' | 'number' | 'boolean' | 'select'
    placeholder?: string
  }
}

type RocketComponentManifest = {
  tag: string
  props: RocketManifestProp[]
  slots: RocketManifestSlot[]
  events: RocketManifestEvent[]
}

type RocketManifestDocument = {
  version: 1
  generatedAt: string
  components: RocketComponentManifest[]
}

type RocketPublishOptions = {
  endpoint: string
  headers?: Record<string, string>
}

type PropObserverRegistration<Props extends Record<string, any>> = {
  fn: PropObserver<Props>
  names: Set<keyof Props & string> | null
}

// Runtime surface used by Rocket's rewritten `@dispatchRocket(...)` calls.
//
// Datastar expressions know only the current DOM element, so this bridge lets dispatch recover the owning component and its local/global action resolution path.
type RocketActionBridge = RocketHost

const actionErrorContext = (el: HTMLOrSVG, name: string) => ({
  plugin: { type: 'action', name },
  element: { id: el.id, tag: el.tagName },
})

// Bridge context adapter from Rocket runtime calls into the engine's global action contract.
//
// Reusing the same context keeps global actions consistent across Datastar expressions, `@dispatchRocket(...)`, and imperative `setup({ actions })` calls.
const globalActionContext = (
  el: HTMLOrSVG,
  evt: Event | undefined,
  name: string,
  cleanups: Map<string, () => void>,
) => ({
  el,
  evt,
  error: createError.bind(0, actionErrorContext(el, name)),
  cleanups,
})

const rocketDeferredIgnoreAttr = 'data-rocket-deferred-ignore'
const rocketDataPrefix = aliasify('')
// Use the engine's configured attribute name so aliased bundles honor Rocket's temporary guard.
const rocketIgnoreAttr = aliasify('ignore')
// Mark speculative hosts separately until a concrete Rocket tag claims the guard.
const speculativeRocketTag = '*'
const rocketTags = new Map<string, RocketDefinition['mode']>()
// Wait by tag, without retaining unresolved or detached host elements.
const pendingSpeculativeTags = new Set<string>()

const hasRocketScopedAttributes = (root: HTMLElement) => {
  for (const node of [root, ...root.querySelectorAll('*')]) {
    for (const { name, value } of node.attributes) {
      if (
        name.startsWith(rocketDataPrefix) &&
        value.includes('$$') &&
        hasAttributePlugin(
          parseAttributeKey(name.slice(rocketDataPrefix.length)).pluginName,
        ) &&
        hasRocketScopedExpression(value)
      ) {
        const ignored = node.closest(`[${rocketIgnoreAttr}]`)
        if (
          !ignored ||
          (ignored === root &&
            root.getAttribute(rocketDeferredIgnoreAttr) ===
              speculativeRocketTag)
        )
          return true
      }
    }
  }
  return false
}

// Release only obsolete guards or hosts defined by another custom-element library.
const releaseSpeculativeRocketHosts = (
  root: ParentNode = document,
  definedTag?: string,
) => {
  const hosts =
    root instanceof HTMLElement &&
    root.getAttribute(rocketDeferredIgnoreAttr) === speculativeRocketTag
      ? [root]
      : [
          ...root.querySelectorAll<HTMLElement>(
            `[${rocketDeferredIgnoreAttr}="${speculativeRocketTag}"]`,
          ),
        ]
  for (const host of hosts) {
    const tag = host.localName
    if (rocketTags.has(tag)) continue
    if (definedTag !== tag && hasRocketScopedAttributes(host)) continue
    hiddenFromFirstApply.delete(host)
    host.removeAttribute(rocketDeferredIgnoreAttr)
    host.removeAttribute(rocketIgnoreAttr)
    // Let queued attribute records bind first, then fill in anything skipped
    // beneath the guard without executing existing bindings a second time.
    queueMicrotask(() => {
      if (host.isConnected) applyMissing(host)
    })
  }
}

const waitForSpeculativeDefinition = (tag: string) => {
  if (pendingSpeculativeTags.has(tag)) return
  pendingSpeculativeTags.add(tag)
  const release = () => {
    pendingSpeculativeTags.delete(tag)
    releaseSpeculativeRocketHosts(document, tag)
  }
  // An invalid custom-element name cannot become a Rocket component either.
  customElements.whenDefined(tag).then(release, release)
}

// Guard unresolved Rocket hosts before custom-element upgrade.
//
// Defer unresolved light hosts or shadow-host children so Datastar cannot evaluate Rocket-local directives before scoping.
const hiddenFromFirstApply = new WeakSet<Element>()

const markPendingRocketHosts = (root: ParentNode, deep = true) => {
  const nodes =
    root instanceof Element
      ? deep
        ? [root, ...root.querySelectorAll('*')]
        : [root]
      : [...root.querySelectorAll('*')]
  for (const node of nodes) {
    if (!(node instanceof HTMLElement)) continue
    const tagName = node.tagName.toLowerCase()
    const mode = rocketTags.get(tagName)
    if (customElements.get(tagName)) {
      if (
        !rocketTags.has(tagName) &&
        node.getAttribute(rocketDeferredIgnoreAttr) === speculativeRocketTag
      )
        releaseSpeculativeRocketHosts(node, tagName)
      continue
    }
    if (!mode) {
      if (
        !tagName.includes('-') ||
        node.hasAttribute(rocketDeferredIgnoreAttr) ||
        node.hasAttribute(rocketIgnoreAttr) ||
        !hasRocketScopedAttributes(node)
      ) {
        continue
      }
      node.setAttribute(rocketIgnoreAttr, '')
      node.setAttribute(rocketDeferredIgnoreAttr, speculativeRocketTag)
      hiddenFromFirstApply.add(node)
      waitForSpeculativeDefinition(tagName)
      continue
    }
    let skippedFirstApply = false
    if (node.getAttribute(rocketDeferredIgnoreAttr) === speculativeRocketTag) {
      skippedFirstApply = hiddenFromFirstApply.delete(node)
      node.removeAttribute(rocketDeferredIgnoreAttr)
      node.removeAttribute(rocketIgnoreAttr)
    }
    if (
      mode === 'light' &&
      (node.hasAttribute(rocketDeferredIgnoreAttr) ||
        node.hasAttribute(rocketIgnoreAttr))
    ) {
      continue
    }
    const targets = mode === 'light' ? [node] : [...node.children]
    for (const target of targets) {
      if (
        target.hasAttribute(rocketDeferredIgnoreAttr) ||
        target.hasAttribute(rocketIgnoreAttr)
      ) {
        continue
      }
      target.setAttribute(rocketIgnoreAttr, '')
      target.setAttribute(rocketDeferredIgnoreAttr, tagName)
      if (skippedFirstApply || !isDocumentObserverActive())
        hiddenFromFirstApply.add(target)
    }
  }
}

// Dynamic application may reach the engine before the DOM observer runs.
const preparePendingRocketBindings = (root: ParentNode, deep: boolean) => {
  if (root instanceof Element) {
    const host = root.closest<HTMLElement>(':not(:defined)')
    if (host instanceof HTMLElement) markPendingRocketHosts(host, false)
  }
  markPendingRocketHosts(root, deep)
}

// Keep the pre-upgrade guard in sync with live DOM insertion.
//
// Watch live DOM insertions and scoped attribute additions so newly unresolved Rocket content is deferred before Datastar can evaluate it.
const observePendingRocketHosts = () => {
  setBindingPreprocessor(preparePendingRocketBindings)
  markPendingRocketHosts(document)
  new MutationObserver((records) => {
    const guarded = new Set<HTMLElement>()
    for (const record of records) {
      if (record.target instanceof Element) {
        const host = record.target.closest<HTMLElement>(
          `[${rocketDeferredIgnoreAttr}="${speculativeRocketTag}"]`,
        )
        if (host) guarded.add(host)
      }
      if (record.type === 'attributes' && record.target instanceof Element) {
        const name = record.attributeName
        if (
          name?.startsWith(rocketDataPrefix) &&
          record.target.getAttribute(name)?.includes('$$')
        ) {
          markPendingRocketHosts(record.target)
        }
        continue
      }
      for (const node of record.addedNodes) {
        if (node instanceof Element) markPendingRocketHosts(node)
      }
      // Added subtrees were scanned above; the target's existing children were
      // scanned when inserted. Re-scanning them for each removal is quadratic.
      if (record.addedNodes.length && record.target instanceof Element) {
        markPendingRocketHosts(record.target, false)
      }
    }
    for (const host of guarded) releaseSpeculativeRocketHosts(host)
  }).observe(document.documentElement, {
    subtree: true,
    childList: true,
    attributes: true,
  })
}

let rocketBootstrapStarted = false
let rocketBootstrapWaiting = false
let pendingHostObservationStarted = false

// Queue early `rocket(tag, options)` registrations because custom-element setup must wait for Datastar's document observer.
const pendingRocketDefinitions = new Map<string, RocketDefinition<any, any>>()

// Start the global Rocket bootstrap once, then flush queued registrations after Datastar is ready to observe them.
const ensureRocketBootstrap = () => {
  if (!pendingHostObservationStarted) {
    pendingHostObservationStarted = true
    observePendingRocketHosts()
  }
  if (rocketBootstrapStarted || rocketBootstrapWaiting) return
  if (isDocumentObserverActive()) {
    rocketBootstrapStarted = true
    if (pendingRocketDefinitions.size === 0) return
    for (const [tag, options] of pendingRocketDefinitions) {
      pendingRocketDefinitions.delete(tag)
      rocket(tag, options)
    }
    return
  }
  rocketBootstrapWaiting = true
  document.addEventListener(
    DATASTAR_READY_EVENT,
    () => {
      rocketBootstrapWaiting = false
      if (rocketBootstrapStarted) return
      rocketBootstrapStarted = true
      if (pendingRocketDefinitions.size === 0) return
      for (const [tag, options] of pendingRocketDefinitions) {
        pendingRocketDefinitions.delete(tag)
        rocket(tag, options)
      }
    },
    { once: true },
  )
}

ensureRocketBootstrap()

const registeredTags = new Set<string>()
const rocketManifestRegistry = new Map<string, RocketComponentManifest>()
const namedInstanceCounts = new Map<string, number>()
let nextInstanceId = 0

// Rocket paths are dot-delimited and embedded in expressions and data-* attributes, so replace unsafe separators without changing IDs such as `myComponent1`.
const signalPathSegment = (value: string): string =>
  value
    .trim()
    .replace(/[^A-Za-z0-9_$]+/g, '_')
    .replace(/_+/g, '_')
    .replace(/^_+|_+$/g, '')

const cloneRocketManifestComponent = (
  component: RocketComponentManifest,
): RocketComponentManifest => ({
  tag: component.tag,
  props: component.props.map((prop) => ({
    ...prop,
    values: prop.values ? [...prop.values] : undefined,
    docs: prop.docs ? { ...prop.docs } : undefined,
  })),
  slots: component.slots.map((slot) => ({ ...slot })),
  events: component.events.map((event) => ({ ...event })),
})

const rocketManifestDocument = (): RocketManifestDocument => ({
  version: 1,
  generatedAt: new Date().toISOString(),
  components: [...rocketManifestRegistry.values()]
    .map(cloneRocketManifestComponent)
    .sort((left, right) => left.tag.localeCompare(right.tag)),
})

export const publishRocketManifests = async ({
  endpoint,
  headers,
}: RocketPublishOptions) =>
  fetch(endpoint, {
    method: 'POST',
    headers: {
      'content-type': 'application/json',
      ...headers,
    },
    body: JSON.stringify(rocketManifestDocument()),
  })

// Map every possible Rocket action origin back to its owning component instance.
//
// This includes:
// - the custom element host
// - host light-DOM children with Rocket-scoped expressions
// - rendered descendants inside the light/shadow mount root
//
// Without this bridge, `@dispatchRocket(...)` would know the current element but not which component should resolve the action.
const rocketHostsByElement = new WeakMap<Element, RocketActionBridge>()

registerAction({
  name: rocketDispatchActionName,

  // `@dispatchRocket(...)` is the bridge emitted by Rocket's expression rewrite.
  // Resolve the active component from the evaluated element so the call reaches the correct instance.
  // Reactive structures can add elements after the direct ownership map was indexed, so fall back to their nearest mapped composed ancestor.
  apply({ el, evt, cleanups }, name: string, ...args: any[]) {
    for (let node: Element | null = el; node; ) {
      const host = rocketHostsByElement.get(node)
      if (host) {
        return host.dispatchRocketAction(name, el, evt, cleanups, ...args)
      }
      const root = node.getRootNode()
      node =
        node.parentElement ?? (root instanceof ShadowRoot ? root.host : null)
    }
  },
})

// Register a custom element tag and wire its props, setup, render, and Datastar bridge into the element class.
const sharedStyleSheets = new WeakMap<Document, Map<string, CSSStyleSheet>>()

export function rocket<
  Refs extends RefCtors = RefCtors,
  Defs extends PropDefs = PropDefs,
>(tag: string, options?: RocketDefinition<Defs, Refs>) {
  if (!tag) return
  if (!isDocumentObserverActive()) {
    const existing = customElements.get(tag)
    if (existing) return existing
    rocketTags.set(tag, options?.mode ?? 'open')
    markPendingRocketHosts(document)
    pendingRocketDefinitions.set(
      tag,
      (options ?? {}) as RocketDefinition<any, any>,
    )
    ensureRocketBootstrap()
    return
  }
  const {
    manifest,
    mode,
    props,
    render,
    renderOnPropChange,
    setup,
    onFirstRender,
    ignoredByMorph = false,
    formAssociated = false,
    delegatesFocus = false,
    formValue,
    onFormAssociated,
    onFormReset,
    onFormDisabled,
    onFormStateRestore,
  } = options ?? {}
  if (registeredTags.has(tag) || customElements.get(tag)) {
    registeredTags.add(tag)
    return customElements.get(tag)
  }
  const propDefs = (props?.(codecRegistry) ?? {}) as Defs
  if (
    ignoredByMorph !== false &&
    ignoredByMorph !== true &&
    ignoredByMorph !== 'children'
  ) {
    throw new TypeError(
      'Rocket ignoredByMorph must be false, true, or "children".',
    )
  }
  const propNames = Object.keys(propDefs) as Array<
    keyof InferProps<Defs> & string
  >
  const attrToProp = new Map(propNames.map((name) => [kebab(name), name]))
  const booleanProps = new Set(
    propNames.filter(
      (name) => getCodecManifest(propDefs[name]).type === 'boolean',
    ),
  )
  for (const name of booleanProps) {
    const codec = propDefs[name]
    if (
      'defaultFactory' in codec &&
      typeof codec.defaultFactory === 'function'
    ) {
      throw new TypeError(
        `Rocket boolean prop "${name}" uses attribute presence and cannot declare a default.`,
      )
    }
  }
  const defaultPropValue = (name: keyof InferProps<Defs> & string) =>
    booleanProps.has(name) ? false : getCodecDefault(propDefs[name])
  const attributePropValue = (
    name: keyof InferProps<Defs> & string,
    value: string | null,
  ) =>
    booleanProps.has(name)
      ? value !== null
      : value === null
        ? defaultPropValue(name)
        : decodeCodec(propDefs[name], value)

  registeredTags.add(tag)
  rocketTags.set(tag, mode ?? 'open')
  markPendingRocketHosts(document)
  const componentManifest: RocketComponentManifest = {
    tag,
    props: propNames.map((name) => {
      const codec = propDefs[name]
      const codecManifest = getCodecManifest(codec)
      return {
        name,
        attribute: kebab(name),
        type: codecManifest.type,
        default: defaultPropValue(name),
        required: false,
        values: codecManifest.values ? [...codecManifest.values] : undefined,
        docs: codecManifest.docs ? { ...codecManifest.docs } : undefined,
      }
    }),
    slots: (manifest?.slots ?? []).map((slot) => ({ ...slot })),
    events: (manifest?.events ?? []).map((event) => ({
      kind: 'event',
      ...event,
    })),
  }
  const tagScope = signalPathSegment(tag)
  const getReactiveScopedPath = <T>(path: string): T | undefined => {
    let value: any = root
    for (const segment of path.split('.')) {
      if (
        value == null ||
        (typeof value !== 'object' && typeof value !== 'function') ||
        !(segment in value)
      ) {
        return
      }
      value = value[segment]
    }
    return value as T
  }
  const hasReactiveScopedPath = (path: string): boolean => {
    let value: any = root
    for (const segment of path.split('.')) {
      if (
        value == null ||
        (typeof value !== 'object' && typeof value !== 'function') ||
        !(segment in value)
      ) {
        return false
      }
      value = value[segment]
    }
    return true
  }
  const subtreeUsesScopedSignals = (
    root: ParentNode,
    signalPathBase: string,
  ): boolean => {
    const scopedKey = encodeSignalKey(signalPathBase)
    const nodes =
      root instanceof Element
        ? [root, ...root.querySelectorAll('*')]
        : [...root.children, ...root.querySelectorAll('*')]
    for (const node of nodes) {
      for (const attr of node.attributes) {
        if (!attr.name.startsWith('data-')) continue
        if (
          attr.name === `${aliasify('signals')}:${scopedKey}` ||
          attr.name.startsWith(`${aliasify('signals')}:${scopedKey}.`)
        ) {
          return true
        }
        const colonIndex = attr.name.indexOf(':')
        if (colonIndex !== -1) {
          const key = attr.name.slice(colonIndex + 1).split('__', 1)[0]
          if (key === scopedKey || key.startsWith(`${scopedKey}.`)) {
            return true
          }
        }
        if (
          attr.value === signalPathBase ||
          attr.value.startsWith(`${signalPathBase}.`) ||
          attr.value.includes(`$.${signalPathBase}`)
        ) {
          return true
        }
      }
    }
    return false
  }
  const initRocketStructures = (
    root: ParentNode,
    scope: TaggedLiteralScope,
  ) => [
    ...initRocketFors(root, scope, initRocketStructures),
    ...initRocketConditionals(root, scope, initRocketStructures),
  ]

  class RocketElement extends HTMLElement {
    #instanceId = ''
    #signalPathBase = ''
    #props = Object.fromEntries(
      propNames.map((name) => [name, defaultPropValue(name)]),
    ) as InferProps<Defs>
    #refs: RefRecord = {}
    #state: StateRecord = {}
    #propObservers: Array<PropObserverRegistration<InferProps<Defs>>> = []
    #actions = new Map<string, RocketAction<InferProps<Defs>>>()
    #cleanups: Array<() => void> = []
    #conditionalCleanups: Array<() => void> = []
    #hostConditionalCleanups: Array<() => void> = []
    #globalActionCleanups = new Map<string, () => void>()
    #datastarApplied = false
    #mounted = false
    #internals?: ElementInternals
    #formContext?: SetupContext<InferProps<Defs>>
    #formPropsChanged?: () => void
    #pendingFormCallbacks: Array<
      (context: SetupContext<InferProps<Defs>>) => void
    > = []
    #renderQueued = false
    #reflecting = false
    #mountRoot?: ShadowRoot | HTMLElement
    #deferredApplyIgnore = false
    #hostTreeScoped = false
    #needsSignalPathBase = false
    #pendingInitialAttrValues = new Map<string, string>()
    #lightSlotNodes?: ChildNode[]
    #adoptedStyles: AdoptedStyle[] = []
    #rewriteScopedDataAttributes = (root: ParentNode) => {
      rewriteDataAttributes(root, {
        signalPathBase: this.#signalPathBase,
      })
      if (!this.#needsSignalPathBase) {
        this.#needsSignalPathBase = subtreeUsesScopedSignals(
          root,
          this.#signalPathBase,
        )
      }
    }
    #syncRefs = () => {
      const nextRefs: RefRecord = {}
      const roots =
        this.#mountRoot === this || !this.#mountRoot
          ? [this as ParentNode]
          : [this as ParentNode, this.#mountRoot]
      for (const root of roots) {
        const elements =
          root instanceof Element
            ? [
                ...(root.hasAttribute(rocketRefAttr) ? [root] : []),
                ...root.querySelectorAll(`[${rocketRefAttr}]`),
              ]
            : [...root.querySelectorAll(`[${rocketRefAttr}]`)]
        for (const element of elements) {
          const name = element.getAttribute(rocketRefAttr)
          if (name) nextRefs[name] = element
        }
      }
      for (const name of Object.keys(this.#refs)) {
        delete this.#refs[name]
      }
      Object.assign(this.#refs, nextRefs)
    }

    // Re-scope host-owned children after backend/DOM patch operations.
    //
    // Use the post-morph `datastar:scope-children` hook to rewrite patched host children before the engine mutation observer applies them.
    //
    // The rescoping surface depends on mode:
    // - `light` mode patches and renders directly on the host, so the host subtree must be rescoped and re-applied
    // - `open`/`closed` mode can patch host-side children, but the host element's own Datastar attributes must remain in outer page scope
    //
    // Without this pass, patched nodes can reach Datastar with raw `$$...` expressions and fail before Rocket can rescope them.
    #scopePatchedChildren = () => {
      if (this.#mountRoot === this) {
        this.#rewriteScopedDataAttributes(this)
        if (this.#needsSignalPathBase) {
          mergePaths([[this.#signalPathBase, {}]], { ifMissing: true })
        }

        // Host-side patches can replace mounted light-mode conditionals, so stale branch controllers must be removed before scanning the new content.
        //
        // Otherwise `initRocketConditionals()` can mistake the reused template head for an initialized branch and leave the patched branch inert.
        for (const cleanup of this.#conditionalCleanups) cleanup()
        this.#conditionalCleanups.length = 0
        this.#conditionalCleanups.push(
          ...initRocketStructures(this, {
            signalPathBase: this.#signalPathBase,
          }),
        )
        apply(this, false)
        this.#syncRefs()
      } else {
        // In shadow mode, rescope only host children because Datastar attributes on the custom element itself belong to the outer page scope.
        for (const child of this.children) {
          this.#rewriteScopedDataAttributes(child)
        }
        if (this.#needsSignalPathBase) {
          mergePaths([[this.#signalPathBase, {}]], { ifMissing: true })
        }
        for (const cleanup of this.#hostConditionalCleanups) cleanup()
        this.#hostConditionalCleanups.length = 0
        for (const child of this.children) {
          this.#hostConditionalCleanups.push(
            ...initRocketStructures(child, {
              signalPathBase: this.#signalPathBase,
            }),
          )
          if (!(child instanceof HTMLElement || child instanceof SVGElement)) {
            continue
          }
          apply(child, false)
        }
        this.#syncRefs()
      }
    }
    #html = hypertext(
      (string) => {
        const template = document.createElement('template')
        template.innerHTML = createHTML(string)
        return document.importNode(template.content, true)
      },
      (fragment) => {
        this.#rewriteScopedDataAttributes(fragment)
        return fragment as DocumentFragment
      },
    )
    #svg = hypertext(
      (string) => {
        const g = document.createElementNS('http://www.w3.org/2000/svg', 'g')
        g.innerHTML = createHTML(string)
        return g
      },
      (g) => {
        const fragment = document.createDocumentFragment()
        while (g.firstChild) fragment.appendChild(g.firstChild)
        this.#rewriteScopedDataAttributes(fragment)
        return fragment
      },
    )

    static get observedAttributes() {
      return [...attrToProp.keys()]
    }

    static formAssociated = formAssociated

    #withFormContext(
      callback: (context: SetupContext<InferProps<Defs>>) => void,
    ) {
      if (this.#formContext) callback(this.#formContext)
      else this.#pendingFormCallbacks.push(callback)
    }

    formAssociatedCallback(form: HTMLFormElement | null) {
      if (onFormAssociated)
        this.#withFormContext((context) => onFormAssociated(context, form))
    }

    formResetCallback() {
      if (onFormReset) this.#withFormContext(onFormReset)
    }

    formDisabledCallback(disabled: boolean) {
      if (onFormDisabled)
        this.#withFormContext((context) => onFormDisabled(context, disabled))
    }

    formStateRestoreCallback(
      state: string | File | FormData | null,
      mode: 'restore' | 'autocomplete',
    ) {
      if (onFormStateRestore)
        this.#withFormContext((context) =>
          onFormStateRestore(context, state, mode),
        )
    }

    get rocketInstanceId() {
      return this.#instanceId
    }

    get rocketSignalPath() {
      return this.#signalPathBase
    }

    constructor() {
      super()
      setMorphOwnership(this, ignoredByMorph)
      const instanceSegment = signalPathSegment(this.id)
      if (instanceSegment) {
        const instanceBase = /^[A-Za-z_$]/.test(instanceSegment)
          ? instanceSegment
          : `id_${instanceSegment}`
        const instanceKey = `${tagScope}.${instanceBase}`
        const instanceCount = (namedInstanceCounts.get(instanceKey) ?? 0) + 1
        namedInstanceCounts.set(instanceKey, instanceCount)
        this.#instanceId =
          instanceCount === 1
            ? instanceBase
            : `${instanceBase}_${instanceCount}`
      } else {
        this.#instanceId = `id${++nextInstanceId}`
      }
      this.#signalPathBase = `_rocket.${tagScope}.${this.#instanceId}`

      // Defer Datastar on the host subtree until Rocket setup has finished.
      //
      // Without this guard, Datastar can evaluate raw `$$...` expressions in host-provided children before Rocket scopes or projects them.
      //
      // Rocket removes the temporary flag after setup/render and manually applies the correct roots once the component is ready.
      //
      // Pre-upgrade hosts adopt and remove the marker in `connectedCallback()` because custom-element constructors cannot mutate attributes on new elements.

      // Opt every Rocket host into the patch-elements child-scoping hook.
      //
      // Mark the host so `patchElements.ts` dispatches its post-morph child-scoping hook here before Datastar applies patched children.
      //
      // Shadow roots do not isolate host-side children or slotted content from backend patches containing raw `$$...` expressions.
      // Scope pre-existing host children during upgrade so Datastar cannot evaluate raw `$$...` expressions first; shadow-mode host attributes remain in outer page scope.
      if (this.childNodes.length) {
        if (mode === 'light') {
          this.#rewriteScopedDataAttributes(this)
        } else {
          for (const child of this.children) {
            this.#rewriteScopedDataAttributes(child)
          }
        }
        this.#hostTreeScoped = true
      }

      // Decode pre-existing observed attributes now so `setup()` and the first render receive their final prop values.
      //
      // Cache them because the browser replays the same attributes through `attributeChangedCallback()` during upgrade, which would otherwise duplicate decoding side effects.
      for (const [attrName, propName] of attrToProp) {
        const rawValue = this.getAttribute(attrName)
        if (rawValue !== null) {
          this.#pendingInitialAttrValues.set(attrName, rawValue)
          this.#props[propName] = attributePropValue(propName, rawValue)
        }
      }

      // Replay pre-upgrade own-properties through Rocket's setters because they otherwise shadow the prototype accessors.
      for (const name of propNames) {
        if (!Object.prototype.hasOwnProperty.call(this, name)) continue
        const value = (this as any)[name]
        delete (this as any)[name]
        ;(this as any)[name] = value
      }
    }

    dispatchRocketAction(
      name: string,
      el: Element | null,
      evt: Event | undefined,
      cleanups: Map<string, () => void>,
      ...args: any[]
    ) {
      const action = this.#actions.get(name)
      if (action) {
        return action(
          {
            host: this as RocketHostWithProps<InferProps<Defs>>,
            props: this.#props,
            state: this.#state,
            el,
            evt,
          },
          ...args,
        )
      }

      // Walk the composed parent chain so nested Rocket children can call actions owned by an ancestor component before falling back globally.
      for (let node: Element | null = this; node; ) {
        const host = rocketHostsByElement.get(node)
        const root = node.getRootNode()
        if (host && host !== this) {
          return host.dispatchRocketAction(name, el, evt, cleanups, ...args)
        }
        node =
          node.parentElement ?? (root instanceof ShadowRoot ? root.host : null)
      }

      // Fall back to Datastar's global registry when `@dispatchRocket(...)` finds no local action so helpers such as `@intl(...)` keep working in Rocket templates.
      const globalAction = actions[name]
      if (globalAction) {
        const actionEl = (el ?? this) as HTMLOrSVG
        return globalAction(
          globalActionContext(actionEl, evt, name, cleanups),
          ...args,
        )
      }
      throw createError(
        actionErrorContext((el ?? this) as HTMLOrSVG, name),
        'UndefinedAction',
      )
    }

    #queueRender() {
      // Prop-driven rerenders are coalesced into one microtask per component.
      //
      // Coalesce multiple same-turn prop writes so Rocket renders once from the latest prop snapshot.
      //
      // This queue covers component rendering only; Datastar directives continue updating through their own effects.
      if (!this.#mounted || this.#renderQueued) return
      this.#renderQueued = true
      queueMicrotask(() => {
        this.#renderQueued = false
        if (this.#mounted) this.#render({})
      })
    }

    #indexActionHosts() {
      // Rocket action dispatch can originate from two different DOM surfaces:
      //
      // 1. the component's rendered mount root (light DOM or shadow DOM)
      // 2. host-provided light-DOM children outside an `open`/`closed` mount root that still contain Rocket-scoped expressions
      //
      // Index both surfaces so `@dispatchRocket(...)` can resolve the owner from the element being evaluated, but stop at nested Rocket hosts so a light-DOM parent cannot overwrite child ownership.
      rocketHostsByElement.set(this, this)
      for (const node of this.querySelectorAll('*')) {
        if (node.closest(`[${rocketHostAttr}]`) === this) {
          rocketHostsByElement.set(node, this)
        }
      }
      if (this.#mountRoot instanceof ShadowRoot) {
        for (const node of this.#mountRoot.querySelectorAll('*')) {
          if (!node.closest(`[${rocketHostAttr}]`)) {
            rocketHostsByElement.set(node, this)
          }
        }
      }
    }

    #setProp(
      name: keyof InferProps<Defs> & string,
      nextValue: InferProps<Defs>[typeof name],
    ) {
      if (Object.is(this.#props[name], nextValue)) return
      this.#props[name] = nextValue
      this.#formPropsChanged?.()
      const changes = { [name]: nextValue } as Partial<InferProps<Defs>>
      for (const observer of this.#propObservers) {
        if (observer.names === null || observer.names.has(name)) {
          observer.fn(this.#props, changes)
        }
      }

      // Notify prop observers synchronously, then optionally queue a render so a burst of writes still collapses into one render.
      if (
        typeof renderOnPropChange === 'function'
          ? renderOnPropChange({
              host: this as RocketHostWithProps<InferProps<Defs>>,
              props: this.#props,
              changes,
            })
          : renderOnPropChange !== false
      ) {
        this.#queueRender()
      }
    }

    #getDefaultProp(name: keyof InferProps<Defs> & string) {
      return this.#props[name]
    }

    #reflectPropAttribute(
      name: keyof InferProps<Defs> & string,
      encoded: string | null,
    ) {
      if (!booleanProps.has(name) && mode !== 'light' && !this.isConnected)
        return
      const attrName = kebab(name)
      this.#reflecting = true
      try {
        if (encoded == null) {
          this.removeAttribute(attrName)
        } else {
          this.setAttribute(attrName, encoded)
        }
      } finally {
        this.#reflecting = false
      }
    }

    #setDefaultProp(
      name: keyof InferProps<Defs> & string,
      value: InferProps<Defs>[typeof name],
    ) {
      const boolean = booleanProps.has(name)
      const normalized = boolean
        ? { value: Boolean(value), encoded: value ? '' : null }
        : normalizeProperty(propDefs[name], value)
      const nextValue = normalized.value as InferProps<Defs>[typeof name]
      this.#setProp(name, nextValue)
      this.#reflectPropAttribute(name, normalized.encoded)
    }

    #syncAdoptedStyles() {
      for (const style of this.#adoptedStyles) {
        if (
          style.root instanceof ShadowRoot &&
          'adoptedStyleSheets' in style.root &&
          'replaceSync' in CSSStyleSheet.prototype
        ) {
          const document = style.root.ownerDocument
          let sheets = sharedStyleSheets.get(document)
          if (!sheets) {
            sheets = new Map()
            sharedStyleSheets.set(document, sheets)
          }
          let sheet = sheets.get(style.text)
          if (!sheet) {
            const Sheet = (document.defaultView as Window & typeof globalThis)
              .CSSStyleSheet
            sheet = new Sheet()
            sheet.replaceSync(style.text)
            sheets.set(style.text, sheet)
          }
          style.sheet = sheet
          if (!style.root.adoptedStyleSheets.includes(style.sheet)) {
            style.root.adoptedStyleSheets = [
              ...style.root.adoptedStyleSheets,
              style.sheet,
            ]
          }
          continue
        }
        if (style.element?.parentNode === style.root) continue
        style.element = document.createElement('style')
        style.element.textContent = style.text
        style.root.prepend(style.element)
      }
    }

    #defineHostProp(name: string, descriptor: HostPropDescriptor) {
      const ownDescriptor = Object.getOwnPropertyDescriptor(this, name)
      const ownValue =
        ownDescriptor && 'value' in ownDescriptor
          ? ownDescriptor.value
          : undefined
      if (ownDescriptor) {
        delete (this as any)[name]
      }
      Object.defineProperty(this, name, {
        configurable: true,
        ...descriptor,
      })
      if (
        ownDescriptor &&
        'value' in ownDescriptor &&
        ownValue !== undefined &&
        typeof descriptor.set === 'function'
      ) {
        ;(this as any)[name] = ownValue
      }
    }

    #overrideProp<Name extends keyof InferProps<Defs> & string>(
      name: Name,
      getter?: PropOverrideGetter<InferProps<Defs>, Name>,
      setter?: PropOverrideSetter<InferProps<Defs>, Name>,
    ) {
      this.#defineHostProp(name, {
        get: getter
          ? () => getter(() => this.#getDefaultProp(name))
          : () => this.#getDefaultProp(name),
        set: setter
          ? (value: InferProps<Defs>[Name]) =>
              setter(value, (nextValue) =>
                this.#setDefaultProp(name, nextValue),
              )
          : (value: InferProps<Defs>[Name]) =>
              this.#setDefaultProp(name, value),
      })
    }

    #render(
      overrides: RenderContextOverrides<InferProps<Defs>>,
      ...args: any[]
    ) {
      if (!render) {
        this.#indexActionHosts()
        return
      }
      if (this.#datastarApplied) {
        for (const cleanup of this.#conditionalCleanups) cleanup()
        this.#conditionalCleanups.length = 0
      }
      const rendered = (
        render as (
          context: RenderContext<InferProps<Defs>>,
          ...args: any[]
        ) => RocketRenderValue
      )(
        {
          html: this.#html,
          svg: this.#svg,
          props: this.#props,
          host: this as RocketHostWithProps<InferProps<Defs>>,
          ...overrides,
        },
        ...args,
      )
      // Normalize every render result through one recursive path so top-level and nested primitives, Nodes, templates, and iterables behave consistently.
      const fragment = document.createDocumentFragment()
      appendComposedTemplateValue(fragment, rendered)
      if (this.#mountRoot === this) {
        const slots = [...fragment.querySelectorAll('slot')]
        if (slots.length) {
          // Light mode has no native slot projection, so treat `<slot>` elements as markers for moving and scoping original host children.
          //
          // Snapshot initial host children once so rerenders can reproject the same nodes without reparsing or duplicating them.
          //
          // Slot matching follows the platform shape closely enough to make the
          // authored markup unsurprising:
          // - <slot name="title"> receives host children with slot="title"
          // - <slot> receives host children with no slot attribute
          // - when no children match, the slot's own fallback children render
          //
          // Resolve slot markers before morphing so light DOM receives the flattened result instead of literal `<slot>` elements.
          if (!this.#lightSlotNodes) {
            const content = document.createDocumentFragment()
            content.append(...this.childNodes)
            rewriteDataAttributes(content, {
              signalPathBase: this.#signalPathBase,
            })
            this.#lightSlotNodes = [...content.childNodes]
          }
          const assigned = new Set<ChildNode>()
          for (const slot of slots) {
            const name = slot.getAttribute('name')
            const children = this.#lightSlotNodes.filter((node) => {
              if (assigned.has(node)) return false
              if (!(node instanceof Element))
                return name === null || name === ''
              const slotName = node.getAttribute('slot')
              return name === null || name === ''
                ? slotName === null || slotName === ''
                : slotName === name
            })
            if (children.length) {
              for (const child of children) assigned.add(child)
              slot.replaceWith(...children)
              continue
            }
            slot.replaceWith(...slot.childNodes)
          }
        }
      }
      morph(this.#mountRoot!, fragment, 'inner', true)
      this.#syncAdoptedStyles()
      if (this.#datastarApplied) {
        this.#conditionalCleanups.push(
          ...initRocketStructures(this.#mountRoot!, {
            signalPathBase: this.#signalPathBase,
          }),
        )
      }
      this.#syncRefs()
      this.#indexActionHosts()
    }

    connectedCallback() {
      if (this.#mounted) return
      this.#mounted = true
      this.addEventListener(
        DATASTAR_SCOPE_CHILDREN_EVENT,
        this.#scopePatchedChildren,
      )

      const neverApplied: HTMLOrSVG[] = []
      // Remove this host's child markers before scoping them while leaving independently pending nested Rocket hosts protected.
      for (const node of this.querySelectorAll(
        `[${rocketDeferredIgnoreAttr}]`,
      )) {
        if (node.getAttribute(rocketDeferredIgnoreAttr) !== tag) continue
        node.removeAttribute(rocketDeferredIgnoreAttr)
        node.removeAttribute(rocketIgnoreAttr)
        if (hiddenFromFirstApply.delete(node))
          neverApplied.push(node as HTMLOrSVG)
      }

      // Apply host markers here because custom-element constructors cannot mutate attributes on freshly created elements.
      if (this.hasAttribute(rocketDeferredIgnoreAttr)) {
        this.removeAttribute(rocketDeferredIgnoreAttr)
        this.#deferredApplyIgnore = true
      } else if (!this.hasAttribute(rocketIgnoreAttr)) {
        this.setAttribute(rocketIgnoreAttr, '')
        this.#deferredApplyIgnore = true
      }
      this.setAttribute('data-scope-children', '')
      this.setAttribute(rocketHostAttr, '')

      if (!this.#mountRoot) {
        this.#mountRoot =
          mode === 'light'
            ? this
            : (this.shadowRoot ??
              this.attachShadow({
                mode: mode === 'closed' ? 'closed' : 'open',
                delegatesFocus,
              }))
      }
      if (!this.#hostTreeScoped) {
        // Scope existing host children once before rendering so later apply passes cannot evaluate raw `$$` expressions; shadow-mode host bindings remain in page scope.
        if (this.#mountRoot === this) {
          this.#rewriteScopedDataAttributes(this)
        } else {
          for (const child of this.children) {
            this.#rewriteScopedDataAttributes(child)
          }
        }
        this.#hostTreeScoped = true
      }

      const localSignals = new Proxy(
        ((name, initialValue) => {
          if (!this.#mounted) return
          const path = `${this.#signalPathBase}.${name}`
          mergePaths(
            [
              [
                path,
                typeof initialValue === 'function'
                  ? datastarComputed(initialValue as () => unknown)
                  : initialValue,
              ],
            ],
            { ifMissing: true },
          )
          if (!Object.prototype.hasOwnProperty.call(this.#state, name)) {
            Object.defineProperty(this.#state, name, {
              configurable: true,
              enumerable: true,
              get: () => getPath(path),
              set: (value) => {
                mergePaths([[path, value]])
              },
            })
          }
          return getPath(path)
        }) as SetupSignal,
        {
          get: (target, name, receiver) => {
            if (typeof name !== 'string' || name in target) {
              return Reflect.get(target, name, receiver)
            }

            // Setup often reads local signals before Datastar has written them.
            //
            // Use `in` checks to subscribe through the proxy's reactive `has` trap without materializing missing Rocket paths as empty-string signals.
            return getReactiveScopedPath(`${this.#signalPathBase}.${name}`)
          },
          set: (_, name, value) => {
            if (typeof name !== 'string') return false
            if (!this.#mounted) return true
            const path = `${this.#signalPathBase}.${name}`
            if (!Object.prototype.hasOwnProperty.call(this.#state, name)) {
              Object.defineProperty(this.#state, name, {
                configurable: true,
                enumerable: true,
                get: () => getPath(path),
                set: (value) => {
                  mergePaths([[path, value]])
                },
              })
            }
            mergePaths([
              [
                path,
                typeof value === 'function'
                  ? datastarComputed(value as () => unknown)
                  : value,
              ],
            ])
            return true
          },
          has: (target, name) => {
            if (typeof name !== 'string') return name in target
            return (
              name in target ||
              hasReactiveScopedPath(`${this.#signalPathBase}.${name}`)
            )
          },
          ownKeys: (target) => [
            ...new Set([
              ...Reflect.ownKeys(target),
              ...Object.keys(
                getReactiveScopedPath<Record<string, any>>(
                  this.#signalPathBase,
                ) ?? {},
              ),
            ]),
          ],
          getOwnPropertyDescriptor: (target, name) => {
            if (Reflect.has(target, name)) {
              return Reflect.getOwnPropertyDescriptor(target, name)
            }
            if (
              typeof name === 'string' &&
              hasReactiveScopedPath(`${this.#signalPathBase}.${name}`)
            ) {
              return {
                configurable: true,
                enumerable: true,
                writable: true,
                value: getReactiveScopedPath(`${this.#signalPathBase}.${name}`),
              }
            }
          },
        },
      )
      const refs = new Proxy(
        {},
        {
          get: (_, name, receiver) =>
            typeof name === 'string'
              ? this.#refs[name]
              : Reflect.get({}, name, receiver),
          has: (_, name) => {
            if (typeof name !== 'string') return false
            return name in this.#refs
          },
          ownKeys: () => Object.keys(this.#refs),
          getOwnPropertyDescriptor: (_, name) => {
            if (typeof name === 'string' && name in this.#refs) {
              return {
                configurable: true,
                enumerable: true,
                writable: false,
                value: this.#refs[name],
              }
            }
          },
        },
      ) as Refs

      const host = this
      const context: SetupContext<InferProps<Defs>> = {
        props: this.#props,
        get internals() {
          return (host.#internals ??= host.attachInternals())
        },

        // `$` is the imperative setup alias for Datastar's global signal root.
        //
        // The name mirrors `$foo` expression syntax while remaining normal JavaScript property access as `$.foo`.
        $: root,

        // `$$` is the imperative setup alias for Rocket-local signals.
        //
        // The name mirrors `$$foo` template syntax while remaining a valid property/callable proxy inside `setup`.
        $$: localSignals,
        effect: (fn) => {
          const cleanup = effect(fn)
          this.#cleanups.push(cleanup)
          return cleanup
        },
        apply: (root, merge = true) => {
          apply(root, merge)
        },
        adoptStyles: (host, ...styles) => {
          const root =
            this.#mountRoot instanceof ShadowRoot ? this.#mountRoot : host
          for (const text of styles) {
            if (
              text &&
              !this.#adoptedStyles.some(
                (style) => style.root === root && style.text === text,
              )
            ) {
              this.#adoptedStyles.push({ root, text })
            }
          }
          this.#syncAdoptedStyles()
        },
        cleanup: (fn) => {
          this.#cleanups.push(fn)
        },
        emit: ((type: string, ...rest: any[]) => {
          if (!rest.length || typeof rest[0] === 'string') {
            for (const name of [type, ...rest]) {
              this.dispatchEvent(
                new Event(name, { bubbles: true, composed: true }),
              )
            }
            return
          }

          this.dispatchEvent(
            new CustomEvent(type, {
              bubbles: true,
              composed: true,
              ...(rest[1] ?? {}),
              detail: rest[0],
            }),
          )
        }) as SetupEmit,
        emitCancellable: ((type: string, ...rest: any[]) => {
          if (!rest.length) {
            return this.dispatchEvent(
              new Event(type, {
                bubbles: true,
                composed: true,
                cancelable: true,
              }),
            )
          }

          return this.dispatchEvent(
            new CustomEvent(type, {
              bubbles: true,
              composed: true,
              cancelable: true,
              ...(rest[1] ?? {}),
              detail: rest[0],
            }),
          )
        }) as SetupEmitCancellable,

        // Expose the global Datastar signal root inside `setup`.
        //
        // This references the shared reactive store directly rather than Rocket's private `_rocket` namespace.
        // Expose global Datastar actions as imperative methods inside `setup`.
        //
        // Use a component-owned cleanup map so teardown from global actions remains tied to the component lifecycle.
        actions: new Proxy(
          {},
          {
            get: (_, name: string | symbol) => {
              if (typeof name !== 'string') return
              return (...args: any[]) => {
                const action = actions[name]
                if (!action) {
                  throw createError(
                    actionErrorContext(this, name),
                    'UndefinedAction',
                  )
                }
                return action(
                  globalActionContext(
                    this,
                    undefined,
                    name,
                    this.#globalActionCleanups,
                  ),
                  ...args,
                )
              }
            },
          },
        ) as Record<string, (...args: any[]) => any>,
        action: (name, fn) => {
          this.#actions.set(name, fn)
        },
        observeProps: (fn, ...propNames) => {
          const registration = {
            fn,
            names: propNames.length === 0 ? null : new Set(propNames),
          }
          this.#propObservers.push(registration)
          return () => {
            const index = this.#propObservers.indexOf(registration)
            if (index >= 0) this.#propObservers.splice(index, 1)
          }
        },
        overrideProp: (name, getter, setter) => {
          this.#overrideProp(name, getter, setter)
        },
        defineHostProp: (name, descriptor) => {
          this.#defineHostProp(name, descriptor)
        },
        render: ((
          overrides: RenderContextOverrides<InferProps<Defs>>,
          ...args: any[]
        ) => {
          this.#render(overrides, ...args)
        }) as SetupRender<InferProps<Defs>>,
        host: this as RocketHostWithProps<InferProps<Defs>>,
      }

      setup?.(context)
      this.#render({})
      if (this.#needsSignalPathBase) {
        mergePaths([[this.#signalPathBase, {}]], { ifMissing: true })
      }
      if (this.#deferredApplyIgnore) {
        this.removeAttribute(rocketIgnoreAttr)
        this.#deferredApplyIgnore = false
      }

      // In shadow mode, apply only the host's Datastar attributes here because they belong to the outer page scope rather than Rocket's private scope.
      if (this.#mountRoot !== this) {
        applyElement(this)
      }

      // Apply only the component-owned render surface here.
      //
      // In `light` mode that surface is the host itself.
      // In shadow modes, re-applying the host would incorrectly rewrite outer-page bindings into Rocket's private signal path.
      apply(this.#mountRoot!, true)
      if (this.#mountRoot !== this && neverApplied.length) {
        queueMicrotask(() => {
          if (!this.isConnected) return
          for (const node of neverApplied) {
            if (node.parentNode === this) applyMissing(node)
          }
        })
      }
      this.#datastarApplied = true
      this.#conditionalCleanups.push(
        ...initRocketStructures(this.#mountRoot!, {
          signalPathBase: this.#signalPathBase,
        }),
      )
      this.#syncRefs()
      const refsObserver = new MutationObserver(() => this.#syncRefs())
      refsObserver.observe(this.#mountRoot!, {
        childList: true,
        subtree: true,
      })
      if (this.#mountRoot !== this) {
        refsObserver.observe(this, {
          childList: true,
          subtree: true,
        })
      }
      this.#cleanups.push(() => refsObserver.disconnect())
      this.#indexActionHosts()
      onFirstRender?.(
        Object.defineProperties(
          { refs: refs as unknown as InstancesOf<Refs> },
          Object.getOwnPropertyDescriptors(context),
        ) as FirstUpdateContext<InferProps<Defs>, Refs>,
      )
      if (formAssociated) this.#formContext = context
      for (const callback of this.#pendingFormCallbacks.splice(0))
        callback(context)
      if (formAssociated && formValue !== false) {
        const revision = signal(0)
        let nextRevision = 0
        this.#formPropsChanged = () => revision(++nextRevision)
        context.effect(() => {
          revision()
          const value =
            typeof formValue === 'function'
              ? formValue(context)
              : propNames.includes('value')
                ? this.#props.value
                : null
          let submitted: string | File | FormData | null
          if (value == null) submitted = null
          else if (
            ['string', 'number', 'boolean', 'bigint'].includes(typeof value)
          )
            submitted = String(value)
          else if (value instanceof File || value instanceof FormData)
            submitted = value
          else
            throw new TypeError(
              `Rocket ${tag}: use formValue to serialize structured form values.`,
            )
          context.internals.setFormValue(submitted)
        })
      }
    }

    connectedMoveCallback() {}

    attributeChangedCallback(
      name: string,
      oldValue: string | null,
      newValue: string | null,
    ) {
      if (this.#reflecting) return
      const propName = attrToProp.get(name)
      if (!propName) return
      if (
        newValue !== null &&
        oldValue === null &&
        this.#pendingInitialAttrValues.get(name) === newValue
      ) {
        this.#pendingInitialAttrValues.delete(name)
        return
      }
      this.#setProp(propName, attributePropValue(propName, newValue))
    }

    disconnectedCallback() {
      this.#formPropsChanged = undefined
      const errors: unknown[] = []
      const release = (cleanup: () => void) => {
        try {
          cleanup()
        } catch (error) {
          errors.push(error)
        }
      }
      this.removeEventListener(
        DATASTAR_SCOPE_CHILDREN_EVENT,
        this.#scopePatchedChildren,
      )
      // Dispose dependent effects and listeners before removing their signals.
      if (this.#mountRoot) release(() => cleanupTree(this.#mountRoot!))
      for (const cleanup of this.#globalActionCleanups.values())
        release(cleanup)
      this.#globalActionCleanups.clear()
      for (const cleanup of this.#hostConditionalCleanups) release(cleanup)
      this.#hostConditionalCleanups.length = 0
      for (const cleanup of this.#conditionalCleanups) release(cleanup)
      this.#conditionalCleanups.length = 0
      for (const cleanup of this.#cleanups) release(cleanup)
      this.#cleanups.length = 0
      for (const name of Object.keys(this.#refs)) delete this.#refs[name]
      this.#propObservers = []
      this.#actions.clear()
      if (this.#mountRoot) forgetRoot(this.#mountRoot)
      this.#mounted = false
      release(() => mergePaths([[this.#signalPathBase, null]]))
      if (errors.length) {
        throw new AggregateError(errors, 'Rocket disconnect cleanup failed')
      }
    }
    static {
      for (const name of propNames) {
        Object.defineProperty(RocketElement.prototype, name, {
          get(this: RocketElement) {
            return this.#getDefaultProp(name)
          },
          set(this: RocketElement, value: InferProps<Defs>[typeof name]) {
            this.#setDefaultProp(name, value)
          },
        })
      }
    }
  }

  Object.defineProperty(RocketElement, 'manifest', {
    configurable: false,
    enumerable: false,
    value: () =>
      cloneRocketManifestComponent(
        rocketManifestRegistry.get(tag) ?? componentManifest,
      ),
  })
  customElements.define(tag, RocketElement)
  rocketManifestRegistry.set(tag, componentManifest)
  return RocketElement
}
export type { RocketDefinition, RocketRenderValue }
