import { Hash, elUniqId } from '../utils/dom'
import { camelize } from '../utils/text'
import { debounce } from '../utils/timing'
import { effect } from '../vendored/preact-core'
import { DSP, DSS, VERSION } from './consts'
import { initErr, runtimeErr } from './errors'
import { SignalsRoot } from './signals'
import {
  type ActionPlugin,
  type ActionPlugins,
  type AttributePlugin,
  type DatastarPlugin,
  type GlobalInitializer,
  type HTMLorSVGElement,
  type InitContext,
  type OnRemovalFn,
  PluginType,
  Requirement,
  type RuntimeContext,
  type RuntimeExpressionFunction,
  type WatcherPlugin,
} from './types'

const removalKey = (k: string, v: string) => `${k}${DSP}${v}`

export class Engine {
  #signals = new SignalsRoot()
  #plugins: AttributePlugin[] = []
  #actions: ActionPlugins = {}
  #watchers: WatcherPlugin[] = []

  // Map of cleanup functions by element, keyed by the raw dataset key and value
  #removals = new Map<Element, Map<string, OnRemovalFn>>()

  constructor() {
    const dsPrefix = 'data-'

    const ob = new MutationObserver((mutations) => {
      for (const {
        target,
        type,
        attributeName,
        oldValue,
        addedNodes,
        removedNodes,
      } of mutations) {
        switch (type) {
          case 'childList':
            {
              for (const node of removedNodes) {
                const el = node as HTMLorSVGElement
                const elRemovals = this.#removals.get(el)
                if (!elRemovals) continue

                for (const [_, removalFn] of elRemovals) {
                  removalFn()
                }
                this.#removals.delete(el)
              }

              for (const node of addedNodes) {
                const el = node as HTMLorSVGElement
                this.#apply(el)
              }
            }
            break
          case 'attributes': {
            {
              if (!attributeName?.startsWith(dsPrefix)) {
                break
              }

              const el = target as HTMLorSVGElement
              const rawKey = camelize(attributeName.slice(dsPrefix.length))

              // If the value is not null and has changed, cleanup the old value
              if (oldValue !== null && el.dataset[rawKey] !== oldValue) {
                const elRemovals = this.#removals.get(el)
                if (elRemovals) {
                  const rk = removalKey(rawKey, oldValue)
                  const removalFn = elRemovals.get(rk)
                  if (removalFn) {
                    removalFn()
                    elRemovals.delete(rk)
                  }
                }
              }

              this.#applyAttributePlugin(el, rawKey)
            }
            break
          }
        }
      }
    })

    ob.observe(document.body, {
      attributes: true,
      attributeOldValue: true,
      childList: true,
      subtree: true,
    })
  }

  get signals() {
    return this.#signals
  }

  get version() {
    return VERSION
  }

  public load(...pluginsToLoad: DatastarPlugin[]) {
    for (const plugin of pluginsToLoad) {
      const that = this // I hate javascript
      const ctx: InitContext = {
        get signals() {
          return that.#signals
        },
        effect: (cb: () => void): OnRemovalFn => effect(cb),
        actions: this.#actions,
        plugin,
      }

      let globalInitializer: GlobalInitializer | undefined
      switch (plugin.type) {
        case PluginType.Watcher: {
          const wp = plugin as WatcherPlugin
          this.#watchers.push(wp)
          globalInitializer = wp.onGlobalInit
          break
        }
        case PluginType.Action: {
          this.#actions[plugin.name] = plugin as ActionPlugin
          break
        }
        case PluginType.Attribute: {
          const ap = plugin as AttributePlugin
          this.#plugins.push(ap)
          globalInitializer = ap.onGlobalInit
          break
        }
        default: {
          throw initErr('InvalidPluginType', ctx)
        }
      }
      if (globalInitializer) {
        globalInitializer(ctx)
      }
    }

    // Sort attribute plugins by descending length then alphabetically
    this.#plugins.sort((a, b) => {
      const lenDiff = b.name.length - a.name.length
      if (lenDiff !== 0) return lenDiff
      return a.name.localeCompare(b.name)
    })

    this.#debouncedApply()
  }

  #debouncedApply = debounce(() => {
    this.#apply(document.body)
  }, 1)

  // Apply all plugins to the element and its children
  #apply(rootElement: Element) {
    this.#walkDownDOM(rootElement, (el) => {
      // Cleanup any existing removal functions
      const elRemovals = this.#removals.get(el)
      if (elRemovals) {
        for (const [, removalFn] of elRemovals) {
          removalFn()
        }
        this.#removals.delete(el)
      }

      // Apply the plugins to the element in order of application
      // since DOMStringMap is ordered, we can be deterministic
      for (const rawKey of Object.keys(el.dataset)) {
        this.#applyAttributePlugin(el, rawKey)
      }
    })
  }

  #applyAttributePlugin(el: HTMLorSVGElement, rawKey: string) {
    // Find the plugin that matches, since the plugins are sorted by length descending and alphabetically
    // the first match will be the most specific
    const plugin = this.#plugins.find((p) => rawKey.startsWith(p.name))

    // Skip if no plugin is found
    if (!plugin) return

    // Ensure the element has an id
    if (!el.id.length) el.id = elUniqId(el)

    // Extract the key and value from the dataset
    let [key, ...rawModifiers] = rawKey.slice(plugin.name.length).split(/\_\_+/)

    const hasKey = key.length > 0
    if (hasKey) {
      // Keys starting with a dash are not converted to camel case in the dataset
      const keySlice1 = key.slice(1)
      key = key.startsWith('-') ? keySlice1 : key[0].toLowerCase() + keySlice1
    }
    const value = el.dataset[rawKey] || ''
    const hasValue = value.length > 0

    // Create the runtime context
    const that = this // I hate javascript
    const ctx: RuntimeContext = {
      get signals() {
        return that.#signals
      },
      effect: (cb: () => void): OnRemovalFn => effect(cb),
      actions: this.#actions,
      genRX: () => this.#genRX(ctx, ...(plugin.argNames || [])),
      plugin,
      el,
      rawKey,
      key,
      value,
      mods: new Map(),
    }

    // Check the requirements
    const keyReq = plugin.keyReq || Requirement.Allowed
    if (hasKey) {
      if (keyReq === Requirement.Denied) {
        throw runtimeErr(`${plugin.name}KeyNotAllowed`, ctx)
      }
    } else if (keyReq === Requirement.Must) {
      throw runtimeErr(`${plugin.name}KeyRequired`, ctx)
    }
    const valReq = plugin.valReq || Requirement.Allowed
    if (hasValue) {
      if (valReq === Requirement.Denied) {
        throw runtimeErr(`${plugin.name}ValueNotAllowed`, ctx)
      }
    } else if (valReq === Requirement.Must) {
      throw runtimeErr(`${plugin.name}ValueRequired`, ctx)
    }

    // Check for exclusive requirements
    if (keyReq === Requirement.Exclusive || valReq === Requirement.Exclusive) {
      if (hasKey && hasValue) {
        throw runtimeErr(`${plugin.name}KeyAndValueProvided`, ctx)
      }
      if (!hasKey && !hasValue) {
        throw runtimeErr(`${plugin.name}KeyOrValueRequired`, ctx)
      }
    }

    for (const rawMod of rawModifiers) {
      const [label, ...mod] = rawMod.split('.')
      ctx.mods.set(camelize(label), new Set(mod.map((t) => t.toLowerCase())))
    }

    // Load the plugin and store any cleanup functions
    const removalFn = plugin.onLoad(ctx)
    if (removalFn) {
      let elRemovals = this.#removals.get(el)
      if (!elRemovals) {
        elRemovals = new Map()
        this.#removals.set(el, elRemovals)
      }
      elRemovals.set(removalKey(rawKey, value), removalFn)
    }

    // Remove the attribute if required
    if (plugin?.removeOnLoad) delete el.dataset[rawKey]
  }

  #genRX(
    ctx: RuntimeContext,
    ...argNames: string[]
  ): RuntimeExpressionFunction {
    let userExpression = ''

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
    //
    // We also want to match the non delimiter part of statements
    // note we only support ; statement delimiters:
    //
    // [^;]
    //
    const statementRe =
      /(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm
    const statements = ctx.value.trim().match(statementRe)
    if (statements) {
      const lastIdx = statements.length - 1
      const last = statements[lastIdx].trim()
      if (!last.startsWith('return')) {
        statements[lastIdx] = `return (${last});`
      }
      userExpression = statements.join(';\n')
    }

    // Ignore any escaped values
    const escaped = new Map<string, string>()
    const escapeRe = new RegExp(`(?:${DSP})(.*?)(?:${DSS})`, 'gm')
    for (const match of userExpression.matchAll(escapeRe)) {
      const k = match[1]
      const v = new Hash('dsEscaped').with(k).value
      escaped.set(v, k)
      userExpression = userExpression.replace(DSP + k + DSS, v)
    }

    const fnCall = /@(\w*)\(/gm
    const matches = userExpression.matchAll(fnCall)
    const methodsCalled = new Set<string>()
    for (const match of matches) {
      methodsCalled.add(match[1])
    }

    // Replace any action calls
    const actionsRe = new RegExp(
      `@(${Object.keys(this.#actions).join('|')})\\(`,
      'gm',
    )

    // Add ctx to action calls
    userExpression = userExpression.replaceAll(
      actionsRe,
      'ctx.actions.$1.fn(ctx,',
    )

    // Replace any signal calls
    const signalNames = ctx.signals.paths()
    if (signalNames.length) {
      // Match any valid `$signalName` followed by a non-word character or end of string
      const signalsRe = new RegExp(`\\$(${signalNames.join('|')})(\\W|$)`, 'gm')
      userExpression = userExpression.replaceAll(
        signalsRe,
        `ctx.signals.signal('$1').value$2`,
      )
    }

    // Replace any escaped values
    for (const [k, v] of escaped) {
      userExpression = userExpression.replace(k, v)
    }

    const fnContent = `return (()=> {\n${userExpression}\n})()` // Wrap in IIFE
    ctx.fnContent = fnContent

    try {
      const fn = new Function('ctx', ...argNames, fnContent)
      return (...args: any[]) => {
        try {
          return fn(ctx, ...args)
        } catch (error: any) {
          throw runtimeErr('ExecuteExpression', ctx, {
            error: error.message,
          })
        }
      }
    } catch (error: any) {
      throw runtimeErr('GenerateExpression', ctx, {
        error: error.message,
      })
    }
  }

  #walkDownDOM(
    element: Element | null,
    callback: (el: HTMLorSVGElement) => void,
  ) {
    if (
      !element ||
      !(element instanceof HTMLElement || element instanceof SVGElement)
    ) {
      return null
    }
    const dataset = element.dataset
    if ('starIgnore' in dataset) {
      return null
    }
    if (!('starIgnore__self' in dataset)) {
      callback(element)
    }
    let el = element.firstElementChild
    while (el) {
      this.#walkDownDOM(el, callback)
      el = el.nextElementSibling
    }
  }
}
