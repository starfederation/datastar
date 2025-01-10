import { Hash, elUniqId } from '../utils/dom'
import { camelize } from '../utils/text'
import { effect } from '../vendored/preact-core'
import { DSP, DSS, VERSION } from './consts'
import { initErr, runtimeErr } from './errors'
import { SignalsRoot } from './nestedSignals'
import {
  type ActionPlugin,
  type ActionPlugins,
  type AttributePlugin,
  type DatastarPlugin,
  type GlobalInitializer,
  type HTMLorSVGElement,
  type InitContext,
  type MacroPlugin,
  type OnRemovalFn,
  PluginType,
  type RemovalEntry,
  Requirement,
  type RuntimeContext,
  type RuntimeExpressionFunction,
  type WatcherPlugin,
} from './types'

export class Engine {
  #signals = new SignalsRoot()
  #plugins: AttributePlugin[] = []
  #macros: MacroPlugin[] = []
  #actions: ActionPlugins = {}
  #watchers: WatcherPlugin[] = []
  #removals = new Map<Element, RemovalEntry>()

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
        apply: this.apply.bind(this),
        cleanup: this.#cleanup.bind(this),
        plugin,
      }

      let globalInitializer: GlobalInitializer | undefined
      switch (plugin.type) {
        case PluginType.Macro: {
          this.#macros.push(plugin as MacroPlugin)
          break
        }
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
    this.apply(document.body)
  }

  // Apply all plugins to the element and its children
  public apply(rootElement: Element) {
    const appliedMacros = new Set<MacroPlugin>()
    this.#plugins.forEach((plugin, pi) => {
      this.#walkDownDOM(rootElement, (el) => {
        // Ignore this element if `data-star-ignore` exists on it
        if ('starIgnore' in el.dataset) return

        // Cleanup if not first plugin
        if (!pi) this.#cleanup(el)

        for (const rawKey in el.dataset) {
          // Check if the key is relevant to the plugin
          if (!rawKey.startsWith(plugin.name)) continue

          // Extract the key and value from the dataset
          const keyRaw = rawKey.slice(plugin.name.length)
          let [key, ...rawModifiers] = keyRaw.split(/\_\_+/)

          const hasKey = key.length > 0
          if (hasKey) {
            // Keys starting with a dash are not converted to camel case in the dataset
            if (key.startsWith('-')) {
              key = key.slice(1)
            } else {
              key = key[0].toLowerCase() + key.slice(1)
            }
          }
          const rawValue = `${el.dataset[rawKey]}` || ''
          const hasValue = rawValue.length > 0

          // Create the runtime context
          const that = this // I hate javascript
          const ctx: RuntimeContext = {
            get signals() {
              return that.#signals
            },
            effect: (cb: () => void): OnRemovalFn => effect(cb),
            apply: that.apply.bind(that),
            cleanup: that.#cleanup.bind(that),
            actions: that.#actions,
            genRX: () => this.#genRX(ctx, ...(plugin.argNames || [])),
            plugin,
            el,
            rawKey,
            rawValue,
            key,
            value: rawValue,
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
          if (
            keyReq === Requirement.Exclusive ||
            valReq === Requirement.Exclusive
          ) {
            if (hasKey && hasValue) {
              throw runtimeErr(`${plugin.name}KeyAndValueProvided`, ctx)
            }
            if (!hasKey && !hasValue) {
              throw runtimeErr(`${plugin.name}KeyOrValueRequired`, ctx)
            }
          }

          // Ensure the element has an id
          if (!el.id.length) el.id = elUniqId(el)

          for (const rawMod of rawModifiers) {
            const [label, ...mod] = rawMod.split('.')
            ctx.mods.set(
              camelize(label),
              new Set(mod.map((t) => t.toLowerCase())),
            )
          }

          // Apply the macros
          appliedMacros.clear()

          const macros = [
            ...(plugin.macros?.pre || []),
            ...this.#macros,
            ...(plugin.macros?.post || []),
          ]
          for (const macro of macros) {
            if (appliedMacros.has(macro)) continue
            appliedMacros.add(macro)
            ctx.value = macro.fn(ctx, ctx.value)
          }

          // Load the plugin and store any cleanup functions
          const removal = plugin.onLoad(ctx)
          if (removal) {
            if (!this.#removals.has(el)) {
              this.#removals.set(el, {
                id: el.id,
                set: new Set(),
              })
            }
            this.#removals.get(el)?.set.add(removal)
          }

          // Remove the attribute if required
          if (plugin?.removeOnLoad) delete el.dataset[rawKey]
        }
      })
    })
  }

  #genRX(
    ctx: RuntimeContext,
    ...argNames: string[]
  ): RuntimeExpressionFunction {
    const stmts = ctx.value
      .split(/;|\n/)
      .map((s) => s.trim())
      .filter((s) => s !== '')
    const lastIdx = stmts.length - 1
    const last = stmts[lastIdx]
    if (!last.startsWith('return')) {
      stmts[lastIdx] = `return (${stmts[lastIdx]});`
    }
    let userExpression = stmts.join(';\n').trim()

    // Ingore any escaped values
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
          throw runtimeErr('RunExpr', ctx, {
            error: error.message,
          })
        }
      }
    } catch (error: any) {
      throw runtimeErr('GenExpr', ctx, {
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
    )
      return null
    callback(element)
    let el = element.firstElementChild
    while (el) {
      this.#walkDownDOM(el, callback)
      el = el.nextElementSibling
    }
  }

  // Clenup all plugins associated with the element
  #cleanup(element: Element) {
    const removalSet = this.#removals.get(element)
    if (removalSet) {
      for (const removal of removalSet.set) {
        removal()
      }
      this.#removals.delete(element)
    }
  }
}
