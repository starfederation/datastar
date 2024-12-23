import { elUniqId } from '~/utils/dom'
import { camelize } from '~/utils/text'
import { effect } from '~/vendored/preact-core'
import { VERSION } from './consts'
import { dsErr } from './errors'
import { SignalsRoot } from './nestedSignals'
import {
  type ActionPlugin,
  type ActionPlugins,
  type AttributePlugin,
  type DatastarPlugin,
  type GlobalInitializer,
  type HTMLorSVGElement,
  type MacroPlugin,
  type Modifiers,
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
          throw dsErr('InvalidPluginType', {
            name: plugin.name,
            type: plugin.type,
          })
        }
      }
      if (globalInitializer) {
        const that = this // I hate javascript
        globalInitializer({
          get signals() {
            return that.#signals
          },
          effect: (cb: () => void): OnRemovalFn => effect(cb),
          actions: this.#actions,
          apply: this.apply.bind(this),
          cleanup: this.#cleanup.bind(this),
        })
      }
    }
    this.apply(document.body)
  }

  // Apply all plugins to the element and its children
  public apply(rootElement: Element) {
    const appliedMacros = new Set<MacroPlugin>()
    this.#plugins.forEach((p, pi) => {
      this.#walkDownDOM(rootElement, (el) => {
        // Ignore this element if `data-star-ignore` exists on it
        if ('starIgnore' in el.dataset) return

        // Cleanup if not first plugin
        if (!pi) this.#cleanup(el)

        for (const rawKey in el.dataset) {
          // Check if the key is relevant to the plugin
          if (!rawKey.startsWith(p.name)) continue

          // Extract the key and value from the dataset
          const keyRaw = rawKey.slice(p.name.length)
          let [key, ...rawTags] = keyRaw.split(/\_\_+/)

          const hasKey = key.length > 0
          if (hasKey) {
            // Keys starting with an underscore are not converted to camel case in the dataset
            if (key.startsWith('-_')) {
              key = key.slice(1)
            } else {
              key = key[0].toLowerCase() + key.slice(1)
            }
          }
          const rawValue = `${el.dataset[rawKey]}` || ''
          let value = rawValue
          const hasValue = value.length > 0

          // Check the requirements
          const keyReq = p.keyReq || Requirement.Allowed
          if (hasKey) {
            if (keyReq === Requirement.Denied) {
              throw dsErr(`${p.name}KeyNotAllowed`, { key })
            }
          } else if (keyReq === Requirement.Must) {
            throw dsErr(`${p.name}KeyRequired`)
          }
          const valReq = p.valReq || Requirement.Allowed
          if (hasValue) {
            if (valReq === Requirement.Denied) {
              throw dsErr(`${p.name}ValueNotAllowed`, { value })
            }
          } else if (valReq === Requirement.Must) {
            throw dsErr(`${p.name}ValueRequired`)
          }

          // Check for exclusive requirements
          if (
            keyReq === Requirement.Exclusive ||
            valReq === Requirement.Exclusive
          ) {
            if (hasKey && hasValue) {
              throw dsErr(`${p.name}KeyAndValueProvided`)
            }
            if (!hasKey && !hasValue) {
              throw dsErr(`${p.name}KeyOrValueRequired`)
            }
          }

          // Ensure the element has an id
          if (!el.id.length) el.id = elUniqId(el)

          // Apply the macros
          appliedMacros.clear()
          const tags: Modifiers = new Map<string, Set<string>>()
          for (const rawTag of rawTags) {
            const [label, ...tag] = rawTag.split('.')
            tags.set(camelize(label), new Set(tag.map((t) => t.toLowerCase())))
          }
          const macros = [
            ...(p.macros?.pre || []),
            ...this.#macros,
            ...(p.macros?.post || []),
          ]
          for (const macro of macros) {
            if (appliedMacros.has(macro)) continue
            appliedMacros.add(macro)
            value = macro.fn(value)
          }

          // Create the runtime context
          const that = this // I hate javascript
          const ctx = {
            get signals() {
              return that.#signals
            },
            effect: (cb: () => void): OnRemovalFn => effect(cb),
            apply: that.apply.bind(that),
            cleanup: that.#cleanup.bind(that),
            actions: that.#actions,
            genRX: () => this.#genRX(ctx, ...(p.argNames || [])),
            el,
            rawKey,
            rawValue,
            key,
            value,
            tags,
          }

          // Load the plugin and store any cleanup functions
          const removal = p.onLoad(ctx)
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
          if (p?.removeOnLoad) delete el.dataset[rawKey]
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
    const userExpression = stmts.join('\n')

    const fnCall = /(\w*)\(/gm
    const matches = userExpression.matchAll(fnCall)
    const methodsCalled = new Set<string>()
    for (const match of matches) {
      methodsCalled.add(match[1])
    }
    // Action names
    const an = Object.keys(this.#actions).filter((i) => methodsCalled.has(i))
    // Action lines
    const al = an.map((a) => `const ${a} = ctx.actions.${a}.fn;`)
    const fnContent = `${al.join('\n')}return (()=> {${userExpression}})()`

    // Add ctx to action calls
    let fnWithCtx = fnContent.trim()
    for (const a of an) {
      fnWithCtx = fnWithCtx.replaceAll(`${a}(`, `${a}(ctx,`)
    }

    try {
      const argumentNames = argNames || []
      const fn = new Function('ctx', ...argumentNames, fnWithCtx)
      return (...args: any[]) => fn(ctx, ...args)
    } catch (error) {
      throw dsErr('GeneratingExpressionFailed', {
        error,
        fnContent,
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
