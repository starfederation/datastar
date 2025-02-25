import { Hash, attrHash, elUniqId, walkDOM } from '../utils/dom'
import { camel } from '../utils/text'
import { effect } from '../vendored/preact-core'
import { DSP, DSS } from './consts'
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

const signals: SignalsRoot = new SignalsRoot()
const plugins: AttributePlugin[] = []
const actions: ActionPlugins = {}
const watchers: WatcherPlugin[] = []

let aliasPrefix = ''
export function setAlias(prefix: string) {
  aliasPrefix = prefix
}
let mutationObserver: MutationObserver | null = null

// Map of cleanup functions by element, keyed by the dataset key and value
const removals = new Map<Element, Map<number, OnRemovalFn>>()

export function load(...pluginsToLoad: DatastarPlugin[]) {
  for (const plugin of pluginsToLoad) {
    const ctx: InitContext = {
      signals,
      effect: (cb: () => void): OnRemovalFn => effect(cb),
      actions: actions,
      plugin,
      apply,
    }

    let globalInitializer: GlobalInitializer | undefined
    switch (plugin.type) {
      case PluginType.Watcher: {
        const wp = plugin as WatcherPlugin
        watchers.push(wp)
        globalInitializer = wp.onGlobalInit
        break
      }
      case PluginType.Action: {
        actions[plugin.name] = plugin as ActionPlugin
        break
      }
      case PluginType.Attribute: {
        const ap = plugin as AttributePlugin
        plugins.push(ap)
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
  plugins.sort((a, b) => {
    const lenDiff = b.name.length - a.name.length
    if (lenDiff !== 0) return lenDiff
    return a.name.localeCompare(b.name)
  })
}

// Apply all plugins to the element and its children
export function apply(
  rootElement: HTMLorSVGElement = document.documentElement,
) {
  walkDOM(rootElement, (el) => {
    // Check if the element has any data attributes already
    const toApply = new Array<string>()
    const elCleanups = removals.get(el) || new Map()
    const toCleanup = new Map<number, OnRemovalFn>([...elCleanups])
    const hashes = new Map<string, number>()

    // Apply the plugins to the element in order of application
    // since DOMStringMap is ordered, we can be deterministic
    for (const datasetKey of Object.keys(el.dataset)) {
      // Ignore data attributes that don’t start with the alias prefix
      if (!datasetKey.startsWith(aliasPrefix)) {
        break
      }

      const datasetValue = el.dataset[datasetKey] || ''
      const currentHash = attrHash(datasetKey, datasetValue)
      hashes.set(datasetKey, currentHash)

      // If the hash hasn't changed, ignore
      // otherwise keep the old cleanup and add new to applys
      if (elCleanups.has(currentHash)) {
        toCleanup.delete(currentHash)
      } else {
        toApply.push(datasetKey)
      }
    }

    // Clean up any old plugins and apply the new ones
    for (const [_, cleanup] of toCleanup) cleanup()
    for (const key of toApply) {
      const h = hashes.get(key)!
      applyAttributePlugin(el, key, h)
    }
  })

  observe()
}

// Set up a mutation observer to run plugin removal and apply functions
function observe() {
  if (mutationObserver) {
    return
  }

  mutationObserver = new MutationObserver((mutations) => {
    const toRemove = new Set<HTMLorSVGElement>()
    const toApply = new Set<HTMLorSVGElement>()
    for (const { target, type, addedNodes, removedNodes } of mutations) {
      switch (type) {
        case 'childList':
          {
            for (const node of removedNodes) {
              toRemove.add(node as HTMLorSVGElement)
            }
            for (const node of addedNodes) {
              toApply.add(node as HTMLorSVGElement)
            }
          }
          break
        case 'attributes': {
          toApply.add(target as HTMLorSVGElement)

          break
        }
      }
    }
    for (const el of toRemove) {
      const elTracking = removals.get(el)
      if (elTracking) {
        for (const [h, cleanup] of elTracking) {
          cleanup()
          elTracking.delete(h)
        }
        if (elTracking.size === 0) {
          removals.delete(el)
        }
      }
    }
    for (const el of toApply) {
      apply(el)
    }
  })

  mutationObserver.observe(document.body, {
    attributes: true,
    attributeOldValue: true,
    childList: true,
    subtree: true,
  })
}

function applyAttributePlugin(
  el: HTMLorSVGElement,
  camelCasedKey: string,
  hash: number,
) {
  // Extract the raw key from the dataset
  const rawKey = camel(camelCasedKey.slice(aliasPrefix.length))

  // Find the plugin that matches, since the plugins are sorted by length descending and alphabetically. The first match will be the most specific.
  const plugin = plugins.find((p) => rawKey.startsWith(p.name))

  // Skip if no plugin is found
  if (!plugin) return

  // Ensure the element has an id
  if (!el.id.length) el.id = elUniqId(el)

  // Extract the key and modifiers
  let [key, ...rawModifiers] = rawKey.slice(plugin.name.length).split(/\_\_+/)

  const hasKey = key.length > 0
  if (hasKey) {
    key = camel(key)
  }
  const value = el.dataset[camelCasedKey] || ''
  const hasValue = value.length > 0

  // Create the runtime context
  const ctx: RuntimeContext = {
    signals,
    apply,
    effect: (cb: () => void): OnRemovalFn => effect(cb),
    actions: actions,
    genRX: () => genRX(ctx, ...(plugin.argNames || [])),
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
    ctx.mods.set(camel(label), new Set(mod.map((t) => t.toLowerCase())))
  }

  // Load the plugin and store any cleanup functions
  const cleanup = plugin.onLoad(ctx)
  if (cleanup) {
    let elTracking = removals.get(el)
    if (!elTracking) {
      elTracking = new Map()
      removals.set(el, elTracking)
    }
    elTracking.set(hash, cleanup)
  }
}

function genRX(
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
    const v = new Hash('dsEscaped').with(k).string
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
  const actionsRe = new RegExp(`@(${Object.keys(actions).join('|')})\\(`, 'gm')

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
