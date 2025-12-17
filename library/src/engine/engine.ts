import { DATASTAR_FETCH_EVENT, DSP, DSS } from '@engine/consts'
import { root } from '@engine/signals'
import type {
  ActionContext,
  ActionPlugin,
  AttributeContext,
  AttributePlugin,
  DatastarFetchEvent,
  HTMLOrSVG,
  Modifiers,
  Requirement,
  WatcherPlugin,
} from '@engine/types'
import { isHTMLOrSVG } from '@utils/dom'
import { aliasify, snake } from '@utils/text'

const url = 'https://data-star.dev/errors'

const error = (
  ctx: Record<string, any>,
  reason: string,
  metadata: Record<string, any> = {},
) => {
  Object.assign(metadata, ctx)
  const e = new Error()
  const r = snake(reason)
  const q = new URLSearchParams({
    metadata: JSON.stringify(metadata),
  }).toString()
  const c = JSON.stringify(metadata, null, 2)
  e.message = `${reason}\nMore info: ${url}/${r}?${q}\nContext: ${c}`
  return e
}

const actionPlugins: Map<string, ActionPlugin> = new Map()
const attributePlugins: Map<string, AttributePlugin> = new Map()
const watcherPlugins: Map<string, WatcherPlugin> = new Map()

export const actions: Record<
  string,
  (ctx: ActionContext, ...args: any[]) => any
> = new Proxy(
  {},
  {
    get: (_, prop: string) => actionPlugins.get(prop)?.apply,
    has: (_, prop: string) => actionPlugins.has(prop),
    ownKeys: () => Reflect.ownKeys(actionPlugins),
    set: () => false,
    deleteProperty: () => false,
  },
)

// Map of cleanups keyed by element, attribute name, and cleanup name
const removals = new Map<HTMLOrSVG, Map<string, Map<string, () => void>>>()

const queuedAttributes: AttributePlugin[] = []
const queuedAttributeNames = new Set<string>()
const observedRoots = new WeakSet<Node>()
export const attribute = <R extends Requirement, B extends boolean>(
  plugin: AttributePlugin<R, B>,
): void => {
  queuedAttributes.push(plugin as unknown as AttributePlugin)

  if (queuedAttributes.length === 1) {
    setTimeout(() => {
      for (const attribute of queuedAttributes) {
        queuedAttributeNames.add(attribute.name)
        attributePlugins.set(attribute.name, attribute)
      }
      queuedAttributes.length = 0
      apply()
      queuedAttributeNames.clear()
    })
  }
}

export const action = <T>(plugin: ActionPlugin<T>): void => {
  actionPlugins.set(plugin.name, plugin)
}

document.addEventListener(DATASTAR_FETCH_EVENT, ((
  evt: CustomEvent<DatastarFetchEvent>,
) => {
  const plugin = watcherPlugins.get(evt.detail.type)
  if (plugin) {
    plugin.apply(
      {
        error: error.bind(0, {
          plugin: { type: 'watcher', name: plugin.name },
          element: {
            id: (evt.target as Element).id,
            tag: (evt.target as Element).tagName,
          },
        }),
      },
      evt.detail.argsRaw,
    )
  }
}) as EventListener)

export const watcher = (plugin: WatcherPlugin): void => {
  watcherPlugins.set(plugin.name, plugin)
}

const cleanupEls = (els: Iterable<HTMLOrSVG>): void => {
  for (const el of els) {
    const elCleanups = removals.get(el)
    if (elCleanups && removals.delete(el)) {
      for (const attrCleanups of elCleanups.values()) {
        for (const cleanup of attrCleanups.values()) {
          cleanup()
        }
      }
    }
  }
}

const aliasedIgnore = aliasify('ignore')
const aliasedIgnoreAttr = `[${aliasedIgnore}]`
const shouldIgnore = (el: HTMLOrSVG) =>
  el.hasAttribute(`${aliasedIgnore}__self`) || !!el.closest(aliasedIgnoreAttr)

const applyEls = (els: Iterable<HTMLOrSVG>, onlyNew?: boolean): void => {
  for (const el of els) {
    if (!shouldIgnore(el)) {
      for (const key in el.dataset) {
        applyAttributePlugin(
          el,
          key.replace(/[A-Z]/g, '-$&').toLowerCase(),
          el.dataset[key]!,
          onlyNew,
        )
      }
    }
  }
}

const observe = (mutations: MutationRecord[]) => {
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
    } else if (
      type === 'attributes' &&
      attributeName!.startsWith('data-') &&
      isHTMLOrSVG(target) &&
      !shouldIgnore(target)
    ) {
      // skip over 'data-'
      const key = attributeName!.slice(5)
      const value = target.getAttribute(attributeName!)
      if (value === null) {
        const elCleanups = removals.get(target)
        if (elCleanups) {
          const attrCleanups = elCleanups.get(key)
          if (attrCleanups) {
            for (const cleanup of attrCleanups.values()) {
              cleanup()
            }
            elCleanups.delete(key)
          }
        }
      } else {
        applyAttributePlugin(target, key, value)
      }
    }
  }
}

// TODO: mutation observer per root so applying to web component doesnt overwrite main observer
const mutationObserver = new MutationObserver(observe)

export const parseAttributeKey = (
  rawKey: string,
): {
  pluginName: string
  key: string | undefined
  mods: Modifiers
} => {
  const [namePart, ...rawModifiers] = rawKey.split('__')
  const [pluginName, key] = namePart.split(/:(.+)/)
  const mods: Modifiers = new Map()

  for (const rawMod of rawModifiers) {
    const [label, ...mod] = rawMod.split('.')
    mods.set(label, new Set(mod))
  }

  return { pluginName, key, mods }
}

export const isDocumentObserverActive = () =>
  observedRoots.has(document.documentElement)

export const apply = (
  root: HTMLOrSVG | ShadowRoot = document.documentElement,
  observeRoot = true,
): void => {
  if (isHTMLOrSVG(root)) {
    applyEls([root], true)
  }
  applyEls(root.querySelectorAll<HTMLOrSVG>('*'), true)

  if (observeRoot) {
    mutationObserver.observe(root, {
      subtree: true,
      childList: true,
      attributes: true,
    })
    observedRoots.add(root)
  }
}

const applyAttributePlugin = (
  el: HTMLOrSVG,
  attrKey: string,
  value: string,
  onlyNew?: boolean,
): void => {
  if (!ALIAS || attrKey.startsWith(`${ALIAS}-`)) {
    const rawKey = ALIAS ? attrKey.slice(ALIAS.length + 1) : attrKey
    const { pluginName, key, mods } = parseAttributeKey(rawKey)
    const plugin = attributePlugins.get(pluginName)
    if ((!onlyNew || queuedAttributeNames.has(pluginName)) && plugin) {
      const ctx = {
        el,
        rawKey,
        mods,
        error: error.bind(0, {
          plugin: { type: 'attribute', name: plugin.name },
          element: { id: el.id, tag: el.tagName },
          expression: { rawKey, key, value },
        }),
        key,
        value,
        loadedPluginNames: {
          actions: new Set(actionPlugins.keys()),
          attributes: new Set(attributePlugins.keys()),
        },
        rx: undefined,
      } as AttributeContext

      const keyReq =
        (plugin.requirement &&
          (typeof plugin.requirement === 'string'
            ? plugin.requirement
            : plugin.requirement.key)) ||
        'allowed'
      const valueReq =
        (plugin.requirement &&
          (typeof plugin.requirement === 'string'
            ? plugin.requirement
            : plugin.requirement.value)) ||
        'allowed'

      const keyProvided = key !== undefined && key !== null && key !== ''
      const valueProvided =
        value !== undefined && value !== null && value !== ''

      if (keyProvided) {
        if (keyReq === 'denied') {
          throw ctx.error('KeyNotAllowed')
        }
      } else if (keyReq === 'must') {
        throw ctx.error('KeyRequired')
      }

      if (valueProvided) {
        if (valueReq === 'denied') {
          throw ctx.error('ValueNotAllowed')
        }
      } else if (valueReq === 'must') {
        throw ctx.error('ValueRequired')
      }

      if (keyReq === 'exclusive' || valueReq === 'exclusive') {
        if (keyProvided && valueProvided) {
          throw ctx.error('KeyAndValueProvided')
        }
        if (!keyProvided && !valueProvided) {
          throw ctx.error('KeyOrValueRequired')
        }
      }

      const cleanups = new Map<string, () => void>()
      if (valueProvided) {
        let cachedRx: GenRxFn
        ctx.rx = (...args: any[]) => {
          if (!cachedRx) {
            cachedRx = genRx(value, {
              returnsValue: plugin.returnsValue,
              argNames: plugin.argNames,
              cleanups,
            })
          }
          return cachedRx(el, ...args)
        }
      }

      const cleanup = plugin.apply(ctx)
      if (cleanup) {
        cleanups.set('attribute', cleanup)
      }

      let elCleanups = removals.get(el)
      if (elCleanups) {
        const attrCleanups = elCleanups.get(rawKey)
        if (attrCleanups) {
          for (const oldCleanup of attrCleanups.values()) {
            oldCleanup()
          }
        }
      } else {
        elCleanups = new Map()
        removals.set(el, elCleanups)
      }
      elCleanups.set(rawKey, cleanups)
    }
  }
}

type GenRxOptions = {
  returnsValue?: boolean
  argNames?: string[]
  cleanups?: Map<string, () => void>
}

type GenRxFn = <T>(el: HTMLOrSVG, ...args: any[]) => T

const genRx = (
  value: string,
  {
    returnsValue = false,
    argNames = [],
    cleanups = new Map(),
  }: GenRxOptions = {},
): GenRxFn => {
  let expr = ''
  if (returnsValue) {
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
    const statements = value.trim().match(statementRe)
    if (statements) {
      const lastIdx = statements.length - 1
      const last = statements[lastIdx].trim()
      if (!last.startsWith('return')) {
        statements[lastIdx] = `return (${last});`
      }
      expr = statements.join(';\n')
    }
  } else {
    expr = value.trim()
  }

  // Ignore any escaped values
  const escaped = new Map<string, string>()
  const escapeRe = RegExp(`(?:${DSP})(.*?)(?:${DSS})`, 'gm')
  let counter = 0
  for (const match of expr.matchAll(escapeRe)) {
    const k = match[1]
    const v = `__escaped${counter++}`
    escaped.set(v, k)
    expr = expr.replace(DSP + k + DSS, v)
  }

  // Replace signal references with bracket notation
  // Examples:
  //   $count          -> $['count']
  //   $count--        -> $['count']--
  //   $foo.bar        -> $['foo']['bar']
  //   $foo-bar        -> $['foo-bar']
  //   $foo.bar-baz    -> $['foo']['bar-baz']
  //   $foo-$bar       -> $['foo']-$['bar']
  //   $arr[$index]    -> $['arr'][$['index']]
  //   $['foo']        -> $['foo']
  //   $foo[obj.bar]   -> $['foo'][obj.bar]
  //   $foo['bar.baz'] -> $['foo']['bar.baz']
  //   $123            -> $['123']
  //   $foo.0.name     -> $['foo']['0']['name']

  expr = expr
    // $['x'] -> $x (normalize existing bracket notation)
    .replace(/\$\['([a-zA-Z_$\d][\w$]*)'\]/g, '$$$1')
    // $x -> $['x'] (including dots and hyphens)
    .replace(/\$([a-zA-Z_\d]\w*(?:[.-]\w+)*)/g, (_, signalName) =>
      signalName
        .split('.')
        .reduce((acc: string, part: string) => `${acc}['${part}']`, '$'),
    )

  expr = expr.replaceAll(/@([A-Za-z_$][\w$]*)\(/g, '__action("$1",evt,')

  // Replace any escaped values
  for (const [k, v] of escaped) {
    expr = expr.replace(k, v)
  }

  try {
    const fn = Function('el', '$', '__action', 'evt', ...argNames, expr)
    return (el: HTMLOrSVG, ...args: any[]) => {
      const action = (name: string, evt: Event | undefined, ...args: any[]) => {
        const err = error.bind(0, {
          plugin: { type: 'action', name },
          element: { id: el.id, tag: el.tagName },
          expression: {
            fnContent: expr,
            value,
          },
        })
        const fn = actions[name]
        if (fn) {
          return fn(
            {
              el,
              evt,
              error: err,
              cleanups,
            },
            ...args,
          )
        }
        throw err('UndefinedAction')
      }
      try {
        return fn(el, root, action, undefined, ...args)
      } catch (e: any) {
        console.error(e)
        throw error(
          {
            element: { id: el.id, tag: el.tagName },
            expression: {
              fnContent: expr,
              value,
            },
            error: e.message,
          },
          'ExecuteExpression',
        )
      }
    }
  } catch (e: any) {
    console.error(e)
    throw error(
      {
        expression: {
          fnContent: expr,
          value,
        },
        error: e.message,
      },
      'GenerateExpression',
    )
  }
}
