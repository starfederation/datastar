import { parseAttributeKey } from '@engine'
import { createError } from '@engine/errors'
import { aliasify, modifyCasing, unaliasify } from '@utils/text'

// Carries the signal base and any loop-local aliases that a rewritten Rocket subtree should resolve against.
//
// Rocket templates author against `$$foo`, `block`, `i`, etc., so every rewrite pass needs the component's private base path and any local alias remaps in scope.
export type TaggedLiteralScope = {
  signalPathBase: string
  localSignals?: Record<string, string>
}

export type TaggedLiteral = (
  strings: TemplateStringsArray,
  ...values: unknown[]
) => DocumentFragment

// Minimal parse modes for placing inert interpolation markers into static HTML while keeping runtime values out of the markup passed to the parser.
type TemplateContext =
  | 'data'
  | 'tag'
  | 'attrDouble'
  | 'attrSingle'
  | 'attrUnquoted'
  | 'rawtext'
  | 'comment'
  | 'declaration'

// Stateful scanner carried across template string chunks.
//
// Tagged templates split source around interpolations, so this tracks enough HTML context to identify where the next `${...}` occurs without parsing the DOM twice.
type TemplateScanState = {
  context: TemplateContext
  tagName: string
  closingTag: boolean
  captureTagName: boolean
  expectAttributeValue: boolean
  rawTextTag: string
}

const rawTextTags = new Set(['script', 'style', 'textarea', 'title'])

// Append Nodes and nested iterables into a fragment so `${child}` and `${items.map(...)}` compose DOM instead of being serialized as HTML text.
export const appendComposedTemplateValue = (
  fragment: DocumentFragment,
  value: unknown,
) => {
  if (value == null || typeof value === 'boolean') return
  if (value instanceof Node) {
    fragment.appendChild(value)
    return
  }
  if (
    value instanceof Date ||
    typeof value === 'string' ||
    typeof value === 'number' ||
    typeof value === 'bigint'
  ) {
    fragment.appendChild(document.createTextNode(stringifyTemplateValue(value)))
    return
  }
  if (
    typeof value !== 'object' ||
    value === null ||
    !(Symbol.iterator in value)
  ) {
    throw createError({ valueType: typeof value }, 'RocketTemplateInvalidValue')
  }
  for (const item of value as Iterable<unknown>) {
    appendComposedTemplateValue(fragment, item)
  }
}

// Build one HTML or SVG fragment from a tagged template in two phases:
//
// 1. parse only the trusted static chunks plus inert interpolation markers
// 2. resolve those markers through DOM APIs before `postprocess(...)`
//
// Rocket needs this split pipeline because its templates allow all of these at once:
// - primitives in text or attribute positions
// - boolean-style whole-attribute expressions like `disabled=${flag}`
// - DOM nodes and nested iterables like `${children}`
// - a final rewrite pass that scopes Datastar attributes after parsing
//
// Runtime values consequently cannot change the parsed structure, while the browser still performs the actual HTML/SVG parsing.
export function hypertext(
  render: (string: string) => ParentNode,
  postprocess: (root: ParentNode) => DocumentFragment,
) {
  // Only nodes produced by this literal have already been scoped for it.
  const produced = new WeakSet<Node>()
  const prepare = (value: unknown): { value: unknown; processed: boolean } => {
    if (value instanceof DocumentFragment) {
      return {
        value,
        processed: [...value.childNodes].every((node) => produced.has(node)),
      }
    }
    if (value instanceof Node) return { value, processed: produced.has(value) }
    if (value && typeof value === 'object' && Symbol.iterator in value) {
      // Materialize once: probing a generator and then inserting it would consume
      // it twice and silently lose its content.
      const items = Array.from(value as Iterable<unknown>, prepare)
      return {
        value: items.map((item) => item.value),
        processed: items.every((item) => item.processed),
      }
    }
    return {
      value,
      processed:
        value === null || typeof value !== 'object' || value instanceof Date,
    }
  }
  return ({ raw: strings }: TemplateStringsArray, ...values: unknown[]) => {
    const state: TemplateScanState = {
      context: 'data',
      tagName: '',
      closingTag: false,
      captureTagName: false,
      expectAttributeValue: false,
      rawTextTag: '',
    }
    let markerPrefix = 'rocketmarker'
    // Keep the prefix distinct from static template content so ordinary text cannot be mistaken for an interpolation marker during resolution.
    while (strings.some((string) => string.includes(markerPrefix))) {
      markerPrefix += 'x'
    }
    const marker = (index: number) => `${markerPrefix}${index}x`
    const markerContexts: TemplateContext[] = []
    let string = ''

    for (let j = 0; j < strings.length; ++j) {
      const input = strings[j]
      // Advance the scanner across this chunk so the next interpolation can use a parser-safe marker without placing its runtime value in the HTML source.
      for (let i = 0; i < input.length; i += 1) {
        const char = input[i]
        switch (state.context) {
          case 'data':
            if (input.startsWith('<!--', i)) {
              state.context = 'comment'
              i += 3
              continue
            }
            if (char === '<') {
              if (input[i + 1] === '!' || input[i + 1] === '?') {
                state.context = 'declaration'
                continue
              }
              state.context = 'tag'
              state.tagName = ''
              state.closingTag = input[i + 1] === '/'
              state.captureTagName = true
              state.expectAttributeValue = false
            }
            break
          case 'comment':
            if (input.startsWith('-->', i)) {
              state.context = 'data'
              i += 2
            }
            break
          case 'declaration':
            if (char === '>') state.context = 'data'
            break
          case 'rawtext':
            if (
              char === '<' &&
              input
                .slice(i + 1, i + 2 + state.rawTextTag.length)
                .toLowerCase() === `/${state.rawTextTag}` &&
              (!input[i + 2 + state.rawTextTag.length] ||
                /[\s/>]/.test(input[i + 2 + state.rawTextTag.length] ?? ''))
            ) {
              state.context = 'tag'
              state.tagName = ''
              state.closingTag = true
              state.captureTagName = true
              state.expectAttributeValue = false
            }
            break
          case 'tag':
            if (!state.tagName) {
              if (state.closingTag && char === '/') continue
              if (/\s/.test(char)) continue
              if (char === '>') {
                state.context = state.rawTextTag ? 'rawtext' : 'data'
                state.closingTag = false
                state.captureTagName = false
                state.expectAttributeValue = false
                continue
              }
              state.tagName = char.toLowerCase()
              continue
            }
            if (state.captureTagName) {
              if (!/[\s/>]/.test(char)) {
                state.tagName += char.toLowerCase()
                continue
              }
              state.captureTagName = false
            }
            if (char === '>') {
              if (state.closingTag) {
                if (state.tagName === state.rawTextTag) state.rawTextTag = ''
                state.context = 'data'
              } else if (rawTextTags.has(state.tagName)) {
                state.rawTextTag = state.tagName
                state.context = 'rawtext'
              } else {
                state.context = 'data'
              }
              state.closingTag = false
              state.captureTagName = false
              state.expectAttributeValue = false
              continue
            }
            if (state.closingTag) continue
            if (state.expectAttributeValue) {
              if (/\s/.test(char)) continue
              state.expectAttributeValue = false
              if (char === '"') state.context = 'attrDouble'
              else if (char === "'") state.context = 'attrSingle'
              else state.context = 'attrUnquoted'
              continue
            }
            if (char === '=') state.expectAttributeValue = true
            break
          case 'attrDouble':
            if (char === '"') state.context = 'tag'
            break
          case 'attrSingle':
            if (char === "'") state.context = 'tag'
            break
          case 'attrUnquoted':
            if (/\s/.test(char)) state.context = 'tag'
            else if (char === '>') {
              if (state.closingTag) {
                if (state.tagName === state.rawTextTag) state.rawTextTag = ''
                state.context = 'data'
              } else if (rawTextTags.has(state.tagName)) {
                state.rawTextTag = state.tagName
                state.context = 'rawtext'
              } else {
                state.context = 'data'
              }
              state.closingTag = false
              state.captureTagName = false
              state.expectAttributeValue = false
            }
            break
        }
      }
      string += input
      if (j === values.length) continue

      let context = state.context
      if (context === 'tag' && state.expectAttributeValue) {
        // An interpolation immediately after `=` has no value character to move the scanner into an attribute context, so treat it as unquoted data.
        context = 'attrUnquoted'
        state.context = context
        state.expectAttributeValue = false
      }
      markerContexts[j] = context
      const token = marker(j)
      // Comments reserve positions that can expand into nodes, while attributes and raw-text contexts require plain tokens that remain inside their values.
      if (context === 'data') string += `<!--${token}-->`
      else if (
        context === 'attrDouble' ||
        context === 'attrSingle' ||
        context === 'attrUnquoted' ||
        context === 'rawtext'
      ) {
        string += token
      } else {
        throw createError(
          { context, valueType: typeof values[j] },
          'RocketTemplateInvalidComposition',
        )
      }
    }
    const root = render(string)

    const resolved = values.map(() => false)
    const markerPattern = new RegExp(`${markerPrefix}(\\d+)x`, 'g')
    const replaceMarkers = (
      source: string,
      replace: (index: number) => string,
    ) =>
      source.replace(markerPattern, (_, rawIndex: string) => {
        const index = +rawIndex
        resolved[index] = true
        return replace(index)
      })
    const stringifyMarkerValue = (index: number) => {
      const value = values[index]
      // Nodes and iterables compose meaningfully only at node positions, and stringifying them elsewhere would conceal an invalid composition.
      if (
        value instanceof Node ||
        (typeof value === 'object' &&
          value !== null &&
          Symbol.iterator in value)
      ) {
        throw createError(
          {
            context: markerContexts[index],
            valueType: value instanceof Node ? 'node' : 'iterable',
          },
          'RocketTemplateInvalidComposition',
        )
      }
      return stringifyTemplateValue(value)
    }

    const resolveMarkers = (target: ParentNode) => {
      const elements =
        target instanceof Element
          ? [target, ...target.querySelectorAll('*')]
          : [...target.querySelectorAll('*')]
      for (const element of elements) {
        for (const attr of [...element.attributes]) {
          if (!attr.value.includes(markerPrefix)) continue
          const match = attr.value.match(new RegExp(`^${markerPrefix}(\\d+)x$`))
          if (match) {
            // A marker that occupies the entire value preserves optional and boolean attribute semantics; partial values retain their surrounding text.
            const index = +match[1]
            const value = values[index]
            resolved[index] = true
            if (value == null || value === false || value === '') {
              element.removeAttribute(attr.name)
            } else if (value === true) {
              element.setAttribute(attr.name, '')
            } else {
              element.setAttribute(attr.name, stringifyMarkerValue(index))
            }
            continue
          }
          element.setAttribute(
            attr.name,
            replaceMarkers(attr.value, stringifyMarkerValue),
          )
        }
      }

      const comments = document.createTreeWalker(
        target,
        NodeFilter.SHOW_COMMENT,
      )
      const commentNodes: Comment[] = []
      while (comments.nextNode())
        commentNodes.push(comments.currentNode as Comment)
      for (const node of commentNodes) {
        const match = node.data.match(new RegExp(`^${markerPrefix}(\\d+)x$`))
        if (!match) continue
        const index = +match[1]
        resolved[index] = true
        const composed = prepare(values[index])
        if (composed.processed) {
          deferred.push([node, composed.value])
          continue
        }
        const fragment = document.createDocumentFragment()
        appendComposedTemplateValue(fragment, composed.value)
        node.replaceWith(fragment)
      }

      const text = document.createTreeWalker(target, NodeFilter.SHOW_TEXT)
      const textNodes: Text[] = []
      while (text.nextNode()) textNodes.push(text.currentNode as Text)
      for (const node of textNodes) {
        if (!node.data.includes(markerPrefix)) continue
        node.data = replaceMarkers(node.data, (index) => {
          const value = values[index]
          if (value == null || typeof value === 'boolean') return ''
          return stringifyMarkerValue(index)
        })
      }

      // Template contents live in separate document fragments that normal descendant traversal does not enter.
      for (const element of elements) {
        if (element instanceof HTMLTemplateElement) {
          resolveMarkers(element.content)
        }
      }
    }
    const deferred: Array<[Comment, unknown]> = []
    resolveMarkers(root)

    const unresolved = resolved.findIndex((value) => !value)
    if (unresolved !== -1) {
      throw createError(
        {
          context: markerContexts[unresolved],
          valueType: typeof values[unresolved],
        },
        'RocketTemplateInvalidComposition',
      )
    }
    const fragment = postprocess(root as DocumentFragment)
    for (const [marker, value] of deferred) {
      const composed = document.createDocumentFragment()
      appendComposedTemplateValue(composed, value)
      marker.replaceWith(composed)
    }
    for (const node of fragment.childNodes) produced.add(node)
    return fragment
  }
}

export const signalNameAttributes = new Set([
  'bind',
  'computed',
  'indicator',
  'ref',
  'signals',
])

const directSignalValuePlugins = new Set(['bind', 'indicator', 'ref'])

export const rocketDispatchActionName = 'dispatchRocket'
export const rocketRefAttr = 'data-rocket-ref'

const toRocketRefName = (raw: string) => {
  const name = raw.split('__', 1)[0]
  return modifyCasing(name, parseAttributeKey(raw).mods)
}

// Convert Rocket-authored expressions into plain Datastar expressions so the engine can evaluate local `$$...` state, loop aliases, and local actions.
const rewriteDatastarExpression = (
  value: string,
  scope: TaggedLiteralScope,
  onScopedSignal?: () => void,
): string => {
  const { signalPathBase, localSignals = {} } = scope
  const aliases = Object.keys(localSignals).sort(
    (left, right) => right.length - left.length,
  )
  const aliasPattern = aliases
    .map((name) => name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'))
    .join('|')
  // Protect literal and comment regions first, then recursively rewrite template substitutions and executable expression tokens.
  return value.replace(
    new RegExp(
      `(//[^\\n]*|/\\*[\\s\\S]*?\\*/|"[^"\\\\]*(?:\\\\.[^"\\\\]*)*"|'[^'\\\\]*(?:\\\\.[^'\\\\]*)*'|(?:^|[([{,:;=!?&|+*%^~<>-]\\s*|\\b(?:return|throw|case|delete|void|typeof|instanceof|in|of|yield|await)\\s+)/(?:\\\\.|\\[(?:\\\\.|[^\\]\\\\])*\\]|[^/\\\\[])+/[dgimsuvy]*)|(\`(?:\\\\.|[^\`\\\\])*\`)|\\$\\$([a-zA-Z_\\d]\\w*(?:[.-]\\w+)*)|@([A-Za-z_$][\\w$]*)\\(|${aliasPattern ? `(?<![\\w$.])(${aliasPattern})(?![\\w$]|\\s*:)` : '(?!)'}`,
      'gm',
    ),
    (
      match,
      protectedRegion: string | undefined,
      template: string | undefined,
      signal: string | undefined,
      action: string | undefined,
      alias: string | undefined,
    ) => {
      if (protectedRegion) return protectedRegion
      if (template) {
        return template.replace(
          /\$\{((?:"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*'|`(?:\\.|[^`\\])*`|\/(?:\\.|[^/\\])+\/[dgimsuvy]*|\{(?:[^{}]|\{[^{}]*\})*\}|[^{}])*)\}/g,
          (_, expression: string) =>
            `\${${rewriteDatastarExpression(expression, scope, onScopedSignal)}}`,
        )
      }
      if (signal) {
        onScopedSignal?.()
        return `$.${signalPathBase}.${signal}`
      }
      if (action) {
        return action === rocketDispatchActionName
          ? match
          : `@${rocketDispatchActionName}(${JSON.stringify(action)},`
      }
      return `$.${localSignals[alias!]}`
    },
  )
}

// Use the same literal-aware rules for pre-upgrade guards and actual scoping.
export const hasRocketScopedExpression = (value: string): boolean => {
  let found = false
  rewriteDatastarExpression(value, { signalPathBase: '' }, () => {
    found = true
  })
  return found
}

// Attribute names are case-insensitive in HTML. Datastar's default camel casing
// restores these escaped capitals without changing the logical Rocket path.
export const encodeSignalKey = (path: string) =>
  path.replace(/[A-Z]/g, (char) => `-${char.toLowerCase()}`)

// Re-scope Rocket aliases like `$$count`, loop locals, and `@foo(...)` into the concrete signal/action paths Datastar executes for this subtree instance.
export const rewriteDataAttributes = (
  root: ParentNode,
  scope?: TaggedLiteralScope,
) => {
  if (!scope) return
  const { signalPathBase, localSignals = {} } = scope
  const scopedBase = encodeSignalKey(signalPathBase)
  const signalsAttr = aliasify('signals')
  const nodes =
    root instanceof Element
      ? [root, ...root.querySelectorAll('*')]
      : [...root.children, ...root.querySelectorAll('*')]
  for (const node of nodes) {
    for (const attr of [...node.attributes]) {
      if (!attr.name.startsWith('data-')) continue
      const attrBody = unaliasify(attr.name.slice(5))
      if (!attrBody) continue
      const { pluginName, key: signalKey, mods } = parseAttributeKey(attrBody)
      // Keep __root in the DOM so later scope passes and clones retain intent.
      if (signalNameAttributes.has(pluginName) && mods.has('root')) {
        if (
          signalKey !== undefined ||
          !directSignalValuePlugins.has(pluginName)
        ) {
          const nextValue = rewriteDatastarExpression(attr.value, scope)
          if (nextValue !== attr.value) node.setAttribute(attr.name, nextValue)
        }
        continue
      }
      if (attr.name === signalsAttr) {
        if (node.hasAttribute(`${signalsAttr}:${scopedBase}`)) {
          node.removeAttribute(attr.name)
          continue
        }
        node.removeAttribute(attr.name)
        node.setAttribute(`${signalsAttr}:${scopedBase}`, attr.value)
        continue
      }
      if (pluginName === 'ref') {
        const colonIndex = attr.name.indexOf(':')
        const refName =
          colonIndex !== -1
            ? toRocketRefName(attr.name.slice(colonIndex + 1))
            : toRocketRefName(attr.value)
        node.removeAttribute(attr.name)
        if (refName) {
          node.setAttribute(rocketRefAttr, refName)
        }
        continue
      }
      if (signalNameAttributes.has(pluginName)) {
        const colonIndex = attr.name.indexOf(':')
        if (colonIndex !== -1) {
          const keyWithMods = attr.name.slice(colonIndex + 1)
          const key = keyWithMods.split('__', 1)[0]
          const scoped = key === scopedBase || key.startsWith(`${scopedBase}.`)
          const scopedKeyWithMods = scoped
            ? keyWithMods
            : `${encodeSignalKey(localSignals[key] ?? `${signalPathBase}.${modifyCasing(key, mods)}`)}${keyWithMods.slice(key.length).replace(/__case(?:\.[^_]+)*/g, '')}`
          const nextValue = rewriteDatastarExpression(attr.value, scope)
          if (scopedKeyWithMods !== keyWithMods) {
            node.removeAttribute(attr.name)
            node.setAttribute(
              `${attr.name.slice(0, colonIndex + 1)}${scopedKeyWithMods}`,
              nextValue,
            )
          } else if (nextValue !== attr.value) {
            node.setAttribute(attr.name, nextValue)
          }
          continue
        }
        if (directSignalValuePlugins.has(pluginName)) {
          if (pluginName === 'ref' && attr.value) {
            if (
              attr.value === signalPathBase ||
              attr.value.startsWith(`${signalPathBase}.`)
            ) {
              continue
            }
            const localSignal = localSignals[attr.value]
            if (localSignal) {
              node.setAttribute(attr.name, localSignal)
              continue
            }
            node.setAttribute(attr.name, `${signalPathBase}.${attr.value}`)
            continue
          }
          const localSignal = localSignals[attr.value]
          if (localSignal) {
            node.setAttribute(attr.name, localSignal)
            continue
          }
          if (attr.value.startsWith('$$')) {
            node.setAttribute(
              attr.name,
              `${signalPathBase}.${attr.value.slice(2)}`,
            )
          }
          continue
        }
      }
      const nextValue = rewriteDatastarExpression(attr.value, scope)
      if (nextValue === attr.value) continue
      node.setAttribute(attr.name, nextValue)
    }
  }
}

// Convert a supported primitive template value into its DOM text form.
function stringifyTemplateValue(value: unknown) {
  if (value instanceof Date) return value.toISOString()
  switch (typeof value) {
    case 'string':
      return value
    case 'number':
    case 'boolean':
    case 'bigint':
      return `${value}`
    case 'object':
      if (value === null) return 'null'
  }
  throw createError({ valueType: typeof value }, 'RocketTemplateInvalidValue')
}
