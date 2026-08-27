import { compileExpression } from '@engine/csp'
import type { Modifiers } from '@engine/types'

export const kebab = (str: string): string =>
  str
    .replace(/([A-Z]+)([A-Z][a-z])/g, '$1-$2')
    .replace(/([a-z0-9])([A-Z])/g, '$1-$2')
    .replace(/([a-z])([0-9]+)/gi, '$1-$2')
    .replace(/([0-9]+)([a-z])/gi, '$1-$2')
    .replace(/[\s_]+/g, '-')
    .toLowerCase()

export const camel = (str: string): string =>
  kebab(str).replace(/-./g, (x) => x[1].toUpperCase())

export const snake = (str: string): string => kebab(str).replace(/-/g, '_')

export const pascal = (str: string): string =>
  camel(str).replace(/(^.|(?<=\.).)/g, (x) => x[0].toUpperCase())

export const title = (str: string): string =>
  str.replace(/\b\w/g, (char) => char.toUpperCase())

export const isStringTrue = (value: unknown): value is string =>
  typeof value === 'string' && value.trim() === 'true'

const RE_FUNCTION_LITERAL =
  /^(?:(?:async\s+)?function\b|(?:async\s*)?(?:\([^)]*\)|[A-Za-z_$][\w$]*)\s*=>)/

type JsStrToObjectOptions = {
  reviveFunctionStrings?: boolean
}

const evaluate = (source: string): any =>
  compileExpression([], `return (${source})`)()

export const jsStrToObject = (
  raw: string,
  options: JsStrToObjectOptions = {},
) => {
  const { reviveFunctionStrings = false } = options
  try {
    if (!reviveFunctionStrings) return JSON.parse(raw)
    return JSON.parse(raw, (_k, value) => {
      if (typeof value !== 'string') return value
      const trimmed = value.trim()
      if (!RE_FUNCTION_LITERAL.test(trimmed)) return value
      try {
        const revived = evaluate(trimmed)
        return typeof revived === 'function' ? revived : value
      } catch {
        return value
      }
    })
  } catch {
    // If JSON parsing fails, try to evaluate as a JavaScript object
    // This is less safe and should be used with caution
    return evaluate(raw)
  }
}

const caseFns: Record<string, (s: string) => string> = {
  camel: (str) => str.replace(/-[a-z]/g, (x) => x[1].toUpperCase()),
  snake: (str) => str.replace(/-/g, '_'),
  pascal: (str) => str[0].toUpperCase() + caseFns.camel(str.slice(1)),
}

export const modifyCasing = (
  str: string,
  mods: Modifiers,
  defaultCase = 'camel',
): string => {
  for (const c of mods.get('case') || [defaultCase]) {
    str = caseFns[c]?.(str) || str
  }
  return str
}

export const aliasify = (name: string) =>
  ALIAS ? `data-${ALIAS}-${name}` : `data-${name}`

export const unaliasify = (name: string) => {
  if (!ALIAS) return name
  if (!name.startsWith(`${ALIAS}-`)) return null
  return name.slice(ALIAS.length + 1)
}
