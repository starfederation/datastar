import type { Modifiers } from '../engine/types'

export const isBoolString = (str: string) => str.trim() === 'true'

export const kebab = (str: string) =>
  str
    .replace(/([A-Z])/g, '-$1')
    .toLowerCase()

export const camel = (str: string) =>
  kebab(str).replace(/-[a-z]/g, (x) => x[1].toUpperCase())

export const snake = (str: string) => kebab(str).replace(/-/g, '_')

export const pascal = (str: string) =>
  str[0].toUpperCase() + camel(str).slice(1)

export const lowerFirst = (str: string) =>
    (str[0] === "-" ? "" : str[0].toLowerCase()) + str.slice(1)

export const jsStrToObject = (raw: string) => {
  try {
    return JSON.parse(raw)
  } catch {
    // If JSON parsing fails, try to evaluate as a JavaScript object
    // This is less safe and should be used with caution
    return Function(`return (${raw})`)()
  }
}

const caseFns: Record<string, (s: string) => string> = { kebab, snake, pascal }

export function modifyCasing(str: string, mods: Modifiers) {
  for (const c of mods.get('case') || []) {
    const fn = caseFns[c]
    if (fn) str = fn(str)
  }
  return str
}
