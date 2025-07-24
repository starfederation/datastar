import type { Modifiers } from '../engine/types'

export const isBoolString = (str: string) => str.trim() === 'true'

export const kebab = (str: string) =>
  str
    .replace(/([A-Z])/g, '-$1')
    .toLowerCase()

export const camel = (str: string) => str

export const snake = (str: string) => kebab(str).replace(/-/g, '_')

export const pascal = (str: string) =>
  str[0].toUpperCase() + str.slice(1)

export const removePrefix = (prefix: string, str: string) => {
  // removes a prefix from a string in dataset case
  // e.g. removePrefix("signals", "signalsKeyname") -> "keyname"
  const suffix = str.slice(prefix.replace(/-[a-z]/g, "-").length)
  if (!suffix) return suffix
  return (suffix[0] === "-" ? "" : suffix[0].toLowerCase()) + suffix.slice(1)
}

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
