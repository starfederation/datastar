import type { Modifiers } from '../engine/types'

export const isBoolString = (str: string) => str.trim() === 'true'

export const kebab = (str: string) =>
  str
    .replace(/([a-z0-9])([A-Z])/g, '$1-$2')
    .replace(/([a-z])([0-9]+)/gi, '$1-$2')
    .replace(/([0-9]+)([a-z])/gi, '$1-$2')
    .toLowerCase()

export const snake = (str: string) => kebab(str).replace(/-/g, '_')

export const jsStrToObject = (raw: string) => {
  try {
    return JSON.parse(raw)
  } catch {
    // If JSON parsing fails, try to evaluate as a JavaScript object
    // This is less safe and should be used with caution
    return Function(`return (${raw})`)()
  }
}

// The case mods expect the input to be raw attribute names (kebab-case)
export const modCamel = (str: string) =>
    str.replace(/-[a-z]/g, (x) => x[1].toUpperCase())

export const modSnake = (str: string) => str.replace(/-/g, '_')

export const modPascal = (str: string) =>
    str[0].toUpperCase() + modCamel(str.slice(1))

const caseFns: Record<string, (s: string) => string> = { camel: modCamel, snake: modSnake, pascal: modPascal }

export function modifyCasing(str: string, mods: Modifiers, defaultCase: string = 'camel') {
  for (const c of mods.get('case') || [defaultCase]) {
    const fn = caseFns[c]
    if (fn) str = fn(str)
  }
  return str
}
