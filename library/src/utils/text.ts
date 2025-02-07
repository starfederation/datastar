import type { Modifiers } from '../engine/types'

export const isBoolString = (str: string) => str.trim() === 'true'

export const kebab = (str: string) =>
  str.replace(
    /[A-Z]+(?![a-z])|[A-Z]/g,
    ($, ofs) => (ofs ? '-' : '') + $.toLowerCase(),
  )

export const camel = (str: string) =>
  kebab(str).replace(/-./g, (x) => x[1].toUpperCase())

export const snake = (str: string) => kebab(str).replace(/-/g, '_')

export const pascal = (str: string) =>
  camel(str).replace(/^./, (x) => x[0].toUpperCase())

export const jsStrToObject = (raw: string) =>
  new Function(`return Object.assign({}, ${raw})`)()

export const trimDollarSignPrefix = (str: string) =>
  str.startsWith('$') ? str.slice(1) : str

const caseFns: Record<string, (s: string) => string> = { kebab, snake, pascal }

export function modifyCasing(str: string, mods: Modifiers) {
  for (const c of mods.get('case') || []) {
    const fn = caseFns[c]
    if (fn) str = fn(str)
  }
  return str
}
