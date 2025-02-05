import type { Modifiers } from '../engine/types'

export const isBoolString = (str: string) => str.trim() === 'true'

export const lcFirst = (str: string) => str[0].toLowerCase() + str.slice(1)

export const ucFirst = (str: string) => str[0].toUpperCase() + str.slice(1)

export const kebabize = (str: string) =>
  str.replace(
    /[A-Z]+(?![a-z])|[A-Z]/g,
    ($, ofs) => (ofs ? '-' : '') + $.toLowerCase(),
  )

export const camelize = (str: string) =>
  kebabize(str).replace(/-./g, (x) => x[1].toUpperCase())

export const jsStrToObject = (raw: string) =>
  new Function(`return Object.assign({}, ${raw})`)()

export const trimDollarSignPrefix = (str: string) =>
  str.startsWith('$') ? str.slice(1) : str

export function modifyCasing(str: string, mods: Modifiers) {
  const casing = mods.get('casing')
  if (casing) {
    str = casing.has('kebab')
      ? kebabize(str)
      : casing.has('snake')
        ? kebabize(str).replace(/-/g, '_')
        : casing.has('pascal')
          ? ucFirst(str)
          : str
  }

  return str
}
