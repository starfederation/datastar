import type { HTMLOrSVG, Modifiers } from '../engine/types'
import { findClosestScoped } from './dom'

export const isBoolString = (str: string) => str.trim() === 'true'

export const kebab = (str: string) =>
  str
    .replace(/([a-z0-9])([A-Z])/g, '$1-$2')
    .replace(/([a-z])([0-9]+)/gi, '$1-$2')
    .replace(/([0-9]+)([a-z])/gi, '$1-$2')
    .toLowerCase()

export const camel = (str: string) =>
  kebab(str).replace(/-./g, (x) => x[1].toUpperCase())

export const snake = (str: string) => kebab(str).replace(/-/g, '_')

export const pascal = (str: string) =>
  camel(str).replace(/(^.|(?<=\.).)/g, (x) => x[0].toUpperCase())

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

export function modifyScope(
  signalName: string,
  el: HTMLOrSVG,
  mods: Modifiers,
) {
  if (mods.has('scoped')) {
    const scope = findClosestScoped(el)
    if (scope) {
      signalName = `${scope}.${signalName}`
    }
  }
  return signalName
}
