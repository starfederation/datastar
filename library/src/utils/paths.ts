import type { Paths } from '@engine/types'
import { hasOwn } from '@utils/polyfills'

export const isPojo = (obj: any): obj is Record<string, any> =>
  obj !== null &&
  typeof obj === 'object' &&
  (Object.getPrototypeOf(obj) === Object.prototype ||
    Object.getPrototypeOf(obj) === null)

export const isEmpty = (obj: Record<string, any>): boolean => {
  for (const prop in obj) {
    if (hasOwn(obj, prop)) {
      return false
    }
  }
  return true
}

export const updateLeaves = (
  obj: Record<string, any>,
  fn: (oldValue: any) => any,
) => {
  for (const key in obj) {
    const val = obj[key]
    if (isPojo(val) || Array.isArray(val)) {
      updateLeaves(val, fn)
    } else {
      obj[key] = fn(val)
    }
  }
}

export const pathToObj = (paths: Paths): Record<string, any> => {
  const result: Record<string, any> = {}
  for (const [path, value] of paths) {
    const keys = path.split('.')
    const lastKey = keys.pop()!
    const obj = keys.reduce((acc, key) => (acc[key] ??= {}), result)
    obj[lastKey] = value
  }
  return result
}
