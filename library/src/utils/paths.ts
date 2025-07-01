export const isPojo = (obj: any): obj is Record<string, any> =>
  obj !== null &&
  typeof obj === 'object' &&
  (Object.getPrototypeOf(obj) === Object.prototype ||
    Object.getPrototypeOf(obj) === null)

export function isEmpty(obj: Record<string, any>): boolean {
  for (const prop in obj) {
    if (Object.hasOwn(obj, prop)) {
      return false
    }
  }
  return true
}

export function updateLeaves(
  obj: Record<string, any>,
  fn: (oldValue: any) => any,
) {
  for (const key in obj) {
    const val = obj[key]
    if (isPojo(val) || Array.isArray(val)) {
      updateLeaves(val, fn)
    } else {
      obj[key] = fn(val)
    }
  }
}

export const pathToObj = (
  target: Record<string, any>,
  paths: Record<string, any>,
): Record<string, any> => {
  for (const path in paths) {
    const keys = path.split('.')
    const lastKey = keys.pop()!
    const obj = keys.reduce((acc, key) => (acc[key] ??= {}), target)
    obj[lastKey] = paths[path]
  }
  return target
}
