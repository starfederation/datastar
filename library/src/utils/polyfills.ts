export const hasOwn: (obj: object, prop: PropertyKey) => boolean =
  // @ts-expect-error
  Object.hasOwn ?? Object.prototype.hasOwnProperty.call
