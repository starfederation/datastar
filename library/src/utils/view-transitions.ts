import type { EventCallbackHandler, Modifiers } from '@engine/types'

export const supportsViewTransitions = (target: Document | Element): boolean =>
  'startViewTransition' in target

export const modifyViewTransition = (
  callback: EventCallbackHandler,
  mods: Modifiers,
): EventCallbackHandler => {
  if (mods.has('viewtransition') && supportsViewTransitions(document)) {
    const cb = callback // I hate javascript
    callback = (...args: any[]) =>
      document.startViewTransition(() => cb(...args))
  }

  return callback
}
