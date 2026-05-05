import type { EventCallbackHandler, Modifiers } from '@engine/types'

export const supportsViewTransitions = (
  element: Element = document.documentElement,
): boolean => 'startViewTransition' in element

export const modifyViewTransition = (
  callback: EventCallbackHandler,
  mods: Modifiers,
): EventCallbackHandler => {
  if (mods.has('viewtransition') && supportsViewTransitions()) {
    const cb = callback // I hate javascript
    callback = (...args: any[]) =>
      document.startViewTransition(() => cb(...args))
  }

  return callback
}
