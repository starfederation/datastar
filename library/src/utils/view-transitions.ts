import { DOCUMENT } from '@engine/consts'
import type { EventCallbackHandler, Modifiers } from '@engine/types'

export const supportsViewTransitions = (target: Document | Element): boolean =>
  'startViewTransition' in target

export const modifyViewTransition = (
  callback: EventCallbackHandler,
  mods: Modifiers,
): EventCallbackHandler => {
  if (mods.has('viewtransition') && supportsViewTransitions(DOCUMENT)) {
    const cb = callback // I hate javascript
    callback = (...args: any[]) =>
      DOCUMENT.startViewTransition(() => cb(...args))
  }

  return callback
}
