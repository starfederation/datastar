import { DOCUMENT } from '@engine/consts'
import type { EventCallbackHandler, Modifiers } from '@engine/types'

export const supportsViewTransitions = (target: Document | Element): boolean =>
  'startViewTransition' in target

export const modifyViewTransition = (
  callback: EventCallbackHandler,
  mods: Modifiers,
): EventCallbackHandler => {
<<<<<<< Updated upstream
  if (mods.has('viewtransition') && supportsViewTransitions(document)) {
=======
  if (mods.has('viewtransition') && supportsViewTransitions(DOCUMENT)) {
>>>>>>> Stashed changes
    const cb = callback // I hate javascript
    callback = (...args: any[]) =>
      DOCUMENT.startViewTransition(() => cb(...args))
  }

  return callback
}
