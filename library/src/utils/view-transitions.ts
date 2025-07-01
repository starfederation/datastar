import type { EventCallbackHandler, Modifiers } from '../engine/types'

export interface DocumentSupportingViewTransitionAPI {
  startViewTransition(
    updateCallback: () => Promise<void> | void,
  ): IViewTransition
}

export interface IViewTransition {
  finished: Promise<void>
  ready: Promise<void>
  updateCallbackDone: Promise<void>
  skipTransition(): void
}

export const supportsViewTransitions = !!document.startViewTransition

export function modifyViewTransition(
  callback: EventCallbackHandler,
  mods: Modifiers,
): EventCallbackHandler {
  if (mods.has('viewtransition') && supportsViewTransitions) {
    const cb = callback // I hate javascript
    callback = (...args: any[]) =>
      document.startViewTransition(() => cb(...args))
  }

  return callback
}
