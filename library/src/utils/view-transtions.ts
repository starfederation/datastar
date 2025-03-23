import type { Modifiers } from "../engine/types"

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

export const docWithViewTransitionAPI =
  document as unknown as DocumentSupportingViewTransitionAPI

export const supportsViewTransitions =
  !!docWithViewTransitionAPI.startViewTransition

export function modifyViewTransition(
  callback: (...args: any[]) => any,
  mods: Modifiers,
  ) {
  if (mods.has('viewtransition') && supportsViewTransitions) {
    const cb = callback // I hate javascript
    callback = (...args: any[]) =>
      document.startViewTransition(() => cb(...args))
  }

  return callback
}