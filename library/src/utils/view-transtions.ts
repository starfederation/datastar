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