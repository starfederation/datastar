import { DATASTAR } from '../../../engine/consts'

export const DATASTAR_SSE_EVENT = `${DATASTAR}-sse`
export const SETTLING_CLASS = `${DATASTAR}-settling`
export const SWAPPING_CLASS = `${DATASTAR}-swapping`
export const STARTED = 'started'
export const FINISHED = 'finished'
export const ERROR = 'error'

export interface DatastarSSEEvent {
  type: string
  argsRaw: Record<string, string>
}

export interface CustomEventMap {
  'datastar-sse': CustomEvent<DatastarSSEEvent>
}
export type WatcherFn<K extends keyof CustomEventMap> = (
  this: Document,
  ev: CustomEventMap[K],
) => void

declare global {
  interface Document {
    //adds definition to Document, but you can do the same with HTMLElement
    addEventListener<K extends keyof CustomEventMap>(
      type: K,
      listener: WatcherFn<K>,
    ): void
    removeEventListener<K extends keyof CustomEventMap>(
      type: K,
      listener: WatcherFn<K>,
    ): void
    dispatchEvent<K extends keyof CustomEventMap>(ev: CustomEventMap[K]): void
  }
}

export function datastarSSEEventWatcher(
  // ctx: InitContext,
  eventType: string,
  fn: (argsRaw: Record<string, string>) => void,
) {
  document.addEventListener(
    DATASTAR_SSE_EVENT,
    (event: CustomEvent<DatastarSSEEvent>) => {
      if (event.detail.type !== eventType) return
      const { argsRaw } = event.detail
      fn(argsRaw)
    },
  )
}
