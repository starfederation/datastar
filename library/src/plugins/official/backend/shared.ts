import { DATASTAR } from '../../../engine/consts'

export const DATASTAR_SSE_EVENT = `${DATASTAR}-sse`
export const STARTED = 'started'
export const FINISHED = 'finished'
export const ERROR = 'error'
export const RETRYING = 'retrying'
export const RETRIES_FAILED = 'retries-failed'

export interface DatastarSSEEvent {
  type: string
  elId: string
  argsRaw: Record<string, string>
}

export interface CustomEventMap {
  [DATASTAR_SSE_EVENT]: CustomEvent<DatastarSSEEvent>
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

export function dispatchSSE(
  type: string,
  elId: string,
  argsRaw: Record<string, string>,
) {
  document.dispatchEvent(
    new CustomEvent<DatastarSSEEvent>(DATASTAR_SSE_EVENT, {
      detail: { type, elId, argsRaw },
    }),
  )
}
