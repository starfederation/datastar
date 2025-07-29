import { DATASTAR } from '../../engine/consts'
import type { HTMLOrSVG } from '../../engine/types'

export const DATASTAR_FETCH_EVENT = `${DATASTAR}-fetch`
export const STARTED = 'started'
export const FINISHED = 'finished'
export const ERROR = 'error'
export const RETRYING = 'retrying'
export const RETRIES_FAILED = 'retries-failed'

export interface DatastarFetchEvent {
  type: string
  el: HTMLOrSVG
  argsRaw: Record<string, string>
}

export interface CustomEventMap {
  [DATASTAR_FETCH_EVENT]: CustomEvent<DatastarFetchEvent>
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
    DATASTAR_FETCH_EVENT,
    (event: CustomEvent<DatastarFetchEvent>) => {
      if (event.detail.type === eventType) {
        const { argsRaw } = event.detail
        fn(argsRaw)
      }
    },
  )
}
