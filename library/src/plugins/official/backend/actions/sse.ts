// Icon: ic:baseline-get-app
// Slug: Use a GET request to fetch data from a server using Server-Sent Events matching the Datastar SDK interface
// Description: Remember, SSE is just a regular SSE request but with the ability to send 0-inf messages to the client.

import { DATASTAR, DATASTAR_REQUEST, DefaultSseRetryDurationMs } from '../../../../engine/consts'
import { runtimeErr } from '../../../../engine/errors'
import type { HTMLorSVGElement, RuntimeContext } from '../../../../engine/types'
import {
  type FetchEventSourceInit,
  fetchEventSource,
} from '../../../../vendored/fetch-event-source'
import {
  DATASTAR_SSE_EVENT,
  type DatastarSSEEvent,
  ERROR,
  FINISHED,
  RETRYING,
  STARTED,
} from '../shared'

function dispatchSSE(el: HTMLorSVGElement, type: string, argsRaw: Record<string, string>) {
  el.dispatchEvent(
    new CustomEvent<DatastarSSEEvent>(DATASTAR_SSE_EVENT, {
      detail: { type, argsRaw },
      bubbles: true,
    }),
  )
}

const isWrongContent = (err: any) => `${err}`.includes('text/event-stream')

export type SSEArgs = {
  headers?: Record<string, string>
  openWhenHidden?: boolean
  retryInterval?: number
  retryScaler?: number
  retryMaxWaitMs?: number
  retryMaxCount?: number
  abort?: AbortSignal
} & (
  | {
      contentType: 'json'
      includeLocal?: boolean
    }
  | {
      contentType: 'form'
      selector?: string
    }
)

export const sse = async (
  ctx: RuntimeContext,
  method: string,
  url: string,
  args: SSEArgs,
) => {
  const {
    el: { id: elId },
    el,
    signals,
  } = ctx
  const {
    headers: userHeaders,
    contentType,
    includeLocal,
    selector,
    openWhenHidden,
    retryInterval,
    retryScaler,
    retryMaxWaitMs,
    retryMaxCount,
    abort,
  } = Object.assign(
    {
      headers: {},
      contentType: 'json',
      includeLocal: false,
      selector: null,
      openWhenHidden: false, // will keep the request open even if the document is hidden.
      retryInterval: DefaultSseRetryDurationMs, // the retry interval in milliseconds
      retryScaler: 2, // the amount to multiply the retry interval by each time
      retryMaxWaitMs: 30_000, // the maximum retry interval in milliseconds
      retryMaxCount: 10, // the maximum number of retries before giving up
      abort: undefined,
    },
    args,
  )
  const action = method.toLowerCase()
  let cleanupFn = (): void => {}
  try {
    dispatchSSE(el, STARTED, { elId })
    if (!url?.length) {
      throw runtimeErr('SseNoUrlProvided', ctx, { action })
    }

    const initialHeaders: Record<string, any> = {}
    initialHeaders[DATASTAR_REQUEST] = true
    // We ignore the content-type header if using form data
    // if missing the boundary will be set automatically
    if (contentType === 'json') {
      initialHeaders['Content-Type'] = 'application/json'
    }
    const headers = Object.assign({}, initialHeaders, userHeaders)

    const req: FetchEventSourceInit = {
      method,
      headers,
      openWhenHidden,
      retryInterval,
      retryScaler,
      retryMaxWaitMs,
      retryMaxCount,
      signal: abort,
      onopen: async (response: Response) => {
        if (response.status >= 400) {
          const status = response.status.toString()
          dispatchSSE(el, ERROR, { status })
        }
      },
      onmessage: (evt) => {
        if (!evt.event.startsWith(DATASTAR)) {
          return
        }
        const type = evt.event
        const argsRawLines: Record<string, string[]> = {}

        const lines = evt.data.split('\n')
        for (const line of lines) {
          const colonIndex = line.indexOf(' ')
          const key = line.slice(0, colonIndex)
          let argLines = argsRawLines[key]
          if (!argLines) {
            argLines = []
            argsRawLines[key] = argLines
          }
          const value = line.slice(colonIndex + 1)
          argLines.push(value)
        }

        const argsRaw: Record<string, string> = {}
        for (const [key, lines] of Object.entries(argsRawLines)) {
          argsRaw[key] = lines.join('\n')
        }

        // if you aren't seeing your event you can debug by using this line in the console
        dispatchSSE(el, type, argsRaw)
      },
      onerror: (error) => {
        if (isWrongContent(error)) {
          // don't retry if the content-type is wrong
          throw runtimeErr('InvalidContentType', ctx, { url })
        }
        // do nothing and it will retry
        if (error) {
          console.error(error.message)
          dispatchSSE(el, RETRYING, { message: error.message })
        }
      },
    }

    const urlInstance = new URL(url, window.location.origin)
    const queryParams = new URLSearchParams(urlInstance.search)

    if (contentType === 'json') {
      const json = signals.JSON(false, !includeLocal)
      if (method === 'GET') {
        queryParams.set(DATASTAR, json)
      } else {
        req.body = json
      }
    } else if (contentType === 'form') {
      const formEl = selector
        ? document.querySelector(selector)
        : el.closest('form')
      if (formEl === null) {
        if (selector) {
          throw runtimeErr('SseFormNotFound', ctx, { action, selector })
        }
        throw runtimeErr('SseClosestFormNotFound', ctx, { action })
      }
      if (el !== formEl) {
        const preventDefault = (evt: Event) => evt.preventDefault()
        formEl.addEventListener('submit', preventDefault)
        cleanupFn = (): void =>
          formEl.removeEventListener('submit', preventDefault)
      }
      if (!formEl.checkValidity()) {
        formEl.reportValidity()
        cleanupFn()
        return
      }
      const formData = new FormData(formEl)
      if (method === 'GET') {
        const formParams = new URLSearchParams(formData as any)
        for (const [key, value] of formParams) {
          queryParams.set(key, value)
        }
      } else {
        req.body = formData
      }
    } else {
      throw runtimeErr('SseInvalidContentType', ctx, { action, contentType })
    }

    urlInstance.search = queryParams.toString()

    try {
      await fetchEventSource(el, urlInstance.toString(), req)
    } catch (error) {
      if (!isWrongContent(error)) {
        throw runtimeErr('SseFetchFailed', ctx, { method, url, error })
      }
      // exit gracefully and do nothing if the content-type is wrong
      // this can happen if the client is sending a request
      // where no response is expected, and they haven't
      // set the content-type to text/event-stream
    }
  } finally {
    dispatchSSE(el, FINISHED, { elId })
    cleanupFn()
  }
}
