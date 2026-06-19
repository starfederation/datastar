// Icon: ion:eye
// Slug: Access signals without subscribing to changes.
// Description: Allows accessing signals without subscribing to their changes in expressions.

import { action } from '@engine'
import { DATASTAR_FETCH_EVENT } from '@engine/consts'
import { filtered, startPeeking, stopPeeking } from '@engine/signals'
import type {
  DatastarFetchEvent,
  HTMLOrSVG,
  SignalFilterOptions,
  WatcherArgs,
} from '@engine/types'
import { kebab } from '@utils/text'

// Map of abort controllers keyed by method and URL
const abortControllers = new Map<string, Map<string, AbortController>>()
const methodSupportsRequestBody = (method: string): boolean =>
  !['GET', 'DELETE'].includes(method)

const createHttpMethod = (
  name: string,
  method: string,
  openWhenHiddenDefault: boolean = true,
): void =>
  action({
    name,
    apply: async (
      { el, evt, error, cleanups },
      url: string,
      {
        selector,
        headers: userHeaders,
        contentType = 'json',
        filterSignals: { include = /.*/, exclude = /(^|\.)_/ } = {},
        openWhenHidden = openWhenHiddenDefault,
        payload,
        requestCancellation = 'auto',
        retry = 'auto',
        retryInterval = 1_000,
        retryScaler = 2,
        retryMaxWait = 30_000,
        retryMaxCount = 10,
      }: FetchArgs = {},
    ) => {
      const controller =
        requestCancellation instanceof AbortController
          ? requestCancellation
          : new AbortController()
      if (requestCancellation === 'auto' || requestCancellation === 'cleanup') {
        abortControllers.get(method)?.get(url)?.abort()
        if (!abortControllers.has(method)) {
          abortControllers.set(method, new Map())
        }
        abortControllers.get(method)!.set(url, controller)
      }
      if (requestCancellation === 'cleanup') {
        cleanups.get(`@${name}`)?.()
        cleanups.set(`@${name}`, async () => {
          controller.abort()
          // wait one tick for FINISHED to fire
          await Promise.resolve()
        })
      }

      let cleanupFn = () => {}

      try {
        if (!url?.length) {
          throw error('FetchNoUrlProvided', { action })
        }

        const initialHeaders: Record<string, any> = {
          Accept: 'text/event-stream, text/html, application/json',
          'Datastar-Request': true,
        }
        if (contentType === 'json' && methodSupportsRequestBody(method)) {
          initialHeaders['Content-Type'] = 'application/json'
        }
        const headers = Object.assign({}, initialHeaders, userHeaders)

        // We ignore the content-type header if using form data
        // if missing the boundary will be set automatically

        const req: FetchEventSourceInit = {
          input: '',
          method,
          headers,
          openWhenHidden,
          retry,
          retryInterval,
          retryScaler,
          retryMaxWait,
          retryMaxCount,
          signal: controller.signal,
          onopen: async (response: Response) => {
            if (response.status >= 400) {
              dispatchFetch(ERROR, el, { status: response.status.toString() })
            }
          },
          onmessage: (evt) => {
            if (!evt.event.startsWith('datastar')) return
            const type = evt.event
            const argsRawLines: Record<string, string[]> = {}

            for (const line of evt.data.split('\n')) {
              const i = line.indexOf(' ')
              const k = line.slice(0, i)
              const v = line.slice(i + 1)
              ;(argsRawLines[k] ||= []).push(v)
            }

            const argsRaw = Object.fromEntries(
              Object.entries(argsRawLines).map(([k, v]) => [k, v.join('\n')]),
            )

            dispatchFetch(type, el, argsRaw)
          },
          onerror: (err) => {
            if (isWrongContent(err)) {
              // don't retry if the content-type is wrong
              throw error('FetchExpectedTextEventStream', { url })
            }
          },
        }

        const buildFetchEventSourceInit = () => {
          const urlInstance = new URL(url, document.baseURI)
          const queryParams = new URLSearchParams(urlInstance.search)
          if (contentType === 'json') {
            startPeeking()
            const requestPayload =
              payload !== undefined ? payload : filtered({ include, exclude })
            stopPeeking()
            const body = JSON.stringify(requestPayload)
            if (methodSupportsRequestBody(method)) {
              req.body = body
            } else {
              queryParams.set('datastar', body)
            }
          } else if (contentType === 'form') {
            const formEl = (
              selector ? document.querySelector(selector) : el.closest('form')
            ) as HTMLFormElement
            if (!formEl) {
              throw error('FetchFormNotFound', { action, selector })
            }

            // Validate the form
            if (!formEl.noValidate && !formEl.checkValidity()) {
              formEl.reportValidity()
              return
            }

            // Collect the form data
            const formData = new FormData(formEl)
            let submitter = el as HTMLElement | null

            if (el === formEl && evt instanceof SubmitEvent) {
              // Get the submitter from the event
              submitter = evt.submitter
            } else {
              // Prevent the form being submitted
              const preventDefault = (evt: Event) => evt.preventDefault()
              formEl.addEventListener('submit', preventDefault)
              cleanupFn = () => {
                formEl.removeEventListener('submit', preventDefault)
              }
            }

            // Append the value of the form submitter if it is a valid submitter and has a name
            if (
              submitter instanceof HTMLButtonElement ||
              (submitter instanceof HTMLInputElement &&
                submitter.type === 'submit')
            ) {
              const name = submitter.getAttribute('name')
              if (name) formData.append(name, submitter.value)
            }

            const multipart =
              formEl.getAttribute('enctype') === 'multipart/form-data'
            // Leave the `Content-Type` header empty for multipart encoding so the browser can set it automatically with the correct boundary
            if (!multipart) {
              headers['Content-Type'] = 'application/x-www-form-urlencoded'
            }

            const formParams = new URLSearchParams(formData as any)
            if (methodSupportsRequestBody(method)) {
              if (multipart) {
                req.body = formData
              } else {
                req.body = formParams
              }
            } else {
              for (const [key, value] of formParams) {
                queryParams.append(key, value)
              }
            }
          } else {
            throw error('FetchInvalidContentType', { action, contentType })
          }
          urlInstance.search = queryParams.toString()
          req.input = urlInstance.toString()
          return req
        }

        dispatchFetch(STARTED, el, {})

        try {
          await fetchEventSource(el, buildFetchEventSourceInit)
        } catch (err: any) {
          if (!isWrongContent(err)) {
            throw error('FetchFailed', { method, url, error: err.message })
          }
          // exit gracefully and do nothing if the content-type is wrong
          // this can happen if the client is sending a request
          // where no response is expected, and they haven’t
          // set the content-type to text/event-stream
        }
      } finally {
        dispatchFetch(FINISHED, el, {})
        cleanupFn()
        cleanups.delete(`@${name}`)
      }
    },
  })

createHttpMethod('get', 'GET', false)
createHttpMethod('patch', 'PATCH')
createHttpMethod('post', 'POST')
createHttpMethod('put', 'PUT')
createHttpMethod('delete', 'DELETE')

export const STARTED = 'started'
export const FINISHED = 'finished'
export const ERROR = 'error'
export const RETRYING = 'retrying'
export const RETRIES_FAILED = 'retries-failed'

const dispatchFetch = (type: string, el: HTMLOrSVG, argsRaw: WatcherArgs) =>
  document.dispatchEvent(
    new CustomEvent<DatastarFetchEvent>(DATASTAR_FETCH_EVENT, {
      detail: { type, el, argsRaw },
    }),
  )

const isWrongContent = (err: any) => `${err}`.includes('text/event-stream')

type ResponseOverrides =
  | {
      selector?: string
      mode?: string
      namespace?: string
      useViewTransition?: boolean
    }
  | {
      onlyIfMissing?: boolean
    }

export type FetchArgs = {
  selector?: string
  headers?: Record<string, string>
  contentType?: 'json' | 'form'
  filterSignals?: SignalFilterOptions
  openWhenHidden?: boolean
  payload?: any
  requestCancellation?: 'auto' | 'cleanup' | 'disabled' | AbortController
  responseOverrides?: ResponseOverrides
  retry?: 'auto' | 'error' | 'always' | 'never'
  retryInterval?: number
  retryScaler?: number
  retryMaxWait?: number
  retryMaxCount?: number
}

// Below originally from https://github.com/Azure/fetch-event-source/blob/main/LICENSE

/**
 * Represents a message sent in an event stream
 * https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#Event_stream_format
 */

interface EventSourceMessage {
  id: string
  event: string
  data: string
  retry?: number
}

/**
 * Converts a ReadableStream into a callback pattern.
 * @param stream The input ReadableStream.
 * @param onChunk A function that will be called on each new byte chunk in the stream.
 * @returns {Promise<void>} A promise that will be resolved when the stream closes.
 */
const getBytes = async (
  stream: ReadableStream<Uint8Array>,
  onChunk: (arr: Uint8Array) => void,
): Promise<void> => {
  const reader = stream.getReader()
  let result = await reader.read()
  while (!result.done) {
    onChunk(result.value)
    result = await reader.read()
  }
}

const getLines = (onLine: (line: Uint8Array, fieldLength: number) => void) => {
  let buffer: Uint8Array | undefined
  let position: number // current read position
  let fieldLength: number // length of the `field` portion of the line
  let discardTrailingNewline = false

  // return a function that can process each incoming byte chunk:
  return (arr: Uint8Array) => {
    if (!buffer) {
      buffer = arr
      position = 0
      fieldLength = -1
    } else {
      // we're still parsing the old line. Append the new bytes into buffer:
      buffer = concat(buffer, arr)
    }

    const bufLength = buffer.length
    let lineStart = 0 // index where the current line starts
    while (position < bufLength) {
      if (discardTrailingNewline) {
        if (buffer[position] === 10) lineStart = ++position // skip to next char
        discardTrailingNewline = false
      }

      // start looking forward till the end of line:
      let lineEnd = -1 // index of the \r or \n char
      for (; position < bufLength && lineEnd === -1; ++position) {
        switch (buffer[position]) {
          case 58: // :
            if (fieldLength === -1) {
              // first colon in line
              fieldLength = position - lineStart
            }
            break
          // @ts-expect-error:7029 \r case below should fallthrough to \n:
          // biome-ignore lint/suspicious/noFallthroughSwitchClause: intentional fallthrough for CR to LF
          case 13: // \r
            discardTrailingNewline = true
          case 10: // \n
            lineEnd = position
            break
        }
      }

      if (lineEnd === -1) break // Wait for the next arr and then continue parsing

      // we've reached the line end, send it out:
      onLine(buffer.subarray(lineStart, lineEnd), fieldLength)
      lineStart = position // we're now on the next line
      fieldLength = -1
    }

    if (lineStart === bufLength)
      buffer = undefined // we've finished reading it
    else if (lineStart) {
      // Create a new view into buffer beginning at lineStart so we don't
      // need to copy over the previous lines when we get the new arr:
      buffer = buffer.subarray(lineStart)
      position -= lineStart
    }
  }
}

const getMessages = (
  onId: (id: string) => void,
  onRetry: (retry: number) => void,
  onMessage?: (msg: EventSourceMessage) => void,
): ((line: Uint8Array, fieldLength: number) => void) => {
  let message = newMessage()
  const decoder = new TextDecoder()

  // return a function that can process each incoming line buffer:
  return (line, fieldLength) => {
    if (!line.length) {
      // empty line denotes end of message. Trigger the callback and start a new message:
      onMessage?.(message)
      message = newMessage()
    } else if (fieldLength > 0) {
      // exclude comments and lines with no values
      // line is of format "<field>:<value>" or "<field>: <value>"
      // https://html.spec.whatwg.org/multipage/server-sent-events.html#event-stream-interpretation
      const field = decoder.decode(line.subarray(0, fieldLength))
      const valueOffset = fieldLength + (line[fieldLength + 1] === 32 ? 2 : 1)
      const value = decoder.decode(line.subarray(valueOffset))

      switch (field) {
        case 'data':
          message.data = message.data ? `${message.data}\n${value}` : value
          break
        case 'event':
          message.event = value
          break
        case 'id':
          onId((message.id = value))
          break
        case 'retry': {
          const retry = +value
          if (!Number.isNaN(retry)) {
            // per spec, ignore non-integers
            onRetry((message.retry = retry))
          }
          break
        }
      }
    }
  }
}

const concat = (a: Uint8Array, b: Uint8Array) => {
  const res = new Uint8Array(a.length + b.length)
  res.set(a)
  res.set(b, a.length)
  return res
}

const newMessage = (): EventSourceMessage => ({
  // data, event, and id must be initialized to empty strings:
  // https://html.spec.whatwg.org/multipage/server-sent-events.html#event-stream-interpretation
  // retry should be initialized to undefined so we return a consistent shape
  // to the js engine all the time: https://mathiasbynens.be/notes/shapes-ics#takeaways
  data: '',
  event: '',
  id: '',
  retry: undefined,
})

type FetchEventSourceInit =
  | (RequestInit & {
      input: RequestInfo
      headers?: Record<string, string>
      onopen?: (response: Response) => Promise<void>
      onmessage?: (ev: EventSourceMessage) => void
      onclose?: () => void
      onerror?: (err: any) => void
      openWhenHidden?: boolean
      fetch?: typeof fetch
      retry?: 'auto' | 'error' | 'always' | 'never'
      retryInterval?: number
      retryScaler?: number
      retryMaxWait?: number
      retryMaxCount?: number
      responseOverrides?: ResponseOverrides
    })
  | undefined

const fetchEventSource = (
  el: HTMLOrSVG,
  buildFetchEventSourceInit: () => FetchEventSourceInit,
): Promise<void> => {
  return new Promise<void>((resolve, reject) => {
    const fetchInit = buildFetchEventSourceInit()
    if (!fetchInit) {
      return
    }
    let {
      input,
      signal: inputSignal,
      headers: inputHeaders,
      onopen: inputOnOpen,
      onmessage,
      onclose,
      openWhenHidden,
      fetch: inputFetch,
      retry = 'auto',
      retryInterval = 1_000,
      retryScaler = 2,
      retryMaxWait = 30_000,
      retryMaxCount = 10,
      responseOverrides,
      ...rest
    }: FetchEventSourceInit = fetchInit

    // make a copy of the input headers since we may modify it below:
    const headers: Record<string, string> = {
      ...inputHeaders,
    }

    let curRequestController: AbortController

    const rebuildAndRetry = () => {
      const currentFetchInit = buildFetchEventSourceInit()
      if (!currentFetchInit) return

      input = currentFetchInit.input
      rest.body = currentFetchInit.body
      create()
    }

    const onVisibilityChange = () => {
      curRequestController.abort()
      if (!document.hidden) {
        rebuildAndRetry()
      }
    }

    if (!openWhenHidden) {
      document.addEventListener('visibilitychange', onVisibilityChange)
    }

    let retryTimer: ReturnType<typeof setTimeout> | undefined
    const dispose = () => {
      document.removeEventListener('visibilitychange', onVisibilityChange)
      clearTimeout(retryTimer)
      curRequestController.abort()
    }

    // if the incoming signal aborts, dispose resources and resolve:
    inputSignal?.addEventListener('abort', () => {
      dispose()
      resolve() // don't waste time constructing/logging errors
    })

    const fetch = inputFetch || window.fetch
    const onopen = inputOnOpen || (() => {})

    let retries = 0
    let baseRetryInterval = retryInterval

    const retryRequest = () => {
      if (retries < retryMaxCount) {
        dispatchFetch(RETRYING, el, {})
        clearTimeout(retryTimer)
        retryTimer = setTimeout(rebuildAndRetry, retryInterval)
        retries++
        // Prepare the interval for the next retry (exponential backoff)
        retryInterval = Math.min(retryInterval * retryScaler, retryMaxWait)
      } else {
        dispatchFetch(RETRIES_FAILED, el, {})
        dispose()
        reject('Max retries reached.')
      }
    }

    const create = async () => {
      curRequestController = new AbortController()
      const curRequestSignal = curRequestController.signal
      try {
        const response = await fetch(input, {
          ...rest,
          headers,
          signal: curRequestSignal,
        })

        await onopen(response)

        const dispatchNonSSE = async (
          dispatchType: string,
          response: Response,
          name: string,
          responseOverrides?: ResponseOverrides,
          ...argNames: string[]
        ) => {
          const argsRaw: WatcherArgs = {
            [name]: await response.text(),
          }
          for (const n of argNames) {
            let v = response.headers.get(`datastar-${kebab(n)}`)
            if (responseOverrides) {
              const o = (responseOverrides as any)[n]
              if (o) v = typeof o === 'string' ? o : JSON.stringify(o)
            }
            if (v) argsRaw[n] = v
          }

          dispatchFetch(dispatchType, el, argsRaw)
          dispose()
          resolve()
        }

        const status = response.status
        const isNoContentStatus = status === 204
        const isRedirectStatus = status >= 300 && status < 400
        const isErrorStatus = status >= 400 && status < 600

        if (status !== 200) {
          onclose?.()
          if (
            retry !== 'never' &&
            !isNoContentStatus &&
            !isRedirectStatus &&
            (retry === 'always' || (retry === 'error' && isErrorStatus))
          ) {
            retryRequest()
            return
          }
          dispose()
          resolve()
          return
        }

        // on successful connection, reset the retry logic
        retries = 0
        retryInterval = baseRetryInterval

        const ct = response.headers.get('Content-Type')
        if (ct?.includes('text/html')) {
          return await dispatchNonSSE(
            'datastar-patch-elements',
            response,
            'elements',
            responseOverrides,
            'selector',
            'mode',
            'namespace',
            'useViewTransition',
          )
        }

        if (ct?.includes('application/json')) {
          return await dispatchNonSSE(
            'datastar-patch-signals',
            response,
            'signals',
            responseOverrides,
            'onlyIfMissing',
          )
        }

        if (ct?.includes('text/javascript')) {
          const script = document.createElement('script')
          const scriptAttributesHeader = response.headers.get(
            'datastar-script-attributes',
          )

          if (scriptAttributesHeader) {
            for (const [name, value] of Object.entries(
              JSON.parse(scriptAttributesHeader),
            )) {
              script.setAttribute(name, value as string)
            }
          }
          script.textContent = await response.text()
          document.head.appendChild(script)
          dispose()
          return
        }

        await getBytes(
          response.body!,
          getLines(
            getMessages(
              (id) => {
                if (id) {
                  // signals the id and send it back on the next retry:
                  headers['last-event-id'] = id
                } else {
                  // don't send the last-event-id header anymore:
                  delete headers['last-event-id']
                }
              },
              (retry) => {
                baseRetryInterval = retryInterval = retry
              },
              onmessage,
            ),
          ),
        )

        onclose?.()

        if (retry === 'always' && !isRedirectStatus) {
          retryRequest()
          return
        }

        dispose()
        resolve()
      } catch {
        if (!curRequestSignal.aborted) {
          try {
            retryRequest()
          } catch (innerErr) {
            dispose()
            reject(innerErr)
          }
        }
      }
    }

    create()
  })
}
