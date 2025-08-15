import {
  DATASTAR,
  DATASTAR_REQUEST,
  DefaultSseRetryDurationMs,
  EventTypePatchElements,
  EventTypePatchSignals,
} from '../../../engine/consts'
// Factory for creating HTTP method action plugins
import type {
  ActionPlugin,
  HTMLOrSVG,
  RuntimeContext,
  SignalFilterOptions,
} from '../../../engine/types'
import { kebab } from '../../../utils/text'
import {
  DATASTAR_FETCH_EVENT,
  type DatastarFetchEvent,
  ERROR,
  FINISHED,
  RETRIES_FAILED,
  RETRYING,
  STARTED,
} from '../shared'

// Global store for active SSE controllers per element
const fetchAbortControllers = new WeakMap<HTMLOrSVG, AbortController>()

// Helper to create HTTP method plugins with consistent structure
export const createHttpMethod = (
  name: string,
  method: string,
): ActionPlugin => ({
  type: 'action',
  name,
  fn: async (ctx, url: string, args: FetchArgs) => {
    const { el } = ctx

    const requestCancellation = args?.requestCancellation ?? 'auto'
    const controller =
      requestCancellation instanceof AbortController
        ? requestCancellation
        : new AbortController()
    const isDisabled = requestCancellation === 'disabled'
    if (!isDisabled) {
      fetchAbortControllers.get(el)?.abort()
    }

    if (!isDisabled && !(requestCancellation instanceof AbortController)) {
      fetchAbortControllers.set(el, controller)
    }

    try {
      await fetcher(ctx, method, url, args, controller.signal)
    } finally {
      if (fetchAbortControllers.get(el) === controller) {
        fetchAbortControllers.delete(el)
      }
    }
  },
})

const dispatchFetch = (
  type: string,
  el: HTMLOrSVG,
  argsRaw: Record<string, string>,
) =>
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
      useViewTransition?: boolean
    }
  | {
      onlyIfMissing?: boolean
    }

export type FetchArgs = {
  headers?: Record<string, string>
  openWhenHidden?: boolean
  retryInterval?: number
  retryScaler?: number
  retryMaxWaitMs?: number
  retryMaxCount?: number
  responseOverrides?: ResponseOverrides
  contentType?: 'json' | 'form'
  filterSignals?: SignalFilterOptions
  selector?: string
  requestCancellation?: 'auto' | 'disabled' | AbortController
}

const fetcher = async (
  { el, evt, filtered, runtimeErr }: RuntimeContext,
  method: string,
  url: string,
  {
    selector,
    headers: userHeaders,
    contentType = 'json',
    filterSignals: { include = /.*/, exclude = /(^|\.)_/ } = {},
    openWhenHidden = false,
    retryInterval = DefaultSseRetryDurationMs,
    retryScaler = 2,
    retryMaxWaitMs = 30_000,
    retryMaxCount = 10,
  }: FetchArgs = {},
  abort?: AbortSignal,
) => {
  const action = method.toLowerCase()
  let cleanupFn = () => {}
  try {
    if (!url?.length) {
      throw runtimeErr('FetchNoUrlProvided', { action })
    }

    const initialHeaders: Record<string, any> = {
      Accept: 'text/event-stream, text/html, application/json',
      [DATASTAR_REQUEST]: true,
    }
    if (contentType === 'json') {
      initialHeaders['Content-Type'] = 'application/json'
    }
    const headers = Object.assign({}, initialHeaders, userHeaders)

    // We ignore the content-type header if using form data
    // if missing the boundary will be set automatically

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
        if (response.status >= 400)
          dispatchFetch(ERROR, el, { status: response.status.toString() })
      },
      onmessage: (evt) => {
        if (!evt.event.startsWith(DATASTAR)) return
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
      onerror: (error) => {
        if (isWrongContent(error)) {
          // don't retry if the content-type is wrong
          throw runtimeErr('InvalidContentType', { url })
        }
        // do nothing and it will retry
        if (error) {
          console.error(error.message)
          dispatchFetch(RETRYING, el, { message: error.message })
        }
      },
    }

    const urlInstance = new URL(url, window.location.href)
    const queryParams = new URLSearchParams(urlInstance.search)

    if (contentType === 'json') {
      const res = JSON.stringify(filtered({ include, exclude }))
      if (method === 'GET') {
        queryParams.set(DATASTAR, res)
      } else {
        req.body = res
      }
    } else if (contentType === 'form') {
      const formEl = (
        selector ? document.querySelector(selector) : el.closest('form')
      ) as HTMLFormElement
      if (!formEl) {
        throw runtimeErr(
          selector ? 'FetchFormNotFound' : 'FetchClosestFormNotFound',
          { action, selector },
        )
      }

      // Validate the form
      if (!formEl.checkValidity()) {
        formEl.reportValidity()
        cleanupFn()
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
        cleanupFn = () => formEl.removeEventListener('submit', preventDefault)
      }

      // Append the value of the form submitter if it is a button with a name
      if (submitter instanceof HTMLButtonElement) {
        const name = submitter.getAttribute('name')
        if (name) formData.append(name, submitter.value)
      }

      const multipart = formEl.getAttribute('enctype') === 'multipart/form-data'
      // Leave the `Content-Type` header empty for multipart encoding so the browser can set it automatically with the correct boundary
      if (!multipart) {
        headers['Content-Type'] = 'application/x-www-form-urlencoded'
      }

      const formParams = new URLSearchParams(formData as any)
      if (method === 'GET') {
        for (const [key, value] of formParams) {
          queryParams.append(key, value)
        }
      } else if (multipart) {
        // Upload progress events are only available for: HTTPS connections (required for streaming uploads) with Multipart form data uploads
        if (__USE_UPLOAD_PROGRESS__ && urlInstance.protocol === 'https:') {
          const boundary = `----FormDataBoundary${Math.random().toString(36).substring(2, 11)}`
          const encoder = new TextEncoder()

          // Calculate total size
          let total = 0
          const parts: Array<{ field: string; value: string | File }> = []

          for (const [name, value] of formData) {
            parts.push({ field: name, value })
            total += encoder.encode(`--${boundary}\r\n`).byteLength

            if (value instanceof File) {
              total += encoder.encode(
                `Content-Disposition: form-data; name="${name}"; filename="${value.name}"\r\n` +
                  `Content-Type: ${value.type || 'application/octet-stream'}\r\n\r\n`,
              ).byteLength
              total += value.size + 2
            } else {
              total += encoder.encode(
                `Content-Disposition: form-data; name="${name}"\r\n\r\n${value}\r\n`,
              ).byteLength
            }
          }
          total += encoder.encode(`--${boundary}--\r\n`).byteLength

          let loaded = 0

          req.body = new ReadableStream({
            async start(controller) {
              const write = (data: Uint8Array) => {
                controller.enqueue(data)
                loaded += data.byteLength

                const progress = Math.round((loaded / total) * 100)
                dispatchFetch('upload-progress', el, {
                  progress: progress.toString(),
                  loaded: loaded.toString(),
                  total: total.toString(),
                })
              }

              dispatchFetch('upload-progress', el, {
                progress: '0',
                loaded: '0',
                total: total.toString(),
              })

              try {
                for (const { field, value } of parts) {
                  write(encoder.encode(`--${boundary}\r\n`))

                  if (value instanceof File) {
                    write(
                      encoder.encode(
                        `Content-Disposition: form-data; name="${field}"; filename="${value.name}"\r\n` +
                          `Content-Type: ${value.type || 'application/octet-stream'}\r\n\r\n`,
                      ),
                    )

                    const reader = value.stream().getReader()
                    try {
                      while (true) {
                        const { done, value: chunk } = await reader.read()
                        if (done) break
                        write(chunk)
                      }
                    } finally {
                      reader.releaseLock()
                    }
                    write(encoder.encode('\r\n'))
                  } else {
                    write(
                      encoder.encode(
                        `Content-Disposition: form-data; name="${field}"\r\n\r\n${value}\r\n`,
                      ),
                    )
                  }
                }

                write(encoder.encode(`--${boundary}--\r\n`))

                if (loaded < total) {
                  dispatchFetch('upload-progress', el, {
                    progress: '100',
                    loaded: total.toString(),
                    total: total.toString(),
                  })
                }

                controller.close()
              } catch (error) {
                controller.error(error)
              }
            },
          })

          // Override content-type header with the boundary
          headers['Content-Type'] = `multipart/form-data; boundary=${boundary}`
          // Set duplex mode for streaming uploads
          ;(req as any).duplex = 'half'
        } else {
          req.body = formData
        }
      } else {
        req.body = formParams
      }
    } else {
      throw runtimeErr('FetchInvalidContentType', { action, contentType })
    }

    dispatchFetch(STARTED, el, {})
    urlInstance.search = queryParams.toString()

    try {
      await fetchEventSource(urlInstance.toString(), el, req)
    } catch (error) {
      if (!isWrongContent(error)) {
        throw runtimeErr('FetchFailed', { method, url, error })
      }
      // exit gracefully and do nothing if the content-type is wrong
      // this can happen if the client is sending a request
      // where no response is expected, and they haven’t
      // set the content-type to text/event-stream
    }
  } finally {
    dispatchFetch(FINISHED, el, {})
    cleanupFn()
  }
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
async function getBytes(
  stream: ReadableStream<Uint8Array>,
  onChunk: (arr: Uint8Array) => void,
): Promise<void> {
  const reader = stream.getReader()
  let result = await reader.read()
  while (!result.done) {
    onChunk(result.value)
    result = await reader.read()
  }
}

function getLines(onLine: (line: Uint8Array, fieldLength: number) => void) {
  let buffer: Uint8Array | undefined
  let position: number // current read position
  let fieldLength: number // length of the `field` portion of the line
  let discardTrailingNewline = false

  // return a function that can process each incoming byte chunk:
  return function onChunk(arr: Uint8Array) {
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
          // @ts-ignore:7029 \r case below should fallthrough to \n:
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

function getMessages(
  onId: (id: string) => void,
  onRetry: (retry: number) => void,
  onMessage?: (msg: EventSourceMessage) => void,
) {
  let message = newMessage()
  const decoder = new TextDecoder()

  // return a function that can process each incoming line buffer:
  return function onLine(line: Uint8Array, fieldLength: number) {
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

interface FetchEventSourceInit extends RequestInit {
  headers?: Record<string, string>
  onopen?: (response: Response) => Promise<void>
  onmessage?: (ev: EventSourceMessage) => void
  onclose?: () => void
  onerror?: (err: any) => number | null | undefined | void
  openWhenHidden?: boolean
  fetch?: typeof fetch
  retryInterval?: number
  retryScaler?: number
  retryMaxWaitMs?: number
  retryMaxCount?: number
  overrides?: ResponseOverrides
}

function fetchEventSource(
  input: RequestInfo,
  el: HTMLOrSVG,
  {
    signal: inputSignal,
    headers: inputHeaders,
    onopen: inputOnOpen,
    onmessage,
    onclose,
    onerror,
    openWhenHidden,
    fetch: inputFetch,
    retryInterval = 1_000,
    retryScaler = 2,
    retryMaxWaitMs = 30_000,
    retryMaxCount = 10,
    overrides,
    ...rest
  }: FetchEventSourceInit,
) {
  return new Promise<void>((resolve, reject) => {
    // make a copy of the input headers since we may modify it below:
    const headers: Record<string, string> = {
      ...inputHeaders,
    }

    let curRequestController: AbortController
    function onVisibilityChange() {
      curRequestController.abort() // close existing request on every visibility change
      if (!document.hidden) create() // page is now visible again, recreate request.
    }

    if (!openWhenHidden) {
      document.addEventListener('visibilitychange', onVisibilityChange)
    }

    let retryTimer = 0
    function dispose() {
      document.removeEventListener('visibilitychange', onVisibilityChange)
      window.clearTimeout(retryTimer)
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
    async function create() {
      curRequestController = new AbortController()
      try {
        const response = await fetch(input, {
          ...rest,
          headers,
          signal: curRequestController.signal,
        })

        // on successful connection, reset the retry logic
        retries = 0
        retryInterval = baseRetryInterval

        await onopen(response)

        const dispatchNonSSE = async (
          dispatchType: string,
          response: Response,
          name: string,
          overrides?: ResponseOverrides,
          ...argNames: string[]
        ) => {
          const argsRaw: Record<string, string> = {
            [name]: await response.text(),
          }
          for (const n of argNames) {
            let v = response.headers.get(`datastar-${kebab(n)}`)
            if (overrides) {
              const o = (overrides as any)[n]
              if (o) v = typeof o === 'string' ? o : JSON.stringify(o)
            }
            if (v) argsRaw[n] = v
          }

          dispatchFetch(dispatchType, el, argsRaw)
          dispose()
          resolve()
        }

        const ct = response.headers.get('Content-Type')
        if (ct?.includes('text/html')) {
          return await dispatchNonSSE(
            EventTypePatchElements,
            response,
            'elements',
            overrides,
            'selector',
            'mode',
            'useViewTransition',
          )
        }

        if (ct?.includes('application/json')) {
          return await dispatchNonSSE(
            EventTypePatchSignals,
            response,
            'signals',
            overrides,
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
        dispose()
        resolve()
      } catch (err) {
        if (!curRequestController.signal.aborted) {
          // if we haven’t aborted the request ourselves:
          try {
            // check if we need to retry:
            const interval: any = onerror?.(err) || retryInterval
            window.clearTimeout(retryTimer)
            retryTimer = window.setTimeout(create, interval)
            retryInterval = Math.min(
              retryInterval * retryScaler,
              retryMaxWaitMs,
            ) // exponential backoff
            if (++retries >= retryMaxCount) {
              dispatchFetch(RETRIES_FAILED, el, {})
              // we should not retry anymore:
              dispose()
              reject('Max retries reached.') // Max retries reached, check your server or network connection
            } else {
              console.error(
                `Datastar failed to reach ${input.toString()} retrying in ${interval}ms.`,
              )
            }
          } catch (innerErr) {
            // we should not retry anymore:
            dispose()
            reject(innerErr)
          }
        }
      }
    }

    create()
  })
}
