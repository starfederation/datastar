// Icon: ic:baseline-get-app
// Slug: Use a GET request to fetch data from a server using Server-Sent Events matching the Datastar SDK interface
// Description: Remember, SSE is just a regular SSE request but with the ability to send 0-inf messages to the client.

import {
  DATASTAR,
  DATASTAR_REQUEST,
  DefaultSseRetryDurationMs,
  EventTypePatchElements,
  EventTypePatchSignals,
} from '../../../../engine/consts'
import type {
  HTMLOrSVG,
  RuntimeContext,
  SignalFilterOptions,
} from '../../../../engine/types'
import { kebab } from '../../../../utils/text'
import {
  DATASTAR_SSE_EVENT,
  type DatastarSSEEvent,
  ERROR,
  FINISHED,
  RETRIES_FAILED,
  RETRYING,
  STARTED,
} from '../shared'

const isWrongContent = (err: any) => `${err}`.includes('text/event-stream')

export type ContentType = 'json' | 'form'

export interface FormCollectionResult {
  body: string | FormData | URLSearchParams | null
  headers: Record<string, string>
  cleanup: () => void
}

export const dispatchSSE = (
  type: string,
  el: HTMLOrSVG,
  argsRaw: Record<string, any>,
) =>
  document.dispatchEvent(
    new CustomEvent<DatastarSSEEvent>(DATASTAR_SSE_EVENT, {
      detail: { type, el, argsRaw },
    }),
  )

export function setupHeaders(
  contentType: ContentType,
  userHeaders?: Record<string, string>,
): Record<string, string> {
  const initialHeaders: Record<string, any> = {
    Accept: 'text/event-stream, text/html, application/json',
    [DATASTAR_REQUEST]: true,
  }
  if (contentType === 'json') {
    initialHeaders['Content-Type'] = 'application/json'
  }
  return Object.assign({}, initialHeaders, userHeaders)
}

export function collectFormData(
  ctx: RuntimeContext,
  selector: string | undefined,
  method: string,
  formEl: HTMLFormElement | null,
): FormCollectionResult {
  const { el, evt, runtimeErr } = ctx
  let cleanup = () => {}

  if (!formEl) {
    throw runtimeErr(selector ? 'FormNotFound' : 'ClosestFormNotFound', {
      method,
      selector,
    })
  }

  if (!formEl.checkValidity()) {
    formEl.reportValidity()
    return { body: null, headers: {}, cleanup }
  }

  const formData = new FormData(formEl)
  let submitter = el as HTMLElement | null

  if (el === formEl && evt instanceof SubmitEvent) {
    submitter = evt.submitter
  } else {
    const preventDefault = (evt: Event) => evt.preventDefault()
    formEl.addEventListener('submit', preventDefault)
    cleanup = () => formEl.removeEventListener('submit', preventDefault)
  }

  if (submitter instanceof HTMLButtonElement) {
    const name = submitter.getAttribute('name')
    if (name) formData.append(name, submitter.value)
  }

  const multipart = formEl.getAttribute('enctype') === 'multipart/form-data'
  const headers: Record<string, string> = {}
  let body: string | FormData | URLSearchParams | null = null

  if (method.toUpperCase() === 'GET') {
    body = null
  } else if (multipart) {
    body = formData
  } else {
    headers['Content-Type'] = 'application/x-www-form-urlencoded'
    body = new URLSearchParams(formData as any)
  }

  return { body, headers, cleanup }
}

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
  abort?: AbortSignal
  responseOverrides?: ResponseOverrides
  contentType?: 'json' | 'form'
  filterSignals?: SignalFilterOptions
  selector?: string
}

export const sse = async (
  { el, evt, filtered, runtimeErr }: RuntimeContext,
  method: string,
  url: string,
  {
    selector,
    headers: userHeaders,
    contentType = 'json',
    filterSignals = { include: /.*/, exclude: /(^|\.)_/ },
    openWhenHidden = false,
    retryInterval = DefaultSseRetryDurationMs,
    retryScaler = 2,
    retryMaxWaitMs = 30_000,
    retryMaxCount = 10,
    abort,
  }: FetchArgs = {},
) => {
  const action = method.toLowerCase()
  let cleanupFn = () => {}

  try {
    if (!url?.length) {
      throw runtimeErr('SseNoUrlProvided', { action })
    }

    const headers = setupHeaders(contentType, userHeaders)
    const urlInstance = new URL(url, window.location.href)
    const queryParams = new URLSearchParams(urlInstance.search)

    let body: any = null

    if (contentType === 'json') {
      const res = JSON.stringify(filtered(filterSignals))
      if (method === 'GET') {
        queryParams.set(DATASTAR, res)
      } else {
        body = res
      }
    } else if (contentType === 'form') {
      const formEl = (
        selector ? document.querySelector(selector) : el.closest('form')
      ) as HTMLFormElement

      const formResult = collectFormData(
        { el, evt, filtered, runtimeErr } as RuntimeContext,
        selector,
        method,
        formEl,
      )
      cleanupFn = formResult.cleanup

      if (!formResult.body && method.toUpperCase() !== 'GET') {
        return
      }

      Object.assign(headers, formResult.headers)

      if (method === 'GET' && formResult.body instanceof URLSearchParams) {
        for (const [key, value] of formResult.body) {
          queryParams.append(key, value)
        }
      } else {
        body = formResult.body
      }
    } else {
      throw runtimeErr('SseInvalidContentType', { action, contentType })
    }

    let duplex: 'half' | undefined
    // Upload progress events are only available for: HTTPS connections (required for streaming uploads) withMultipart form data uploads
    if (__USE_UPLOAD_PROGRESS__) {
      if (urlInstance.protocol === 'https:' && body instanceof FormData) {
        const boundary = `----FormDataBoundary${Math.random().toString(36).substring(2, 11)}`
        const encoder = new TextEncoder()

        // Calculate size
        let total = 0
        const parts: Array<{ field: string; value: string | File }> = []

        for (const [name, value] of body) {
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

        body = new ReadableStream({
          async start(controller) {
            const write = (data: Uint8Array) => {
              controller.enqueue(data)
              loaded += data.byteLength

              const progress = Math.round((loaded / total) * 100)
              dispatchSSE('upload-progress', el, {
                progress,
                loaded,
                total,
              })
            }

            dispatchSSE('upload-progress', el, {
              progress: 0,
              loaded: 0,
              total: total,
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
                dispatchSSE('upload-progress', el, {
                  progress: 100,
                  loaded: total,
                  total,
                })
              }

              controller.close()
            } catch (error) {
              controller.error(error)
            }
          },
        })
        duplex = 'half'

        Object.assign(headers, {
          'Content-Type': `multipart/form-data; boundary=${boundary}`,
        })
      }
    }

    dispatchSSE(STARTED, el, {})
    urlInstance.search = queryParams.toString()

    try {
      await performSSERequest(urlInstance.toString(), el, {
        method,
        headers,
        body,
        signal: abort,
        openWhenHidden,
        retryInterval,
        retryScaler,
        retryMaxWaitMs,
        retryMaxCount,
        duplex,
      })
    } catch (error) {
      if (!isWrongContent(error)) {
        throw runtimeErr('SseFetchFailed', { method, url, error })
      }
    }
  } finally {
    dispatchSSE(FINISHED, el, {})
    cleanupFn()
  }
}

async function performSSERequest(
  url: string,
  el: HTMLOrSVG,
  options: RequestInit & {
    headers: Record<string, string>
    openWhenHidden?: boolean
    retryInterval?: number
    retryScaler?: number
    retryMaxWaitMs?: number
    retryMaxCount?: number
    duplex?: 'half'
  },
) {
  const {
    signal,
    openWhenHidden = false,
    retryInterval = 1000,
    retryScaler = 2,
    retryMaxWaitMs = 30000,
    retryMaxCount = 10,
    ...fetchOptions
  } = options
  let retries = 0
  let currentRetryInterval = retryInterval
  let controller = new AbortController()

  const visibilityHandler = () => {
    if (document.hidden && !openWhenHidden) {
      controller.abort()
    }
  }
  document.addEventListener('visibilitychange', visibilityHandler)

  const cleanup = () => {
    document.removeEventListener('visibilitychange', visibilityHandler)
    controller.abort()
  }

  signal?.addEventListener('abort', cleanup)

  async function attempt(): Promise<void> {
    controller = new AbortController()

    try {
      const response = await fetch(url, {
        ...fetchOptions,
        signal: controller.signal,
      })

      if (response.status >= 400) {
        dispatchSSE(ERROR, el, { status: response.status.toString() })
      }

      retries = 0
      currentRetryInterval = retryInterval

      const ct = response.headers.get('Content-Type')

      // Handle non-SSE responses
      if (ct?.includes('text/html')) {
        const html = await response.text()
        const argsRaw: Record<string, string> = { elements: html }
        for (const key of ['selector', 'mode', 'useViewTransition']) {
          const value = response.headers.get(`datastar-${kebab(key)}`)
          if (value) argsRaw[key] = value
        }
        dispatchSSE(EventTypePatchElements, el, argsRaw)
      } else if (ct?.includes('application/json')) {
        const json = await response.text()
        const argsRaw: Record<string, string> = { signals: json }
        for (const key of ['onlyIfMissing', 'mode', 'useViewTransition']) {
          const value = response.headers.get(`datastar-${kebab(key)}`)
          if (value) argsRaw[key] = value
        }
        dispatchSSE(EventTypePatchSignals, el, argsRaw)
      } else if (ct?.includes('text/javascript')) {
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
      } else {
        // Handle SSE stream
        await processSSEStream(response, el, (retry) => {
          currentRetryInterval = retry
        })
      }

      cleanup()
    } catch (err) {
      if (!controller.signal.aborted) {
        if (++retries >= retryMaxCount) {
          dispatchSSE(RETRIES_FAILED, el, {})
          cleanup()
          throw new Error('Max retries reached')
        }

        dispatchSSE(RETRYING, el, { message: `${err}` })
        currentRetryInterval = Math.min(
          currentRetryInterval * retryScaler,
          retryMaxWaitMs,
        )

        await new Promise((resolve) =>
          setTimeout(resolve, currentRetryInterval),
        )
        return attempt()
      }
    }
  }

  return attempt()
}

async function processSSEStream(
  response: Response,
  el: HTMLOrSVG,
  onRetry: (retry: number) => void,
) {
  const reader = response.body!.getReader()
  const decoder = new TextDecoder()
  let buffer = ''

  while (true) {
    const { done, value } = await reader.read()
    if (done) break

    buffer += decoder.decode(value, { stream: true })
    const lines = buffer.split('\n')
    buffer = lines.pop() || ''

    let lineIndex = 0
    while (lineIndex < lines.length) {
      // Process SSE messages - collect lines until empty line
      let currentEvent = ''
      const dataLines: string[] = []
      let retry: number | undefined

      // Collect all lines for this message until empty line
      while (lineIndex < lines.length && lines[lineIndex].length > 0) {
        const line = lines[lineIndex]

        if (line.startsWith('event:')) {
          currentEvent = line.slice(6).trimStart()
        } else if (line.startsWith('data:')) {
          dataLines.push(line.slice(5).trimStart())
        } else if (line.startsWith('retry:')) {
          const retryValue = Number.parseInt(line.slice(6).trimStart())
          if (!Number.isNaN(retryValue)) retry = retryValue
        }
        // Ignore other fields like id:, or lines without colons

        lineIndex++
      }

      // Process the collected message if it's a datastar event with data
      if (currentEvent.includes(DATASTAR) && dataLines.length > 0) {
        const argsRaw: Record<string, string[]> = {}

        // Collect all values for each key
        for (const dataLine of dataLines) {
          const spaceIndex = dataLine.indexOf(' ')
          if (spaceIndex > 0) {
            const key = dataLine.slice(0, spaceIndex)
            const value = dataLine.slice(spaceIndex + 1)
            if (!argsRaw[key]) {
              argsRaw[key] = []
            }
            argsRaw[key].push(value)
          }
        }

        // Join multi-line values with newlines
        const argsRawJoined: Record<string, string> = {}
        for (const [key, values] of Object.entries(argsRaw)) {
          argsRawJoined[key] = values.join('\n')
        }

        dispatchSSE(currentEvent, el, argsRawJoined)
      }

      // Handle retry if present
      if (retry !== undefined) {
        onRetry(retry)
      }

      // Skip the empty line
      lineIndex++
    }
  }
}
