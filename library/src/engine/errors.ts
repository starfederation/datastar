import { snake } from '../utils/text'
import { DATASTAR } from './consts'
import type { InitContext, RuntimeContext } from './types'

const url = 'https://data-star.dev/errors'

interface Metadata {
  error?: string
  [key: string]: any
}

function dserr(type: string, reason: string, metadata: Metadata = {}) {
  const e = new Error()
  e.name = `${DATASTAR} ${type} error`
  const r = snake(reason)
  const q = new URLSearchParams({
    metadata: JSON.stringify(metadata),
  }).toString()
  const c = JSON.stringify(metadata, null, 2)
  e.message = `${reason}\nMore info: ${url}/${type}/${r}?${q}\nContext: ${c}`
  return e
}

export function internalErr(from: string, reason: string, args = {}) {
  return dserr('internal', reason, Object.assign({ from }, args))
}

export function initErr(ctx: InitContext, reason: string, metadata = {}) {
  const errCtx = {
    plugin: {
      name: ctx.plugin.name,
      type: ctx.plugin.type,
    },
  }
  return dserr('init', reason, Object.assign(errCtx, metadata))
}

export function runtimeErr(ctx: RuntimeContext, reason: string, metadata = {}) {
  const errCtx = {
    plugin: {
      name: ctx.plugin.name,
      type: ctx.plugin.type,
    },
    element: {
      id: ctx.el.id,
      tag: ctx.el.tagName,
    },
    expression: {
      rawKey: ctx.rawKey,
      key: ctx.key,
      value: ctx.value,
      // validSignals:
      fnContent: ctx.fnContent,
    },
  }
  return dserr('runtime', reason, Object.assign(errCtx, metadata))
}
