import { kebabize } from '../utils/text'
import { DATASTAR } from './consts'
import { type InitContext, PluginType, type RuntimeContext } from './types'

// const url = 'https://data-star.dev/errors'
const url = 'http://localhost:8080/errors'

export const hasValNonExpr = /([\w0-9.]+)\.value/gm

function dserr(type: string, reason: string, metadata = {}) {
  const e = new Error()
  e.name = `${DATASTAR} ${type} error ${reason}`
  const r = kebabize(reason).replaceAll('-', '_')
  const q = new URLSearchParams({
    metadata: JSON.stringify(metadata),
  }).toString()
  e.message = `for more info see ${url}/${type}/${r}?${q}`
  return e
}

export function internalErr(reason: string, args = {}) {
  return dserr('internal', reason, args)
}

export function initErr(reason: string, ctx: InitContext, metadata = {}) {
  const errCtx = {
    plugin: {
      name: ctx.plugin.name,
      type: PluginType[ctx.plugin.type],
    },
  }
  return dserr('init', reason, Object.assign(errCtx, { metadata }))
}

export function runtimeErr(reason: string, ctx: RuntimeContext, metadata = {}) {
  const errCtx = {
    plugin: {
      name: ctx.plugin.name,
      type: PluginType[ctx.plugin.type],
    },
    element: {
      id: ctx.el.id,
      tag: ctx.el.tagName,
    },
    raw: {
      key: ctx.rawKey,
      value: ctx.rawValue,
    },
    expression: {
      key: ctx.key,
      value: ctx.value,
      validSignals: ctx.signals.paths(),
      fnContent: ctx.fnContent,
    },
  }
  return dserr('runtime', reason, Object.assign(errCtx, metadata))
}
