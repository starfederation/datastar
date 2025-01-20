import { kebabize } from '../utils/text'
import { DATASTAR } from './consts'
import { type InitContext, PluginType, type RuntimeContext } from './types'

// const url = 'https://data-star.dev/errors'
const url = `${window.location.origin}/errors`

function dserr(type: string, reason: string, metadata = {}) {
  const e = new Error()
  reason = reason[0].toUpperCase() + reason.slice(1)
  e.name = `${DATASTAR} ${type} error ${reason}`
  const r = kebabize(reason).replaceAll('-', '_')
  const q = new URLSearchParams({
    metadata: JSON.stringify(metadata),
  }).toString()
  e.message = `for more info see ${url}/${type}/${r}?${q}`
  return e
}

export function internalErr(from: string, reason: string, args = {}) {
  return dserr('internal', reason, Object.assign({ from }, args))
}

export function initErr(reason: string, ctx: InitContext, metadata = {}) {
  const errCtx = {
    plugin: {
      name: ctx.plugin.name,
      type: PluginType[ctx.plugin.type],
    },
  }
  return dserr('init', reason, Object.assign(errCtx, metadata))
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
    expression: {
      rawKey: ctx.rawKey,
      key: ctx.key,
      value: ctx.value,
      validSignals: ctx.signals.paths(),
      fnContent: ctx.fnContent,
    },
  }
  return dserr('runtime', reason, Object.assign(errCtx, metadata))
}