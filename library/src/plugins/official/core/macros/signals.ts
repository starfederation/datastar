import { DSP, DSS } from '~/engine/consts'
import { type MacroPlugin, PluginType } from '~/engine/types'

export const TagTemplateEscaperMacro: MacroPlugin = {
  name: 'TagTemplateEscaper',
  type: PluginType.Macro,
  fn: (ctx, original) => {
    const signalPaths = new Array<string>()
    ctx.signals.walk((path) => signalPaths.push(path))
    if (!signalPaths.length) return original
    const signalRe = new RegExp(
      `(?!${DSP})(${signalPaths.join('|')})(?!${DSS})`,
      'gm',
    )

    const tagTemplateEscaper =
      /`(?<preexpr>.*)(?:\$){(?<expr>.*)(?:\})(?<postexpr>.*)`/gm
    const escaped = `${DSP}$1${DSS}`
    const escapeTagTempls = (s: string) => {
      const matches = [...s.matchAll(tagTemplateEscaper)]

      let revised = s
      for (const match of matches) {
        if (!match.groups) continue
        const { preexpr, expr, postexpr } = match.groups

        const revisedPreExpr = preexpr.replaceAll(signalRe, escaped)
        const revisedExpr = escapeTagTempls(expr)
        const revisedPostExpr = postexpr.replaceAll(signalRe, escaped)

        revised = `\`${revisedPreExpr}\${${revisedExpr}}${revisedPostExpr}\``
      }
      return revised
    }
    return escapeTagTempls(original)
  },
}

export const SignalValueMacro: MacroPlugin = {
  name: 'signalValue',
  type: PluginType.Macro,
  fn: (ctx, original) => {
    if (!original.length) return original

    const signalPaths = new Array<string>()
    ctx.signals.walk((path) => signalPaths.push(path))
    if (!signalPaths.length) return original
    const signalRegx = new RegExp(
      `(?!(?:${DSP})|\\.)(?<!\\.)(${signalPaths.join('|')})(?=[\\s.=)},+*-/]+|$)(?!${DSS})`,
      'gm',
    )
    const fixed = original.replaceAll(
      signalRegx,
      `ctx.signals.signal('$1').value`,
    )
    return fixed
  },
}

export const IngnoreAnythingMacro: MacroPlugin = {
  name: 'IngnoreAnything',
  type: PluginType.Macro,
  fn: (_, original) => {
    const keyEscaper = /([\w.]+)/gm
    const revised = original.replaceAll(keyEscaper, `${DSP}$1${DSS}`)
    return revised
  },
}

export const UnignoreAnythingMacro: MacroPlugin = {
  name: 'UnignoreAnything',
  type: PluginType.Macro,
  fn: (_, original) => {
    const keyUnescaper = new RegExp(`${DSP}(.+?)${DSS}`, 'gm')
    const revised = original.replaceAll(keyUnescaper, '$1')
    return revised
  },
}
