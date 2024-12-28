import { DSP, DSS } from '~/engine/consts'
import { type MacroPlugin, PluginType } from '~/engine/types'

const tagTemplateRe = /`(.*?)`/gm
export const TagTemplateEscaperMacro: MacroPlugin = {
  name: 'TagTemplateEscaper',
  type: PluginType.Macro,
  fn: (ctx, original) => {
    // const tagTemplateLiteralRe= /`(.*?)`/gm
    const isTagTemplate = tagTemplateRe.test(original)
    console.log('isTagTemplate', isTagTemplate)
    if (!isTagTemplate) return original

    const signalPaths = new Array<string>()
    ctx.signals.walk((path) => signalPaths.push(path))
    if (!signalPaths.length) return original
    const signalRe = new RegExp(
      `(?!${DSP})(${signalPaths.join('|')})(?!${DSS})`,
      'gm',
    )

    // const tagTemplateEscaper =
    //   /(?<preexpr>.*?)\${(?<expr>.*?)}(?<postexpr>.*?)/gm
    const expressionPartsRe = /(?:\${)(?<expr>.*?)(?:})/gm
    const escaped = `${DSP}$1${DSS}`
    const escapeTagTempls = (s: string) => {
      const revisedParts = new Array<string>()
      for (const match of s.matchAll(expressionPartsRe)) {
        // preexpression
        const preexpression = s
          .slice(0, match.index + 2)
          .replaceAll(signalRe, escaped)
        revisedParts.push(preexpression)

        // expression
        const expression = match.groups?.expr || ''
        revisedParts.push(expression)

        // postexpression
        const postexpression = s
          .slice(match[0].length + match.index - 1)
          .replaceAll(signalRe, escaped)
        revisedParts.push(postexpression)
      }
      if (!revisedParts.length) return s
      return revisedParts.join('')
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
