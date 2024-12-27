import { DSP, DSS } from '~/engine/consts'
import { type MacroPlugin, PluginType } from '~/engine/types'

export const SignalValueMacro: MacroPlugin = {
  name: 'signalValue',
  type: PluginType.Macro,
  fn: (ctx, original) => {
    const signalPaths = new Array<string>()
    ctx.signals.walk((path) => signalPaths.push(path))
    if (!signalPaths.length) return original
    const signalRegx = new RegExp(
      `(?!${DSP})(${signalPaths.map((p) => `${p}(?=[\\s.=)}]+|$)`).join('|')})(?!${DSS})`,
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
    const keyUnescaper = new RegExp(`${DSP}(.+)${DSS}`, 'gm')
    const revised = original.replaceAll(keyUnescaper, '$1')
    return revised
  },
}

export const SignalIgnoreMacros: Record<string, MacroPlugin[]> = Object.freeze({
  pre: [IngnoreAnythingMacro],
  post: [UnignoreAnythingMacro],
})
