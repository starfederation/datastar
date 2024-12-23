import { type MacroPlugin, PluginType } from '~/engine/types'

export const SignalValueMacro: MacroPlugin = {
  name: 'signalValue',
  type: PluginType.Macro,
  fn: (original: string) => {
    const validJS = /(?<path>[\w0-9.]*)((\.value))/gm
    const sub = `ctx.signals.signal('$1').value`
    return original.replaceAll(validJS, sub)
  },
}
