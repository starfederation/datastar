import { DSP, DSS } from '~/engine/consts'
import {
  type AttributePlugin,
  type NestedValues,
  PluginType,
  Requirement,
} from '~/engine/types'
import { jsStrToObject } from '~/utils/text'

export const Signals: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'signals',
  valReq: Requirement.Must,
  removeOnLoad: true,
  macros: {
    pre: [
      {
        name: 'ObjectKeyEscaper',
        type: PluginType.Macro,
        fn: (_, original) => {
          let revised = original
          for (const quote of ["'", '"', '`']) {
            const keyEscaper = new RegExp(
              `${quote}([^${quote}:]*)${quote}\s*:`,
              'gm',
            )
            revised = revised.replaceAll(keyEscaper, `"${DSP}$1${DSS}":`)
          }
          return revised
        },
      },
    ],
    post: [
      {
        name: 'ObjectKeyUnescaper',
        type: PluginType.Macro,
        fn: (_, original) => {
          const keyUnescaper = RegExp(`(?:"${DSP})([\\w_]+)(?:${DSS}":)`, 'gm')
          const revised = original.replaceAll(keyUnescaper, '"$1":')
          return revised
        },
      },
    ],
  },
  onLoad: (ctx) => {
    const { key, genRX, signals, mods } = ctx
    const ifMissing = mods.has('ifmissing')
    if (key !== '' && !ifMissing) {
      signals.setValue(key, genRX()())
    } else {
      const obj = jsStrToObject(ctx.value)
      ctx.value = JSON.stringify(obj)
      const rx = genRX()
      const nv = rx<NestedValues>()
      signals.merge(nv, ifMissing)
    }
  },
}
