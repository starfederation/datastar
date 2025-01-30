import {
  type AttributePlugin,
  type NestedValues,
  PluginType,
} from '../../../../engine/types'
import { jsStrToObject } from '../../../../utils/text'

export const Signals: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'signals',
  onLoad: (ctx) => {
    const { key, value, genRX, signals, mods } = ctx
    const ifMissing = mods.has('ifmissing')
    if (key !== '' && !ifMissing) {
      const v = value === '' ? value : genRX()()
      signals.setValue(key, v)
    } else {
      const obj = jsStrToObject(ctx.value)
      ctx.value = JSON.stringify(obj)
      const rx = genRX()
      const nv = rx<NestedValues>()
      signals.merge(nv, ifMissing)
    }
  },
}
