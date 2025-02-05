import {
  type AttributePlugin,
  type NestedValues,
  PluginType,
} from '../../../../engine/types'
import { jsStrToObject, modifyCasing } from '../../../../utils/text'

export const Signals: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'signals',
  removeOnLoad: () => true,
  onLoad: (ctx) => {
    const { key, mods, signals, value, genRX } = ctx
    const ifMissing = mods.has('ifmissing')
    // BEN: investigate use of `ifMissing` in the following line
    if (key !== '' && !ifMissing) {
      const k = modifyCasing(key, mods)
      const v = value === '' ? value : genRX()()
      signals.setValue(k, v)
    } else {
      const obj = jsStrToObject(ctx.value)
      ctx.value = JSON.stringify(obj)
      const rx = genRX()
      const nv = rx<NestedValues>()
      signals.merge(nv, ifMissing)
    }
  },
}
