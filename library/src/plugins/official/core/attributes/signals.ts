import {
  type AttributePlugin,
  type NestedValues,
  PluginType,
} from '../../../../engine/types'
import { jsStrToObject, modifyCasing } from '../../../../utils/text'

export const Signals: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'signals',
  onLoad: (ctx) => {
    const { key, mods, signals, value, genRX } = ctx
    const ifMissing = mods.has('ifmissing')
    const { rxFn } = genRX()
    if (key !== '') {
      const k = modifyCasing(key, mods)
      const v = value === '' ? value : rxFn()
      if (ifMissing) {
        signals.upsertIfMissing(k, v)
      } else {
        signals.setValue(k, v)
      }
    } else {
      const obj = jsStrToObject(ctx.value)
      ctx.value = JSON.stringify(obj)
      const nv = rxFn<NestedValues>()
      signals.merge(nv, ifMissing)
    }
  },
}
