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
  onLoad: (ctx) => {
    const { key, genRX, signals, tags } = ctx
    const ifMissing = tags.has('ifmissing')
    if (key !== '' && !ifMissing) {
      signals.setValue(key, genRX()())
    } else {
      const obj = jsStrToObject(ctx.value)
      ctx.value = JSON.stringify(obj)
      signals.merge(genRX()<NestedValues>(), ifMissing)
    }
  },
}
