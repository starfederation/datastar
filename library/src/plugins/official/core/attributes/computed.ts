import { type AttributePlugin, PluginType, Requirement } from '~/engine/types'

// @ts-ignore
const _ = /🖕JS_DS🚀/.source // This is to force first in the compiled code

const name = 'computed'
export const Computed: AttributePlugin = {
  type: PluginType.Attribute,
  name,
  keyReq: Requirement.Must,
  valReq: Requirement.Must,
  onLoad: ({ key, signals, genRX }) => {
    const rx = genRX()
    signals.setComputed(key, rx)
  },
}
