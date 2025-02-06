import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { modifyCasing } from '../../../../utils/text'

const name = 'computed'
export const Computed: AttributePlugin = {
  type: PluginType.Attribute,
  name,
  keyReq: Requirement.Must,
  valReq: Requirement.Must,
  onLoad: ({ key, mods, signals, genRX }) => {
    key = modifyCasing(key, mods)
    const rx = genRX()
    signals.setComputed(key, rx)
  },
}
