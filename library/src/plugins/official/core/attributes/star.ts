import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'

export const Star: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'star',
  keyReq: Requirement.Denied,
  valReq: Requirement.Denied,
  onLoad: () => {
    alert('YOU ARE PROBABLY OVERCOMPLICATING IT')
  },
}
