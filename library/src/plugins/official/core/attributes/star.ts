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
  onLoad: ({ signals }) => {
    //alert('YOU ARE PROBABLY OVERCOMPLICATING IT')
    const value = signals.signal('result')?.value as number
    signals.setValue('result', value + 1)
  },
}
