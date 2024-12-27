// Authors: Delaney Gillilan
// Icon: mdi:cursor-pointer
// Slug: Create a reference to an element
// Description: This attribute creates a reference to an element that can be used in other expressions.

import { type AttributePlugin, PluginType, Requirement } from '~/engine/types'
import { SignalIgnoreMacros } from '../../core/macros/signals'

// Sets the value of the element
export const Ref: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'ref',
  keyReq: Requirement.Exclusive,
  valReq: Requirement.Exclusive,
  macros: SignalIgnoreMacros,
  onLoad: ({ el, key, value, signals }) => {
    const signalName = key ? key : value
    signals.upsert(signalName, el)
    return () => signals.setValue(signalName, null)
  },
}
