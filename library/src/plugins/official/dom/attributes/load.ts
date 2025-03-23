// Authors: Ben Croker
// Icon: material-symbols:mail
// Slug: Executes and expression when the element is loaded
// Description: This plugin executes an expression when the element is loaded.

import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { modifyTiming } from '../../../../utils/timing'
import { modifyViewTransition } from '../../../../utils/view-transtions'

export const Load: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'load',
  keyReq: Requirement.Denied,
  valReq: Requirement.Must,
  onLoad: ({ mods, genRX }) => {
    const rx = genRX()
    let callback = () => rx()
    callback = modifyTiming(callback, mods)
    callback = modifyViewTransition(callback, mods)

    // Delay the callback to the next macrotask so that indicators can be set
    setTimeout(callback)

    return () => {}
  },
}
