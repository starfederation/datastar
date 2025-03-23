// Authors: Ben Croker
// Icon: material-symbols:mail
// Slug: Executes and expression when the element is loaded
// Description: This attribute executes an expression when the element is loaded.

import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { tagToMs } from '../../../../utils/tags'

export const OnLoad: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'onLoad',
  keyReq: Requirement.Denied,
  valReq: Requirement.Must,
  onLoad: ({ mods, genRX }) => {
    const callback = genRX()

    let wait = 0
    const delayArgs = mods.get('delay')
    if (delayArgs) {
      wait = tagToMs(delayArgs)
    }

    setTimeout(callback, wait)

    return () => {}
  },
}
