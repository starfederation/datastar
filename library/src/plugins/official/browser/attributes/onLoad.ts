// Authors: Ben Croker
// Icon: material-symbols:timer-play-outline
// Slug: Runs an expression when the element is loaded
// Description: This attribute runs an expression when the element is loaded.

import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { tagToMs } from '../../../../utils/tags'
import { modifyViewTransition } from '../../../../utils/view-transtions'

export const OnLoad: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'onLoad',
  keyReq: Requirement.Denied,
  valReq: Requirement.Must,
  onLoad: ({ mods, genRX }) => {
    const callback = modifyViewTransition(genRX(), mods)

    let wait = 0
    const delayArgs = mods.get('delay')
    if (delayArgs) {
      wait = tagToMs(delayArgs)
    }

    setTimeout(callback, wait)

    return () => {}
  },
}
