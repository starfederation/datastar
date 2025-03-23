// Authors: Ben Croker
// Icon: material-symbols:mail
// Slug: Executes and expression on an interval
// Description: This plugin executes an expression on an interval. The interval can be set to a specific duration, and can be set to trigger immediately.

import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { tagHas, tagToMs } from '../../../../utils/tags'
import { modifyTiming } from '../../../../utils/timing'
import { modifyViewTransition } from '../../../../utils/view-transtions'

export const Interval: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'interval',
  keyReq: Requirement.Denied,
  valReq: Requirement.Must,
  onLoad: ({ mods, genRX }) => {
    const rx = genRX()
    let callback = () => rx()
    callback = modifyTiming(callback, mods)
    callback = modifyViewTransition(callback, mods)

    let duration = 1000
    const durationArgs = mods.get('duration')
    if (durationArgs) {
      duration = tagToMs(durationArgs)
      const leading = tagHas(durationArgs, 'leading', false)
      if (leading) {
        callback()
      }
    }
    const intervalId = setInterval(callback, duration)

    return () => {
      clearInterval(intervalId)
    }
  },
}
