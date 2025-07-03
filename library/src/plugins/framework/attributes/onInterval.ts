// Icon: material-symbols:timer-outline
// Slug: Runs an expression on an interval
// Description: This attribute runs an expression on an interval. The interval can be set to a specific duration, and can be set to trigger immediately.

import type { AttributePlugin } from '../../../engine/types'
import { tagHas, tagToMs } from '../../../utils/tags'
import { modifyViewTransition } from '../../../utils/view-transitions'

export const OnInterval: AttributePlugin = {
  type: 'attribute',
  name: 'onInterval',
  keyReq: 'denied',
  valReq: 'must',
  onLoad: ({ mods, rx, startBatch, endBatch }) => {
    let callback = () => {
      startBatch()
      rx()
      endBatch()
    }
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
