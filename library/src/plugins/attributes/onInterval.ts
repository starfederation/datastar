// Icon: material-symbols:timer-outline
// Slug: Runs an expression at a regular interval.
// Description: Runs an expression at a regular interval.

import { attribute } from '@engine'
import { beginBatch, endBatch } from '@engine/signals'
import { tagHas, tagToMs } from '@utils/tags'
import { modifyViewTransition } from '@utils/view-transitions'

attribute({
  name: 'on-interval',
  requirement: {
    key: 'denied',
    value: 'must',
  },
  apply({ mods, rx }) {
    let callback = () => {
      beginBatch()
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
})
