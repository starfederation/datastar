// Icon: material-symbols:timer-play-outline
// Slug: Runs an expression when loaded into the DOM.
// Description: Runs an expression when the element is loaded into the DOM.

import type { AttributePlugin } from '../../engine/types'
import { tagToMs } from '../../utils/tags'
import { delay } from '../../utils/timing'
import { modifyViewTransition } from '../../utils/view-transitions'

export const OnLoad: AttributePlugin = {
  type: 'attribute',
  name: 'onLoad',
  keyReq: 'denied',
  valReq: 'must',
  onLoad: ({ rx, mods, startBatch, endBatch }) => {
    let callback = () => {
      startBatch()
      rx()
      endBatch()
    }
    callback = modifyViewTransition(callback, mods)
    let wait = 0
    const delayArgs = mods.get('delay')
    if (delayArgs) {
      wait = tagToMs(delayArgs)
      if (wait > 0) {
        callback = delay(callback, wait)
      }
    }
    callback()
  },
}
