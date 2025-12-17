// Icon: ion:checkmark-round
// Slug: Sets the value of all matching signals.
// Description: Sets the value of all matching signals (or all signals if no filter is used) to the expression provided in the first argument.

import { action } from '@engine'
import {
  filtered,
  mergePatch,
  startPeeking,
  stopPeeking,
} from '@engine/signals'
import type { SignalFilterOptions } from '@engine/types'
import { updateLeaves } from '@utils/paths'

action({
  name: 'setAll',
  apply(_, value: any, filter: SignalFilterOptions) {
    // peek because in an effect you would be subscribing to signals and then setting them which
    // would cause an infinite loop and why would you want to infinite loop on purpose
    startPeeking()
    const masked = filtered(filter)
    updateLeaves(masked, () => value)
    mergePatch(masked)
    stopPeeking()
  },
})
