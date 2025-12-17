// Icon: material-symbols:toggle-off
// Slug: Toggles the value of all matching signals.
// Description: Toggles the boolean value of all matching signals (or all signals if no filter is used).

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
  name: 'toggleAll',
  apply(_, filter: SignalFilterOptions) {
    // peek because in an effect you would be subscribing to signals and then setting them which
    // would cause an infinite loop and why would you want to infinite loop on purpose
    startPeeking()
    const masked = filtered(filter)
    updateLeaves(masked, (oldValue: any) => !oldValue)
    mergePatch(masked)
    stopPeeking()
  },
})
