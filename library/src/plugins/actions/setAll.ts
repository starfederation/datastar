// Icon: ion:checkmark-round
// Slug: Sets the value of all matching signals.
// Description: Sets the value of all matching signals (or all signals if no filter is used) to the expression provided in the first argument.

import type {
  ActionPlugin,
  RuntimeContext,
  SignalFilterOptions,
} from '../../engine/types'
import { updateLeaves } from '../../utils/paths'

export const SetAll: ActionPlugin = {
  type: 'action',
  name: 'setAll',
  fn: (
    { filtered, mergePatch, peek }: RuntimeContext,
    value: any,
    filter: SignalFilterOptions,
  ) => {
    peek(() => {
      const masked = filtered(filter)
      updateLeaves(masked, () => value)
      mergePatch(masked)
    })
  },
}
