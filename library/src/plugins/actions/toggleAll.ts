// Icon: material-symbols:toggle-off
// Slug: Toggles the value of all matching signals.
// Description: Toggles the boolean value of all matching signals (or all signals if no filter is used).

import type {
  ActionPlugin,
  RuntimeContext,
  SignalFilterOptions,
} from '../../engine/types'
import { updateLeaves } from '../../utils/paths'

export const ToggleAll: ActionPlugin = {
  type: 'action',
  name: 'toggleAll',
  fn: (
    { filtered, mergePatch, peek }: RuntimeContext,
    filter: SignalFilterOptions,
  ) => {
    peek(() => {
      const masked = filtered(filter)
      updateLeaves(masked, (oldValue: any) => !oldValue)
      mergePatch(masked)
    })
  },
}
