// Icon: material-symbols:toggle-off
// Slug: Toggle all signals that match the signal path
// Description: Toggle all signals that match one or more space-separated paths in which `*` can be used as a wildcard

import type {
  ActionPlugin,
  RuntimeContext,
  SignalFilterOptions,
} from '../../../engine/types'
import { updateLeaves } from '../../../utils/paths'

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
