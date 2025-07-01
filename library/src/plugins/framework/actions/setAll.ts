// Icon: ion:checkmark-round
// Slug: Set all signals that match the signal path
// Description: Set all signals that match one or more space-separated paths in which `*` can be used as a wildcard

import type {
  ActionPlugin,
  RuntimeContext,
  SignalFilterOptions,
} from '../../../engine/types'
import { updateLeaves } from '../../../utils/paths'

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
