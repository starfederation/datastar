// Authors: Delaney Gillilan
// Icon: ion:checkmark-round
// Slug: Set all signals that match the signal path
// Description: Set all signals that match one or more space-separated paths in which `*` can be used as a wildcard

import { type ActionPlugin, PluginType } from '../../../../engine/types'
import { applyToMatchingPaths } from '../../../../utils/paths'

export const SetAll: ActionPlugin = {
  type: PluginType.Action,
  name: 'setAll',
  fn: ({ signals }, paths: string, newValue) => {
    applyToMatchingPaths(signals, paths, (signal) => {
      signal.value = newValue
    })
  },
}
