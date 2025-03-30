// Authors: Delaney Gillilan
// Icon: material-symbols:toggle-off
// Slug: Toggle all signals that match the signal path
// Description: Toggle all signals that match one or more space-separated paths in which `*` can be used as a wildcard

import { type ActionPlugin, PluginType } from '../../../../engine/types'
import { applyToMatchingPaths } from '../../../../utils/paths'

export const ToggleAll: ActionPlugin = {
  type: PluginType.Action,
  name: 'toggleAll',
  fn: ({ signals }, paths: string) => {
    applyToMatchingPaths(signals, paths, (signal) => {
      signal.value = !signal.value
    })
  },
}
