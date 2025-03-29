// Authors: Delaney Gillilan
// Icon: material-symbols:toggle-off
// Slug: Toggle all signals that match the signal path
// Description: Toggle all signals that match one or more space-separated paths in which `*` can be used as a wildcard

import { type ActionPlugin, PluginType } from '../../../../engine/types'
import { pathMatchesPattern } from '../../../../utils/paths'
import { trimDollarSignPrefix } from '../../../../utils/text'

export const ToggleAll: ActionPlugin = {
  type: PluginType.Action,
  name: 'toggleAll',
  fn: ({ signals }, paths: string) => {
    let patterns = paths.split(/\s+/).filter((p) => p !== '')
    patterns = patterns.map((p) => trimDollarSignPrefix(p))

    for (const pattern of patterns) {
      signals.walk((path, signal) => {
        if (pathMatchesPattern(path, pattern)) {
          signal.value = !signal.value
        }
      })
    }
  },
}
