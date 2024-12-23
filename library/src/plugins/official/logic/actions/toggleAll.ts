// Authors: Delaney Gillilan
// Icon: material-symbols:toggle-off
// Slug: Toggle all signals that match a regular expression

import { type ActionPlugin, PluginType } from '~/engine/types'

export const ToggleAll: ActionPlugin = {
  type: PluginType.Action,
  name: 'toggleAll',
  fn: ({ signals }, prefix: string) => {
    signals.walk((path, signal) => {
      if (!path.startsWith(prefix)) return
      signal.value = !signal.value
    })
  },
}
