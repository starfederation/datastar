// Authors: Delaney Gillilan
// Icon: ion:checkmark-round
// Slug: Set all signals that match a regular expression

import type { Signal } from '../../../../engine/signals'
import { type ActionPlugin, PluginType } from '../../../../engine/types'

export const SetAll: ActionPlugin = {
  type: PluginType.Action,
  name: 'setAll',
  fn: ({ signals }, prefix: string, newValue) => {
    signals.walk((path, signal) => {
      if (!path.startsWith(prefix)) {
        return
      }
      (signal as Signal<any>).value = newValue
    })
  },
}
