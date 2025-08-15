// Icon: streamline-ultimate:wifi-signal-2
// Slug: Creates a computed signal.
// Description: Creates a signal that is computed based on an expression.

import type { AttributePlugin } from '../../engine/types'
import { modifyCasing } from '../../utils/text'

export const Computed: AttributePlugin = {
  type: 'attribute',
  name: 'computed',
  keyReq: 'must',
  valReq: 'must',
  returnsValue: true,
  onLoad: ({ key, mods, rx, computed, mergePaths }) => {
    mergePaths([[modifyCasing(key, mods), computed(rx)]])
  },
}
