// Icon: streamline:wifi-signal-full-remix
// Slug: Patches signals into the existing signals.
// Description: Patches (adds, updates or removes) one or more signals into the existing signals.

import type { AttributePlugin } from '../../engine/types'
import { modifyCasing } from '../../utils/text'

export const Signals: AttributePlugin = {
  type: 'attribute',
  name: 'signals',
  returnsValue: true,
  onLoad: ({ key, mods, rx, mergePatch, mergePaths }) => {
    const ifMissing = mods.has('ifmissing')

    if (key) {
      key = modifyCasing(key, mods)
      mergePaths([[key, rx()]], { ifMissing })
    } else {
      const patch = Object.assign({}, rx<Record<string, any>>())
      mergePatch(patch, { ifMissing })
    }
  },
}
