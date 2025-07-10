// Icon: streamline:wifi-signal-full-remix
// Slug: Patches signals into the existing signals.
// Description: Patches (adds, updates or removes) one or more signals into the existing signals.

import type { AttributePlugin } from '../../engine/types'
import { pathToObj } from '../../utils/paths'
import { modifyCasing } from '../../utils/text'

export const Signals: AttributePlugin = {
  type: 'attribute',
  name: 'signals',
  returnsValue: true,
  onLoad: ({ key, mods, rx, mergePatch }) => {
    const ifMissing = mods.has('ifmissing')

    if (key) {
      key = modifyCasing(key, mods)
      mergePatch(pathToObj({}, { [key]: rx() }), { ifMissing })
    } else {
      const patch = rx<Record<string, any>>()
      const pathObj: Record<string, any> = {}
      for (const key in patch) {
        pathObj[key] = patch[key]
      }
      mergePatch(pathToObj({}, pathObj), { ifMissing })
    }
  },
}
