// Icon: material-symbols:settings-input-antenna
// Slug: Patches signals.
// Description: Patches signals.

import { watcher } from '@engine'
import { mergePatch } from '@engine/signals'
import { jsStrToObject } from '@utils/text'

watcher({
  name: 'datastar-patch-signals',
  apply({ error }, { signals, onlyIfMissing }) {
    if (signals) {
      const ifMissing = onlyIfMissing?.trim() === 'true'
      mergePatch(jsStrToObject(signals), { ifMissing })
    } else {
      throw error('PatchSignalsExpectedSignals')
    }
  },
})
