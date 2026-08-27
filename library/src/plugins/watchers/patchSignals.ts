// Icon: material-symbols:settings-input-antenna
// Slug: Patches signals.
// Description: Patches signals.

import { watcher } from '@engine'
import { mergePatch } from '@engine/signals'
import { isStringTrue, jsStrToObject } from '@utils/text'

watcher({
  name: 'datastar-patch-signals',
  apply({ error }, { signals, onlyIfMissing }) {
    if (typeof signals !== 'string') {
      throw error('PatchSignalsExpectedSignals')
    }

    const ifMissing = isStringTrue(onlyIfMissing)
    mergePatch(jsStrToObject(signals), { ifMissing })
  },
})
