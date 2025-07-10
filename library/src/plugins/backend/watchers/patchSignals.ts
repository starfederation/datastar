// Icon: material-symbols:settings-input-antenna
// Slug: Patches signals.
// Description: Patches signals.

import {
  DefaultPatchSignalsOnlyIfMissing,
  EventTypePatchSignals,
} from '../../../engine/consts'
import type { WatcherPlugin } from '../../../engine/types'
import { isBoolString, jsStrToObject } from '../../../utils/text'
import { datastarSSEEventWatcher } from '../shared'

export const PatchSignals: WatcherPlugin = {
  type: 'watcher',
  name: EventTypePatchSignals,
  onGlobalInit: (ctx) =>
    datastarSSEEventWatcher(
      EventTypePatchSignals,
      ({
        signals: raw = '{}',
        onlyIfMissing: onlyIfMissingRaw = `${DefaultPatchSignalsOnlyIfMissing}`,
      }) =>
        ctx.mergePatch(jsStrToObject(raw), {
          ifMissing: isBoolString(onlyIfMissingRaw),
        }),
    ),
}
