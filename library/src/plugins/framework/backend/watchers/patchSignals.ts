// Icon: material-symbols:settings-input-antenna
// Slug: Patch signals using a Server-Sent Event
// Description: Remember, SSE is just a regular SSE request but with the ability to send 0-inf messages to the client.

import {
  DefaultPatchSignalsOnlyIfMissing,
  EventTypePatchSignals,
} from '../../../../engine/consts'
import type { WatcherPlugin } from '../../../../engine/types'
import { isBoolString, jsStrToObject } from '../../../../utils/text'
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
