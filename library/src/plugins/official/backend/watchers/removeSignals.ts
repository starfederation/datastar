// Icon: material-symbols:settings-input-antenna
// Slug: Remove signals using a Server-Sent Event
// Description: Remember, SSE is just a regular SSE request but with the ability to send 0-inf messages to the client.

import { EventTypes } from '~/engine/consts'
import { dsErr } from '~/engine/errors'
import { PluginType, type WatcherPlugin } from '~/engine/types'
import { datastarSSEEventWatcher } from '../shared'

export const RemoveSignals: WatcherPlugin = {
  type: PluginType.Watcher,
  name: EventTypes.RemoveSignals,
  onGlobalInit: async (ctx) => {
    datastarSSEEventWatcher(
      EventTypes.RemoveSignals,
      ({ paths: pathsRaw = '' }) => {
        const paths = pathsRaw.split('\n').map((p) => p.trim())
        if (!paths?.length) {
          throw dsErr('NoPathsProvided')
        }
        ctx.signals.remove(...paths)
        ctx.apply(document.body)
      },
    )
  },
}
