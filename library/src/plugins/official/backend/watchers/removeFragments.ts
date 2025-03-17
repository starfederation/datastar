// Icon: material-symbols:settings-input-antenna
// Slug: Remove fragments from the DOM using a Server-Sent Event
// Description: Remember, SSE is just a regular SSE request but with the ability to send 0-inf messages to the client.

import {
  DefaultFragmentsUseViewTransitions,
  EventTypes,
} from '../../../../engine/consts'
import { initErr } from '../../../../engine/errors'
import { PluginType, type WatcherPlugin } from '../../../../engine/types'
import { isBoolString } from '../../../../utils/text'
import {
  docWithViewTransitionAPI,
  supportsViewTransitions,
} from '../../../../utils/view-transtions'
import { datastarSSEEventWatcher } from '../shared'

export const RemoveFragments: WatcherPlugin = {
  type: PluginType.Watcher,
  name: EventTypes.RemoveFragments,
  onGlobalInit: async (ctx) => {
    datastarSSEEventWatcher(
      EventTypes.RemoveFragments,
      ({
        selector,
        useViewTransition:
          useViewTransitionRaw = `${DefaultFragmentsUseViewTransitions}`,
      }) => {
        if (!selector.length) {
          throw initErr('NoSelectorProvided', ctx)
        }

        const useViewTransition = isBoolString(useViewTransitionRaw)
        const removeTargets = document.querySelectorAll(selector)

        const applyToTargets = () => {
          for (const target of removeTargets) {
            target.remove()
          }
        }

        if (useViewTransition && supportsViewTransitions) {
          docWithViewTransitionAPI.startViewTransition(() => applyToTargets())
        } else {
          applyToTargets()
        }
      },
    )
  },
}
