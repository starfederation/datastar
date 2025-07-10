// Icon: material-symbols:network-wifi
// Slug: Sets the indicator signal used when fetching data via SSE
// Description: must be a valid signal name

import type { AttributePlugin } from '../../../engine/types'
import { pathToObj } from '../../../utils/paths'
import { modifyCasing, modifyScope } from '../../../utils/text'
import {
  DATASTAR_SSE_EVENT,
  type DatastarSSEEvent,
  FINISHED,
  STARTED,
} from '../backend/shared'

export const Indicator: AttributePlugin = {
  type: 'attribute',
  name: 'indicator',
  keyReq: 'exclusive',
  valReq: 'exclusive',
  onLoad: ({ el, key, mods, mergePatch, value }) => {
    let signalName = key ? modifyCasing(key, mods) : value
    signalName = modifyScope(signalName, el, mods)

    mergePatch(pathToObj({}, { [signalName]: false }), { ifMissing: true })

    const watcher = ((event: CustomEvent<DatastarSSEEvent>) => {
      const { type, el: elt } = event.detail
      if (elt !== el) {
        return
      }
      switch (type) {
        case STARTED:
          mergePatch(pathToObj({}, { [signalName]: true }))
          break
        case FINISHED:
          mergePatch(pathToObj({}, { [signalName]: false }))
          break
      }
    }) as EventListener
    document.addEventListener(DATASTAR_SSE_EVENT, watcher)
    return () => {
      mergePatch(pathToObj({}, { [signalName]: false }))
      document.removeEventListener(DATASTAR_SSE_EVENT, watcher)
    }
  },
}
