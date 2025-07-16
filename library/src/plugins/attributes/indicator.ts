// Icon: streamline:signal-loading-remix
// Slug: Creates an indicator for whether an SSE request is in flight.
// Description: Creates a signal and sets its value to `true` while an SSE request request is in flight, otherwise `false`.

import type { AttributePlugin } from '../../engine/types'
import { pathToObj } from '../../utils/paths'
import { modifyCasing } from '../../utils/text'
import {
  DATASTAR_FETCH_EVENT,
  type DatastarFetchEvent,
  FINISHED,
  STARTED,
} from '../backend/shared'

export const Indicator: AttributePlugin = {
  type: 'attribute',
  name: 'indicator',
  keyReq: 'exclusive',
  valReq: 'exclusive',
  shouldEvaluate: false,
  onLoad: ({ el, key, mods, mergePatch, value }) => {
    const signalName = key ? modifyCasing(key, mods) : value

    mergePatch(pathToObj({}, { [signalName]: false }), { ifMissing: true })

    const watcher = ((event: CustomEvent<DatastarFetchEvent>) => {
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
    document.addEventListener(DATASTAR_FETCH_EVENT, watcher)
    return () => {
      mergePatch(pathToObj({}, { [signalName]: false }))
      document.removeEventListener(DATASTAR_FETCH_EVENT, watcher)
    }
  },
}
