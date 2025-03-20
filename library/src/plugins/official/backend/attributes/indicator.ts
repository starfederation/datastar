// Icon: material-symbols:network-wifi
// Slug: Sets the indicator signal used when fetching data via SSE
// Description: must be a valid signal name

import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { modifyCasing, trimDollarSignPrefix } from '../../../../utils/text'
import {
  DATASTAR_SSE_EVENT,
  type DatastarSSEEvent,
  ERROR,
  FINISHED,
  STARTED,
} from '../shared'

export const Indicator: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'indicator',
  keyReq: Requirement.Exclusive,
  valReq: Requirement.Exclusive,
  onLoad: ({ el, key, mods, signals, value }) => {
    const signalName = key ? modifyCasing(key, mods) : trimDollarSignPrefix(value)
    const { signal } = signals.upsertIfMissing(signalName, false)
    const watcher = ((event: CustomEvent<DatastarSSEEvent>) => {
      const {
        type,
        argsRaw: { elId },
      } = event.detail
      if (elId !== el.id) return
      switch (type) {
        case STARTED:
          signal.value = true
          break
        case ERROR:
        case FINISHED:
          signal.value = false
          break
      }
    }) as EventListener
  
    el.addEventListener(DATASTAR_SSE_EVENT, watcher)

    return () => {
      // Reset the signal
      signal.value = false
      
      el.removeEventListener(DATASTAR_SSE_EVENT, watcher)
    }
  },
}
