// Icon: material-symbols:network-wifi
// Slug: Sets the indicator signal used when fetching data via SSE
// Description: must be a valid signal name

import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { trimDollarSignPrefix } from '../../../../utils/text'
import {
  DATASTAR_SSE_EVENT,
  type DatastarSSEEvent,
  FINISHED,
  STARTED,
} from '../shared'

export const Indicator: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'indicator',
  keyReq: Requirement.Exclusive,
  valReq: Requirement.Exclusive,
  onLoad: ({ value, signals, el, key }) => {
    const signalName = key ? key : trimDollarSignPrefix(value)
    const signal = signals.upsertIfMissing(signalName, false)
    const watcher = (event: CustomEvent<DatastarSSEEvent>) => {
      const {
        type,
        argsRaw: { elId },
      } = event.detail
      if (elId !== el.id) return
      switch (type) {
        case STARTED:
          signal.value = true
          break
        case FINISHED:
          signal.value = false
          break
      }
    }
    document.addEventListener(DATASTAR_SSE_EVENT, watcher)

    return () => {
      document.removeEventListener(DATASTAR_SSE_EVENT, watcher)
    }
  },
}
