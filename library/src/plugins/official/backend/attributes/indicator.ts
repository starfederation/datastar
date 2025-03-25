// Authors: Delaney Gillilan
// Icon: material-symbols:clock-loader-60-sharp
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
  FINISHED,
  STARTED,
} from '../shared'

export const Indicator: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'indicator',
  keyReq: Requirement.Exclusive,
  valReq: Requirement.Exclusive,
  onLoad: ({ el, key, mods, signals, value }) => {
    const signalName = key
      ? modifyCasing(key, mods)
      : trimDollarSignPrefix(value)
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
        case FINISHED:
          signal.value = false
          break
      }
    }) as EventListener

    el.addEventListener(DATASTAR_SSE_EVENT, watcher)

    return () => {
      // Delay removing the event listener to the next macrotask so that the watcher can set the signal appropriately (`queueMicrotask` is insufficient, ask me how I know!)
      setTimeout(() => el.removeEventListener(DATASTAR_SSE_EVENT, watcher))
    }
  },
}
