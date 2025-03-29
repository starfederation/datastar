// Authors: Ben Croker
// Icon: material-symbols:bigtop-updates
// Slug: Runs an expression whenever a signal changes
// Description: This attribute runs an expression whenever a signal changes. 

import {
  type AttributePlugin,
  DATASTAR_SIGNAL_EVENT,
  type DatastarSignalEvent,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { pathMatchesPattern } from '../../../../utils/paths'
import { modifyCasing } from '../../../../utils/text'
import { modifyTiming } from '../../../../utils/timing'
import { modifyViewTransition } from '../../../../utils/view-transtions'
import { effect, type Signal } from '../../../../vendored/preact-core'

export const OnSignalChange: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'onSignalChange',
  valReq: Requirement.Must,
  onLoad: ({ key, mods, signals, genRX }) => {
    let callback = modifyTiming(genRX(), mods)
    callback = modifyViewTransition(callback, mods)

    if (key === '') {
      const signalFn = (event: CustomEvent<DatastarSignalEvent>) =>
        callback(event)
      document.addEventListener(DATASTAR_SIGNAL_EVENT, signalFn)

      return () => {
        document.removeEventListener(DATASTAR_SIGNAL_EVENT, signalFn)
      }
    }

    const pattern = modifyCasing(key, mods)
    const signalValues = new Map<Signal, any>()
    signals.walk((path, signal) => {
      if (pathMatchesPattern(path, pattern)) {
        signalValues.set(signal, signal.value)
      }
    })

    return effect(() => {
      for (const [signal, prev] of signalValues) {
        if (prev !== signal.value) {
          callback()
          signalValues.set(signal, signal.value)
        }
      }
    })
  },
}
