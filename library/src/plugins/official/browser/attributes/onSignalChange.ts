// Authors: Ben Croker
// Icon: material-symbols:mail
// Slug: Executes and expression on any signal change
// Description: This attribute executes an expression on any signal change. 

import {
  type AttributePlugin,
  DATASTAR_SIGNAL_EVENT,
  type DatastarSignalEvent,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { modifyCasing } from '../../../../utils/text'
import { effect, type Signal } from '../../../../vendored/preact-core'

export const OnSignalChange: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'onSignalChange',
  valReq: Requirement.Must,
  onLoad: ({ key, mods, signals, genRX }) => {
    const callback = genRX()

    if (key === '') {
      const signalFn = (event: CustomEvent<DatastarSignalEvent>) =>
        callback(event)
      document.addEventListener(DATASTAR_SIGNAL_EVENT, signalFn)

      return () => {
        document.removeEventListener(DATASTAR_SIGNAL_EVENT, signalFn)
      }
    }

    const signalPath = modifyCasing(key, mods)
    const signalValues = new Map<Signal, any>()
    signals.walk((path, signal) => {
      if (path.startsWith(signalPath)) {
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
