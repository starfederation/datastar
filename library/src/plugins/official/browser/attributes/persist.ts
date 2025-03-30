// Authors: Delaney Gillilan
// Icon: mdi:floppy-variant
// Slug: Persist data to local storage or session storage
// Description: This plugin allows you to persist data to local storage or session storage.  Once you add this attribute the data will be persisted to local storage or session storage.

import { DATASTAR } from '../../../../engine/consts'
import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { getMatchingSignalPaths } from '../../../../utils/paths'

export const Persist: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'persist',
  keyReq: Requirement.Denied,
  onLoad: ({ effect, mods, signals, value }) => {
    const key = DATASTAR
    const storage = mods.has('session') ? sessionStorage : localStorage
    
    // If the value is empty, persist all signals
    const paths = value !== '' ? value : '**'

    const storageToSignals = () => {
      const data = storage.getItem(key) || '{}'
      const nestedValues = JSON.parse(data)
      signals.merge(nestedValues)
    }

    const signalsToStorage = () => {
      const signalPaths = getMatchingSignalPaths(signals, paths)
      const nv = signals.subset(...signalPaths)
      storage.setItem(key, JSON.stringify(nv))
    }

    storageToSignals()
    return effect(() => {
      signalsToStorage()
    })
  },
}
