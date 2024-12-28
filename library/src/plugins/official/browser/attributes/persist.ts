// Authors: Delaney Gillilan
// Icon: mdi:floppy-variant
// Slug: Persist data to local storage or session storage
// Description: This plugin allows you to persist data to local storage or session storage.  Once you add this attribute the data will be persisted to local storage or session storage.

import { DATASTAR } from '~/engine/consts'
import {
  type AttributePlugin,
  type NestedValues,
  PluginType,
} from '~/engine/types'
import { IngnoreAnythingMacro } from '../../core/macros/signals'

const SESSION = 'session'

export const Persist: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'persist',
  mods: new Set([SESSION]),
  macros: { pre: [IngnoreAnythingMacro] },
  onLoad: ({ key, value, signals, effect, mods }) => {
    if (key === '') {
      key = DATASTAR
    }
    const storage = mods.has(SESSION) ? sessionStorage : localStorage
    const paths = value.split(/\s+/).filter((p) => p !== '')

    const storageToSignals = () => {
      const data = storage.getItem(key) || '{}'
      const nestedValues = JSON.parse(data)
      signals.merge(nestedValues)
    }

    const signalsToStorage = () => {
      let nv: NestedValues
      if (!paths.length) {
        nv = signals.values()
      } else {
        nv = signals.subset(...paths)
      }
      storage.setItem(key, JSON.stringify(nv))
    }

    storageToSignals()
    return effect(() => {
      signalsToStorage()
    })
  },
}
