// Authors: Delaney Gillilan
// Icon: mdi:floppy-variant
// Slug: Persist data to local storage or session storage
// Description: This plugin allows you to persist data to local storage or session storage.  Once you add this attribute the data will be persisted to local storage or session storage.

import { DATASTAR } from '../../../../engine/consts'
import {
  type AttributePlugin,
  type NestedValues,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { trimDollarSignPrefix } from '../../../../utils/text'

const SESSION = 'session'

export const Persist: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'persist',
  keyReq: Requirement.Denied,
  mods: new Set([SESSION]),
  onLoad: ({ effect, mods, signals, value }) => {
    const key = DATASTAR
    const storage = mods.has(SESSION) ? sessionStorage : localStorage
    let paths = value.split(/\s+/).filter((p) => p !== '')
    paths = paths.map((p) => trimDollarSignPrefix(p))

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
