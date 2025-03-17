// Authors: Delaney Gillilan
// Icon: mdi:floppy-variant
// Slug: Persist data to local storage or session storage
// Description: This plugin allows you to persist data to local storage or session storage.  Once you add this attribute the data will be persisted to local storage or session storage.

import { DATASTAR } from '../../../../engine/consts'
import {
  type AttributePlugin,
  DATASTAR_SIGNAL_EVENT,
  type DatastarSignalEvent,
  type NestedValues,
  PluginType,
} from '../../../../engine/types'
import {
  camel,
  modifyCasing,
  trimDollarSignPrefix,
} from '../../../../utils/text'
import { SIGNALS_CHANGE_PREFIX } from '../../dom/attributes/on'

const SESSION = 'session'

export const Persist: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'persist',
  mods: new Set([SESSION]),
  onLoad: ({ key, mods, signals, value }) => {
    key = modifyCasing(key, mods)
    if (key === '') {
      key = DATASTAR
    }

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

    const hasPrefix = key !== SIGNALS_CHANGE_PREFIX
    const signalPath = modifyCasing(
      camel(key.slice(SIGNALS_CHANGE_PREFIX.length)),
      mods,
    )
    const signalFn = (event: CustomEvent<DatastarSignalEvent>) => {
      if (hasPrefix) {
        const { added, removed, updated } = event.detail
        if (
          ![...added, ...removed, ...updated].some((d) =>
            d.startsWith(signalPath),
          )
        ) {
          return
        }
      }
      signalsToStorage()
    }
    document.addEventListener(DATASTAR_SIGNAL_EVENT, signalFn)
    storageToSignals()
    return () => {
      document.removeEventListener(DATASTAR_SIGNAL_EVENT, signalFn)
    }
  },
}
