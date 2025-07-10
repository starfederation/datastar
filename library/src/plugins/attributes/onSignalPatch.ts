// Icon: material-symbols:change-circle-outline
// Slug: Runs an expression when signals are patched.
// Description: Runs an expression whenever one or more signals are patched.

import {
  type AttributePlugin,
  DATASTAR_SIGNAL_PATCH_EVENT,
  type JSONPatch,
  type SignalFilterOptions,
} from '../../engine/types'
import { isEmpty } from '../../utils/paths'
import { jsStrToObject } from '../../utils/text'
import { modifyTiming } from '../../utils/timing'

export const OnSignalPatch: AttributePlugin = {
  type: 'attribute',
  name: 'onSignalPatch',
  valReq: 'must',
  argNames: ['patch'],
  returnsValue: true,
  onLoad: ({
    el,
    key,
    mods,
    plugin,
    rx,
    filtered,
    runtimeErr,
    startBatch,
    endBatch,
  }) => {
    // Throw an error if the key exists and is not `filter`
    if (!!key && key !== 'filter') {
      throw runtimeErr(`${plugin.name}KeyNotAllowed`)
    }

    // Look for data-on-signal-patch-filter data attribute
    const filtersRaw = el.getAttribute('data-on-signal-patch-filter')
    let filters: SignalFilterOptions = {}
    if (filtersRaw) {
      filters = jsStrToObject(filtersRaw)
    }

    const callback: EventListener = modifyTiming(
      (evt: CustomEvent<JSONPatch>) => {
        const watched = filtered(filters, evt.detail)
        if (!isEmpty(watched)) {
          startBatch()
          rx(watched)
          endBatch()
        }
      },
      mods,
    )

    document.addEventListener(DATASTAR_SIGNAL_PATCH_EVENT, callback)
    return () => {
      document.removeEventListener(DATASTAR_SIGNAL_PATCH_EVENT, callback)
    }
  },
}
