// Icon: material-symbols:change-circle-outline
// Slug: Runs an expression when signals are patched.
// Description: Runs an expression whenever one or more signals are patched.

import { attribute } from '@engine'
import { DATASTAR_SIGNAL_PATCH_EVENT, DOCUMENT } from '@engine/consts'
import {
  beginBatch,
  endBatch,
  filtered,
  startPeeking,
  stopPeeking,
} from '@engine/signals'
import type { JSONPatch, SignalFilterOptions } from '@engine/types'
import { isEmpty } from '@utils/paths'
import { aliasify, jsStrToObject } from '@utils/text'
import { modifyTiming } from '@utils/timing'

attribute({
  name: 'on-signal-patch',
  requirement: {
    value: 'must',
  },
  argNames: ['patch'],
  returnsValue: true,
  apply({ el, key, mods, rx, error }) {
    if (!!key && key !== 'filter') {
      throw error('KeyNotAllowed')
    }

    const filterAttr = aliasify(`${this.name}-filter`)
    const filtersRaw = el.getAttribute(filterAttr)
    let filters: SignalFilterOptions = {}
    if (filtersRaw) {
      filters = jsStrToObject(filtersRaw)
    }

    let running = false

    const callback: EventListener = modifyTiming(
      (evt: CustomEvent<JSONPatch>) => {
        if (running) return
        // Peek when getting the filtered signals because we don’t want to subscribe to them, we just want to know which ones are being patched.
        startPeeking()
        const watched = filtered(filters, evt.detail)
        stopPeeking()
        if (!isEmpty(watched)) {
          running = true
          beginBatch()
          try {
            rx(watched)
          } finally {
            endBatch()
            running = false
          }
        }
      },
      mods,
    )

    DOCUMENT.addEventListener(DATASTAR_SIGNAL_PATCH_EVENT, callback)
    return () => {
      DOCUMENT.removeEventListener(DATASTAR_SIGNAL_PATCH_EVENT, callback)
    }
  },
})
