// Icon: material-symbols:change-circle-outline
// Slug: Runs an expression when signals are patched.
// Description: Runs an expression whenever one or more signals are patched.

import { attribute } from '@engine'
import { DATASTAR_SIGNAL_PATCH_EVENT } from '@engine/consts'
import { beginBatch, endBatch, filtered } from '@engine/signals'
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
        const watched = filtered(filters, evt.detail)
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

    document.addEventListener(DATASTAR_SIGNAL_PATCH_EVENT, callback)
    return () => {
      document.removeEventListener(DATASTAR_SIGNAL_PATCH_EVENT, callback)
    }
  },
})
