// Icon: si:json-fill
// Slug: Outputs a JSON stringified version of signals.
// Description: Sets the text content of an element to a reactive JSON stringified version of signals.

import type { AttributePlugin, SignalFilterOptions } from '../../engine/types'
import { jsStrToObject } from '../../utils/text'

export const JsonSignals: AttributePlugin = {
  type: 'attribute',
  name: 'jsonSignals',
  keyReq: 'denied',
  onLoad: ({ el, effect, value, filtered, mods }) => {
    const spaces = mods.has('terse') ? 0 : 2
    let filters: SignalFilterOptions = {}
    if (value) {
      filters = jsStrToObject(value)
    }

    const callback = () => {
      observer.disconnect()
      el.textContent = JSON.stringify(filtered(filters), null, spaces)
      observer.observe(el, {
        childList: true,
        characterData: true,
        subtree: true,
      })
    }
    const observer = new MutationObserver(callback)
    const cleanup = effect(callback)

    return () => {
      observer.disconnect()
      cleanup()
    }
  },
}
