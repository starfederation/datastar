// Icon: streamline:interface-edit-view-eye-eyeball-open-view
// Slug: Shows or hides an element.
// Description: Shows or hides an element based on whether an expression evaluates to `true` or `false`.

import type { AttributePlugin } from '../../engine/types'

const NONE = 'none'
const DISPLAY = 'display'

export const Show: AttributePlugin = {
  type: 'attribute',
  name: 'show',
  keyReq: 'denied',
  valReq: 'must',
  returnsValue: true,
  onLoad: ({ el, effect, rx }) => {
    const update = () => {
      observer.disconnect()
      const shouldShow = rx()
      if (shouldShow) {
        if (el.style.display === NONE) el.style.removeProperty(DISPLAY)
      } else {
        el.style.setProperty(DISPLAY, NONE)
      }
      observer.observe(el, { attributeFilter: ['style'] })
    }
    const observer = new MutationObserver(update)
    const cleanup = effect(update)

    return () => {
      observer.disconnect()
      cleanup()
    }
  },
}
