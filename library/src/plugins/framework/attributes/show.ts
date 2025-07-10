// Icon: streamline:interface-edit-view-eye-eyeball-open-view
// Slug: Show or hide an element
// Description: This attribute shows or hides an element based on the value of the expression. If the expression is true, the element is shown. If the expression is false, the element is hidden. The element is hidden by setting the display property to none.

import type { AttributePlugin } from '../../../engine/types'

const NONE = 'none'
const DISPLAY = 'display'

export const Show: AttributePlugin = {
  type: 'attribute',
  name: 'show',
  keyReq: 'denied',
  valReq: 'must',
  isExpr: true,
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
