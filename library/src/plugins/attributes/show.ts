// Icon: streamline:interface-edit-view-eye-eyeball-open-view
// Slug: Shows or hides an element.
// Description: Shows or hides an element based on whether an expression evaluates to `true` or `false`.

import { attribute } from '@engine'
import { effect } from '@engine/signals'

const NONE = 'none'
const DISPLAY = 'display'

attribute({
  name: 'show',
  requirement: {
    key: 'denied',
    value: 'must',
  },
  returnsValue: true,
  apply({ el, rx }) {
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
})
