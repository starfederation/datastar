// Icon: icon-park-outline:text
// Slug: Binds the text content of an element.
// Description: Binds the text content of an element to an expression.

import { attribute } from '@engine'
import { effect } from '@engine/signals'

attribute({
  name: 'text',
  requirement: {
    key: 'denied',
    value: 'must',
  },
  returnsValue: true,
  apply({ el, rx }) {
    const update = () => {
      observer.disconnect()
      el.textContent = `${rx()}`
      observer.observe(el, {
        childList: true,
        characterData: true,
        subtree: true,
      })
    }

    const observer = new MutationObserver(update)
    const cleanup = effect(update)

    return () => {
      observer.disconnect()
      cleanup()
    }
  },
})
