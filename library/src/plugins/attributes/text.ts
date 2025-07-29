// Icon: icon-park-outline:text
// Slug: Binds the text content of an element.
// Description: Binds the text content of an element to an expression.

import type { AttributePlugin } from '../../engine/types'

export const Text: AttributePlugin = {
  type: 'attribute',
  name: 'text',
  keyReq: 'denied',
  valReq: 'must',
  returnsValue: true,
  onLoad: ({ el, effect, rx }) => {
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
}
