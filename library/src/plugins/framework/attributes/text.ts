// Icon: tabler:typography
// Slug: Set the text content of an element
// Description: This attribute sets the text content of an element to the result of the expression.

import type { AttributePlugin } from '../../../engine/types'

export const Text: AttributePlugin = {
  type: 'attribute',
  name: 'text',
  keyReq: 'denied',
  valReq: 'must',
  isExpr: true,
  onLoad: ({ el, effect, rx }) => {
    const update = () => {
      observer.disconnect()
      el.textContent = `${rx()}`
      observer.observe(el, { childList: true })
    }
    const observer = new MutationObserver(update)
    const cleanup = effect(update)

    return () => {
      observer.disconnect()
      cleanup()
    }
  },
}
