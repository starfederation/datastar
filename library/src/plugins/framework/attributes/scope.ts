// Icon: tabler:box
// Slug: Create a scope for scoped signals
// Description: This attribute creates a scope for scoping data to specific DOM elements.

import type { AttributePlugin } from '../../../engine/types'
import { modifyScope } from '../../../utils/text'

export const Scope: AttributePlugin = {
  type: 'attribute',
  name: 'scope',
  keyReq: 'denied',
  onLoad: ({ el, value, mods, mergePatch }) => {
    let scopeKey = value || `_${crypto.randomUUID()}`

    if (el.parentElement) {
      scopeKey = modifyScope(scopeKey, el.parentElement, mods)
    }

    const scopeKeyObj = scopeKey
      .split('.')
      .reduceRight<any>((acc, part) => ({ [part]: acc }), {})
    mergePatch(scopeKeyObj)

    el.setAttribute('data-scope', scopeKey)
    el.removeAttribute('data-scope__scoped')

    return () => {
      const parts = scopeKey.split('.')
      const removalObj = parts.reduceRight<any>(
        (acc, part) => ({ [part]: acc }),
        null,
      )
      mergePatch(removalObj)
    }
  },
}
