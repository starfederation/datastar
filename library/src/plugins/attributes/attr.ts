// Icon: material-symbols:edit-attributes-outline
// Slug: Syncs the value of an attribute with an expression.
// Description: Sets the value of any HTML attribute to an expression, and keeps it in sync.

import type { AttributePlugin } from '../../engine/types'
import { kebab } from '../../utils/text'

export const Attr: AttributePlugin = {
  type: 'attribute',
  name: 'attr',
  valReq: 'must',
  returnsValue: true,
  onLoad: ({ el, effect, key, rx }) => {
    const syncAttr = (key: string, val: any) => {
      if (val === '' || val === true) {
        el.setAttribute(key, '')
      } else if (val === false || val == null) {
        el.removeAttribute(key)
      } else if (typeof val === 'string') {
        el.setAttribute(key, val)
      } else {
        el.setAttribute(key, JSON.stringify(val))
      }
    }

    key = kebab(key)
    const update = key
      ? () => {
          observer.disconnect()
          const val = rx<string>()
          syncAttr(key, val)
          observer.observe(el, {
            attributeFilter: [key],
          })
        }
      : () => {
          observer.disconnect()
          const obj = rx<Record<string, any>>()
          const attributeFilter = Object.keys(obj)
          for (const key of attributeFilter) {
            syncAttr(key, obj[key])
          }
          observer.observe(el, {
            attributeFilter,
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
