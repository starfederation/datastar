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
      } else if (val === false || val === null || val === undefined) {
        el.removeAttribute(key)
      } else {
        el.setAttribute(key, val)
      }
    }
    if (key === '') {
      const observer = new MutationObserver(() => {
        observer.disconnect()
        const obj = rx() as Record<string, any>
        for (const [key, val] of Object.entries(obj)) {
          syncAttr(key, val)
        }
        observer.observe(el, {
          attributeFilter: Object.keys(obj),
        })
      })
      const cleanup = effect(() => {
        observer.disconnect()
        const obj = rx() as Record<string, any>
        for (const key in obj) {
          syncAttr(key, obj[key])
        }
        observer.observe(el, {
          attributeFilter: Object.keys(obj),
        })
      })

      return () => {
        observer.disconnect()
        cleanup()
      }
    }
    // Attributes are always kebab-case
    const k = kebab(key)
    const observer = new MutationObserver(() => {
      observer.disconnect()
      const value = rx<string>()
      syncAttr(k, value)
      observer.observe(el, {
        attributeFilter: [value],
      })
    })
    const cleanup = effect(() => {
      observer.disconnect()
      const value = rx<string>()
      syncAttr(k, value)
      observer.observe(el, {
        attributeFilter: [value],
      })
    })

    return () => {
      observer.disconnect()
      cleanup()
    }
  },
}
