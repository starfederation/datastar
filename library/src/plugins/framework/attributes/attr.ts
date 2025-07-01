// Icon: material-symbols:edit-attributes-outline
// Slug: Bind attributes to expressions
// Description: Any attribute can be bound to an expression. The attribute will be updated reactively whenever the expression signal changes.

import type { AttributePlugin } from '../../../engine/types'
import { kebab } from '../../../utils/text'

export const Attr: AttributePlugin = {
  type: 'attribute',
  name: 'attr',
  valReq: 'must',
  isExpr: true,
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
