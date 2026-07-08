// Icon: material-symbols:edit-attributes-outline
// Slug: Syncs the value of an attribute with an expression.
// Description: Sets the value of any HTML attribute to an expression, and keeps it in sync.

import { attribute } from '@engine'
import { effect } from '@engine/signals'

attribute({
  name: 'attr',
  requirement: { value: 'must' },
  returnsValue: true,
  apply({ el, key, rx }) {
    const syncAttr = (key: string, val: any) => {
      if (val === '' || val === true) {
        el.setAttribute(key, '')
      } else if (val === false || val == null) {
        el.removeAttribute(key)
      } else if (typeof val === 'string') {
        el.setAttribute(key, val)
      } else if (typeof val === 'function') {
        el.setAttribute(key, val.toString())
      } else {
        el.setAttribute(
          key,
          JSON.stringify(val, (_k, v) =>
            typeof v === 'function' ? v.toString() : v,
          ),
        )
      }
    }

    let attributeFilter: string[] = [];

    const update = key
      ? () => {
          observer.disconnect()
          const val = rx() as string
          syncAttr(key, val)
          observer.observe(el, {
            attributeFilter: [key],
          })
        }
      : () => {
          observer.disconnect()
          const obj = rx() as Record<string, any>
          for (const key of attributeFilter.filter(key => !obj.hasOwnProperty(key))) {
            el.removeAttribute(key)
          }
          attributeFilter = Object.keys(obj);
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
})
