// Icon: ic:baseline-format-paint
// Slug: Add or remove classes from an element reactively
// Description: This action adds or removes classes from an element reactively based on the expression provided. The expression should be an object where the keys are the class names and the values are booleans. If the value is true, the class is added. If the value is false, the class is removed.

import type { AttributePlugin } from '../../../engine/types'
import { kebab, modifyCasing } from '../../../utils/text'

export const Class: AttributePlugin = {
  type: 'attribute',
  name: 'class',
  valReq: 'must',
  isExpr: true,
  onLoad: ({ key, el, effect, mods, rx }) => {
    if (key === '') {
      const observer = new MutationObserver(() => {
        observer.disconnect()
        const classes = rx<Record<string, boolean>>()
        for (const [k, v] of Object.entries(classes)) {
          const classNames = k.split(/\s+/)
          if (v) {
            el.classList.add(...classNames)
          } else {
            el.classList.remove(...classNames)
          }
        }
        observer.observe(el, {
          attributeFilter: ['class'],
        })
      })
      const cleanup = effect(() => {
        observer.disconnect()
        const classes = rx<Record<string, boolean>>()
        for (const [k, v] of Object.entries(classes)) {
          const classNames = k.split(/\s+/)
          if (v) {
            el.classList.add(...classNames)
          } else {
            el.classList.remove(...classNames)
          }
        }
        observer.observe(el, {
          attributeFilter: ['class'],
        })
      })

      return () => {
        observer.disconnect()
        cleanup()
      }
    }

    // Default to kebab-case and allow modifying
    let className = kebab(key)
    className = modifyCasing(className, mods)
    const observer = new MutationObserver(() => {
      observer.disconnect()
      const shouldInclude = rx<boolean>()
      if (shouldInclude) {
        el.classList.add(className)
      } else {
        el.classList.remove(className)
      }
      observer.observe(el, {
        attributeFilter: ['class'],
      })
    })
    const cleanup = effect(() => {
      observer.disconnect()
      const shouldInclude = rx<boolean>()
      if (shouldInclude) {
        el.classList.add(className)
      } else {
        el.classList.remove(className)
      }
      observer.observe(el, {
        attributeFilter: ['class'],
      })
    })

    return () => {
      observer.disconnect()
      cleanup()
    }
  },
}
