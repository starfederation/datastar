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
    // Track classes added by this attribute
    const addedClasses = new Set<string>()
    let isUpdating = false

    // Watch for external class changes (e.g., from morphing)
    const observer = new MutationObserver(() => {
      if (!isUpdating) {
        // External change detected, clear our tracking
        addedClasses.clear()
      }
    })

    const cleanup = effect(() => {
      isUpdating = true
      observer.disconnect()

      // Remove all previously added classes
      for (const className of addedClasses) {
        el.classList.remove(className)
      }
      addedClasses.clear()

      // Get classes - either from object expression or single key
      const classes = key === '' 
        ? rx<Record<string, boolean>>()
        : { [modifyCasing(kebab(key), mods)]: rx<boolean>() }

      // Add new classes and track them
      for (const [k, v] of Object.entries(classes)) {
        // We don't remove classes when v is false because
        // we're already removing ALL previously added classes above
        if (v) {
          const classNames = k.split(/\s+/).filter((cn) => cn.length > 0)
          for (const className of classNames) {
            el.classList.add(className)
            addedClasses.add(className)
          }
        }
      }

      isUpdating = false
      observer.observe(el, { attributeFilter: ['class'] })
    })

    return () => {
      observer.disconnect()
      // Clean up by removing all tracked classes
      for (const className of addedClasses) {
        el.classList.remove(className)
      }
      cleanup()
    }
  },
}
