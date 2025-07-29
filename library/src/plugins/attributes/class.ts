// Icon: ic:baseline-format-paint
// Slug: Adds or removes a class based on an expression.
// Description: Adds or removes a class to or from an element based on an expression.

import type { AttributePlugin } from '../../engine/types'
import { kebab, modifyCasing } from '../../utils/text'

export const Class: AttributePlugin = {
  type: 'attribute',
  name: 'class',
  valReq: 'must',
  returnsValue: true,
  onLoad: ({ key, el, effect, mods, rx }) => {
    if (key) {
      key = modifyCasing(kebab(key), mods)
    }

    const callback = () => {
      observer.disconnect()

      const classes = key
        ? { [key]: rx<boolean>() }
        : rx<Record<string, boolean>>()

      for (const k in classes) {
        const classNames = k.split(/\s+/).filter((cn) => cn.length > 0)
        if (classes[k]) {
          for (const name of classNames) {
            if (!el.classList.contains(name)) {
              el.classList.add(name)
            }
          }
        } else {
          for (const name of classNames) {
            if (el.classList.contains(name)) {
              el.classList.remove(name)
            }
          }
        }
      }

      observer.observe(el, { attributeFilter: ['class'] })
    }

    const observer = new MutationObserver(callback)
    const cleanup = effect(callback)

    return () => {
      observer.disconnect()
      cleanup()

      const classes = key
        ? { [key]: rx<boolean>() }
        : rx<Record<string, boolean>>()

      for (const k in classes) {
        const classNames = k.split(/\s+/).filter((cn) => cn.length > 0)
        for (const name of classNames) {
          el.classList.remove(name)
        }
      }
    }
  },
}
