// Icon: material-symbols:format-paint-outline
// Slug: Sets inline styles on an element based on an expression.
// Description: Sets CSS styles on an element using either key-based or object syntax, and keeps them in sync with reactive signals.

import type { AttributePlugin } from '../../engine/types'
import { kebab } from '../../utils/text'

export const Style: AttributePlugin = {
  type: 'attribute',
  name: 'style',
  valReq: 'must',
  returnsValue: true,
  onLoad: ({ key, el, effect, rx }) => {
    const { style } = el
    const initialStyles = new Map<string, string>()

    key &&= kebab(key)

    const apply = (prop: string, value: any) => {
      const initial = initialStyles.get(prop)
      if (!value && value !== 0) {
        initial !== undefined &&
          (initial
            ? style.setProperty(prop, initial)
            : style.removeProperty(prop))
      } else {
        initial === undefined &&
          initialStyles.set(prop, style.getPropertyValue(prop))
        style.setProperty(prop, String(value))
      }
    }

    const update = () => {
      observer.disconnect()

      if (key) {
        apply(key, rx())
      } else {
        const styles = rx<Record<string, any>>()

        for (const [prop, initial] of initialStyles) {
          prop in styles ||
            (initial
              ? style.setProperty(prop, initial)
              : style.removeProperty(prop))
        }

        for (const prop in styles) {
          apply(kebab(prop), styles[prop])
        }
      }

      observer.observe(el, { attributeFilter: ['style'] })
    }

    const observer = new MutationObserver(update)
    const cleanup = effect(update)

    return () => {
      observer.disconnect()
      cleanup()
      for (const [prop, initial] of initialStyles) {
        initial ? style.setProperty(prop, initial) : style.removeProperty(prop)
      }
    }
  },
}
