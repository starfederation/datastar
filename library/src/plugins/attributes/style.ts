// Icon: material-symbols:format-paint-outline
// Slug: Sets inline styles on an element based on an expression.
// Description: Sets CSS styles on an element using either key-based or object syntax, and keeps them in sync with reactive signals.

import { attribute } from '@engine'
import { effect } from '@engine/signals'
import { kebab } from '@utils/text'

attribute({
  name: 'style',
  requirement: {
    value: 'must',
  },
  returnsValue: true,
  apply({ key, el, rx }) {
    const { style } = el
    const initialStyles = new Map<string, string>()

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
        const styles = rx() as Record<string, any>

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
})
