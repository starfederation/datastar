// Authors: Delaney Gillilan
// Icon: akar-icons:link-chain
// Slug: Bind attributes to expressions
// Description: Any attribute can be bound to an expression. The attribute will be updated reactively whenever the expression signal changes.

import {
  type AttributePlugin,
  type NestedValues,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { kebab } from '../../../../utils/text'

export const Attr: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'attr',
  valReq: Requirement.Must,
  onLoad: ({ el, key, effect, genRX }) => {
    const { deps, rxFn } = genRX()
    if (key === '') {
      return effect(deps, async () => {
        const binds = rxFn<NestedValues>()
        for (const [key, val] of Object.entries(binds)) {
          if (val === false) {
            el.removeAttribute(key)
          } else {
            el.setAttribute(key, val)
          }
        }
      })
    }

    // Attributes are always kebab-case
    key = kebab(key)

    return effect(deps, async () => {
      let value = false
      try {
        value = rxFn()
      } catch (e) {} //
      let v: string
      if (typeof value === 'string') {
        v = value
      } else {
        v = JSON.stringify(value)
      }
      if (!v || v === 'false' || v === 'null' || v === 'undefined') {
        el.removeAttribute(key)
      } else {
        el.setAttribute(key, v)
      }
    })
  },
}
