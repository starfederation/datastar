// Authors: Delaney Gillilan
// Icon: akar-icons:link-chain
// Slug: Bind attributes to expressions
// Description: Any attribute can be bound to an expression. The attribute will be updated reactively whenever the expression signal changes.

import {
  type AttributePlugin,
  type NestedValues,
  PluginType,
  Requirement,
} from '~/engine/types'
import { kebabize } from '~/utils/text'

export const Attributes: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'attributes',
  valReq: Requirement.Must,
  onLoad: ({ el, genRX, key, effect }) => {
    const rx = genRX()
    if (key === '') {
      return effect(async () => {
        const binds = rx<NestedValues>()
        for (const [attr, val] of Object.entries(binds)) {
          el.setAttribute(attr, val)
        }
      })
    }

    key = kebabize(key)
    return effect(async () => {
      let value = false
      try {
        value = rx()
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
