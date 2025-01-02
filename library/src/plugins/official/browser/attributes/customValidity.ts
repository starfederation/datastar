// Authors: Ben Croker
// Icon: mdi-message-alert
// Slug: Add custom validity to an element using an expression
// Description: This plugin allows you to add custom validity to an element using an expression. The expression should evaluate to a string that will be set as the custom validity message. This can be used to provide custom error messages for form validation.

import { dsErr } from '../../../../engine/errors'
import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'

export const CustomValidity: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'customValidity',
  keyReq: Requirement.Denied,
  valReq: Requirement.Must,
  onLoad: ({ el, genRX, effect }) => {
    if (!(el instanceof HTMLInputElement)) {
      throw dsErr('CustomValidityInvalidElement', { el })
    }
    const rx = genRX()
    return effect(() => {
      const result = rx<string>()
      if (typeof result !== 'string') {
        throw dsErr('CustomValidityInvalidExpression', { result })
      }
      el.setCustomValidity(result)
    })
  },
}
