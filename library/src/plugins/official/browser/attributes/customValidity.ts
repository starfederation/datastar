// Authors: Ben Croker
// Icon: mdi-message-alert
// Slug: Add custom validity to an element using an expression
// Description: This plugin allows you to add custom validity to an element using an expression. The expression should evaluate to a string that will be set as the custom validity message. This can be used to provide custom error messages for form validation.

import { runtimeErr } from '../../../../engine/errors'
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
  onLoad: (ctx) => {
    const { el, genRX, effect } = ctx
    if (
      !(
        el instanceof HTMLInputElement ||
        el instanceof HTMLSelectElement ||
        el instanceof HTMLTextAreaElement
      )
    ) {
      throw runtimeErr('CustomValidityInvalidElement', ctx)
    }
    const { deps, rxFn } = genRX()
    return effect(deps, () => {
      const result = rxFn<string>()
      if (typeof result !== 'string') {
        throw runtimeErr('CustomValidityInvalidExpression', ctx, { result })
      }
      el.setCustomValidity(result)
    })
  },
}
