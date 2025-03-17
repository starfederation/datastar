// Authors: Delaney Gillilan
// Icon: streamline:interface-edit-view-eye-eyeball-open-view
// Slug: Show or hide an element
// Description: This attribute shows or hides an element based on the value of the expression. If the expression is true, the element is shown. If the expression is false, the element is hidden. The element is hidden by setting the display property to none.

import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'

const NONE = 'none'
const DISPLAY = 'display'

export const Show: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'show',
  keyReq: Requirement.Denied,
  valReq: Requirement.Must,
  onLoad: ({ el: { style: s }, genRX, effect }) => {
    const { deps, rxFn } = genRX()
    return effect(deps, async () => {
      const shouldShow = rxFn<boolean>()
      if (shouldShow) {
        if (s.display === NONE) {
          s.removeProperty(DISPLAY)
        }
      } else {
        s.setProperty(DISPLAY, NONE)
      }
    })
  },
}
