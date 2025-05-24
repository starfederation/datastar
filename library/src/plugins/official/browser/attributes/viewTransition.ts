// Authors: Delaney Gillilan
// Icon: material-symbols:masked-transitions
// Slug: Setup view transitions
// Description: This attribute plugin sets up view transitions for the current view. This plugin requires the view transition API to be enabled in the browser. If the browser does not support view transitions, an error will be logged to the console.

import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { supportsViewTransitions } from '../../../../utils/view-transtions'

const VIEW_TRANSITION = 'view-transition'

export const ViewTransition: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'viewTransition',
  keyReq: Requirement.Denied,
  valReq: Requirement.Must,
  onLoad: ({ effect, el, genRX }) => {
    if (!supportsViewTransitions) {
      console.error('Browser does not support view transitions')
      return
    }
    const rx = genRX()
    return effect(() => {
      const name = rx<string>()
      if (!name?.length) return
      const elVTASTyle = el.style as unknown as CSSStyleDeclaration
      elVTASTyle.viewTransitionName = name
    })
  },
}
