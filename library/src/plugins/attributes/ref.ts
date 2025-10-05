// Icon: mdi:cursor-pointer
// Slug: Creates a reference to an element.
// Description: Creates a new signal that is a reference to the element on which the data attribute is placed.

import type { AttributePlugin } from '../../engine/types'
import { modifyCasing } from '../../utils/text'

// Sets the value of the element
export const Ref: AttributePlugin = {
  type: 'attribute',
  name: 'ref',
  keyReq: 'exclusive',
  valReq: 'exclusive',
  shouldEvaluate: false,
  onLoad: ({ el, key, mods, value, mergePaths }) => {
    const signalName = key ? modifyCasing(key, mods) : value
    mergePaths([[signalName, el]])
  },
}
