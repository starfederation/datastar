// Icon: mdi:cursor-pointer
// Slug: Creates a reference to an element.
// Description: Creates a new signal that is a reference to the element on which the data attribute is placed.

import type { AttributePlugin } from '../../engine/types'
import { pathToObj } from '../../utils/paths'
import { modifyCasing } from '../../utils/text'

// Sets the value of the element
export const Ref: AttributePlugin = {
  type: 'attribute',
  name: 'ref',
  keyReq: 'exclusive',
  valReq: 'exclusive',
  shouldEvaluate: false,
  onLoad: ({ el, key, mods, value, mergePatch }) => {
    const signalName = key ? modifyCasing(key, mods) : value

    mergePatch(pathToObj({}, { [signalName]: el }))
  },
}
