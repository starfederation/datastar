// Icon: mdi:cursor-pointer
// Slug: Create a reference to an element
// Description: This attribute creates a reference to an element that can be used in other expressions.

import type { AttributePlugin } from '../../../engine/types'
import { pathToObj } from '../../../utils/paths'
import { modifyCasing, modifyScope } from '../../../utils/text'

// Sets the value of the element
export const Ref: AttributePlugin = {
  type: 'attribute',
  name: 'ref',
  keyReq: 'exclusive',
  valReq: 'exclusive',
  onLoad: ({ el, key, mods, value, mergePatch }) => {
    let signalName = key ? modifyCasing(key, mods) : value
    signalName = modifyScope(signalName, el, mods)

    mergePatch(pathToObj({}, { [signalName]: el }))
  },
}
