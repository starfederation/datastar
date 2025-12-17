// Icon: mdi:cursor-pointer
// Slug: Creates a reference to an element.
// Description: Creates a new signal that is a reference to the element on which the data attribute is placed.

import { attribute } from '@engine'
import { mergePaths } from '@engine/signals'
import { modifyCasing } from '@utils/text'

attribute({
  name: 'ref',
  requirement: 'exclusive',
  apply({ el, key, mods, value }) {
    const signalName = key != null ? modifyCasing(key, mods) : value
    mergePaths([[signalName, el]])
  },
})
