// Authors: Delaney Gillilan
// Icon: mdi-light:vector-intersection
// Slug: Executes an expression on element intersection with viewport
// Description: An attribute that executes an expression on element intersection with the viewport.

import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'

const ONCE = 'once'
const HALF = 'half'
const FULL = 'full'

export const Intersect: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'intersect',
  keyReq: Requirement.Denied,
  mods: new Set([ONCE, HALF, FULL]),
  onLoad: ({ el, rawKey, mods, genRX }) => {
    const options = { threshold: 0 }
    if (mods.has(FULL)) options.threshold = 1
    else if (mods.has(HALF)) options.threshold = 0.5

    const rx = genRX()
    const observer = new IntersectionObserver((entries) => {
      for (const entry of entries) {
        if (entry.isIntersecting) {
          rx()
          if (mods.has(ONCE)) {
            observer.disconnect()
            delete el.dataset[rawKey]
          }
        }
      }
    }, options)

    observer.observe(el)
    return () => observer.disconnect()
  },
}
