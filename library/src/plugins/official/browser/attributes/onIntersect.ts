// Authors: Delaney Gillilan
// Icon: mdi-light:vector-intersection
// Slug: Executes an expression when an element intersects with the viewport
// Description: An attribute that executes an expression when an element intersects with the viewport.

import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'

const ONCE = 'once'
const HALF = 'half'
const FULL = 'full'

export const OnIntersect: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'onIntersect',
  keyReq: Requirement.Denied,
  mods: new Set([ONCE, HALF, FULL]),
  onLoad: ({ el, rawKey, mods, genRX }) => {
    const callback = genRX()
    const options = { threshold: 0 }
    if (mods.has(FULL)) {
      options.threshold = 1
    } else if (mods.has(HALF)) {
      options.threshold = 0.5
    }

    const observer = new IntersectionObserver((entries) => {
      for (const entry of entries) {
        if (entry.isIntersecting) {
          callback()

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
