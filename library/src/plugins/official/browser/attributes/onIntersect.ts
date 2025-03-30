// Authors: Delaney Gillilan
// Icon: mdi-light:vector-intersection
// Slug: Executes an expression when an element intersects with the viewport
// Description: An attribute that executes an expression when an element intersects with the viewport.

import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { modifyTiming } from '../../../../utils/timing'
import { modifyViewTransition } from '../../../../utils/view-transtions'

export const OnIntersect: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'onIntersect',
  keyReq: Requirement.Denied,
  onLoad: ({ el, rawKey, mods, genRX }) => {
    let callback = modifyTiming(genRX(), mods)
    callback = modifyViewTransition(callback, mods)

    const options = { threshold: 0 }
    if (mods.has('full')) {
      options.threshold = 1
    } else if (mods.has('half')) {
      options.threshold = 0.5
    }

    const observer = new IntersectionObserver((entries) => {
      for (const entry of entries) {
        if (entry.isIntersecting) {
          callback()

          if (mods.has('once')) {
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
