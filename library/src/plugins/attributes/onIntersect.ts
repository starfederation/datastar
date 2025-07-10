// Icon: mdi-light:vector-intersection
// Slug: Runs an expression on intersection.
// Description: Runs an expression when the element intersects with the viewport.

import type { AttributePlugin, HTMLOrSVG } from '../../engine/types'
import { modifyTiming } from '../../utils/timing'
import { modifyViewTransition } from '../../utils/view-transitions'

const once = new WeakSet<HTMLOrSVG>()

export const OnIntersect: AttributePlugin = {
  type: 'attribute',
  name: 'onIntersect',
  keyReq: 'denied',
  onLoad: ({ el, mods, rx, startBatch, endBatch }) => {
    let callback = () => {
      startBatch()
      rx()
      endBatch()
    }
    callback = modifyTiming(callback, mods)
    callback = modifyViewTransition(callback, mods)
    const options = { threshold: 0 }
    if (mods.has('full')) {
      options.threshold = 1
    } else if (mods.has('half')) {
      options.threshold = 0.5
    }
    let observer: IntersectionObserver | null = new IntersectionObserver(
      (entries) => {
        for (const entry of entries) {
          if (entry.isIntersecting) {
            callback()
            if (observer && once.has(el)) {
              observer.disconnect()
            }
          }
        }
      },
      options,
    )
    observer.observe(el)
    if (mods.has('once')) {
      once.add(el)
    }
    return () => {
      if (!mods.has('once')) {
        once.delete(el)
      }
      if (observer) {
        observer.disconnect()
        observer = null
      }
    }
  },
}
