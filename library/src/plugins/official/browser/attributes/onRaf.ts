// Authors: Ben Croker
// Icon: material-symbols:animated-images-outline
// Slug: Runs an expression on every animation frame
// Description: This attribute runs an expression on every animation frame.

import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { modifyTiming } from '../../../../utils/timing'
import { modifyViewTransition } from '../../../../utils/view-transtions'

export const OnRaf: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'onRaf',
  keyReq: Requirement.Denied,
  valReq: Requirement.Must,
  onLoad: ({ mods, genRX }) => {
    let callback = modifyTiming(genRX(), mods)
    callback = modifyViewTransition(callback, mods)

    let rafId: number | undefined
    const raf = () => {
      callback()
      rafId = requestAnimationFrame(raf)
    }
    rafId = requestAnimationFrame(raf)

    return () => {
      if (rafId) {
        cancelAnimationFrame(rafId)
      }
    }
  },
}
