// Authors: Ben Croker
// Icon: material-symbols:mail
// Slug: Executes and expression on every animation frame
// Description: This plugin executes an expression on every animation frame.

import {
  type AttributePlugin,
  PluginType,
  Requirement,
} from '../../../../engine/types'
import { modifyTiming } from '../../../../utils/timing'
import { modifyViewTransition } from '../../../../utils/view-transtions'

export const Raf: AttributePlugin = {
  type: PluginType.Attribute,
  name: 'raf',
  keyReq: Requirement.Denied,
  valReq: Requirement.Must,
  onLoad: ({ mods, genRX }) => {
    const rx = genRX()
    let callback = () => rx()
    callback = modifyTiming(callback, mods)
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
