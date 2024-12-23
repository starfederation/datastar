// Authors: Delaney Gillilan
// Icon: material-symbols:fit-screen-outline
// Slug: Clamp a value to a new range
// Description: This action clamps a value to a new range. The value is first scaled to the new range, then clamped to the new range. This is useful for scaling a value to a new range, then clamping it to that range.

import {
  type ActionPlugin,
  PluginType,
  type RuntimeContext,
} from '~/engine/types'

const { round, max, min } = Math
export const Fit: ActionPlugin = {
  type: PluginType.Action,
  name: 'fit',
  fn: (
    _: RuntimeContext,
    v: number,
    oldMin: number,
    oldMax: number,
    newMin: number,
    newMax: number,
    shouldClamp = false,
    shouldRound = false,
  ) => {
    let fitted = ((v - oldMin) / (oldMax - oldMin)) * (newMax - newMin) + newMin
    if (shouldRound) {
      fitted = round(fitted)
    }
    if (shouldClamp) {
      fitted = max(newMin, min(newMax, fitted))
    }
    return fitted
  },
}
