import { DSP } from '~/engine/consts'

// @ts-ignore
const _ = DSP // This is to force the import of DSP first in the compiled code

import { Computed } from '~/plugins/official/core/attributes/computed'
import { Signals } from '~/plugins/official/core/attributes/signals'
import { Star } from '~/plugins/official/core/attributes/star'
import { Engine } from './engine'

const DS = new Engine()
DS.load(Star, Signals, Computed)
export const Datastar = DS
