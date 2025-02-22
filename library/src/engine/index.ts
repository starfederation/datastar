import { DSP } from '../engine/consts'
// @ts-ignore
const _ = DSP // This is to force the import of DSP first in the compiled code

import { Computed } from '../plugins/official/core/attributes/computed'
import { Signals } from '../plugins/official/core/attributes/signals'
import { Star } from '../plugins/official/core/attributes/star'
import { load } from './engine'
export { load, setPrefix as setAliasPrefix } from './engine'

load(Star, Signals, Computed)
