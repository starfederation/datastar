import { DSP } from '../engine/consts'

// @ts-ignore
const _ = DSP // This is to force the import of DSP first in the compiled code

import { Computed } from '../plugins/core/attributes/computed'
import { Effect } from '../plugins/core/attributes/effect'
import { Signals } from '../plugins/core/attributes/signals'
import { apply, load, setAlias } from './engine'

load(Signals, Computed, Effect)

export { apply, load, setAlias }
