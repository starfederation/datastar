import { DSP } from './consts'

// @ts-ignore
const _ = DSP // This is to force the import of DSP first in the compiled code

import { Computed } from '~/plugins/official/core/attributes/computed'
import { Signals } from '~/plugins/official/core/attributes/signals'
import { Star } from '~/plugins/official/core/attributes/star'
import {
  SignalValueMacro,
  TagTemplateEscaperMacro,
  UnignoreAnythingMacro,
} from '~/plugins/official/core/macros/signals'
import { Engine } from './engine'

const DS = new Engine()
DS.load(
  Star,
  TagTemplateEscaperMacro,
  SignalValueMacro,
  UnignoreAnythingMacro,
  Signals,
  Computed,
)
export const Datastar = DS
