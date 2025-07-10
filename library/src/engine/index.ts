import { DSP } from '../engine/consts'

// @ts-ignore
const _ = DSP // This is to force the import of DSP first in the compiled code

import { apply, load, setAlias } from './engine'

export { apply, load, setAlias }
