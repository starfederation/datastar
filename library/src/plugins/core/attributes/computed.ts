import type { AttributePlugin } from '../../../engine/types'
import { pathToObj } from '../../../utils/paths'
import { modifyCasing, modifyScope } from '../../../utils/text'

export const Computed: AttributePlugin = {
  type: 'attribute',
  name: 'computed',
  keyReq: 'must',
  valReq: 'must',
  isExpr: true,
  onLoad: ({ el, key, mods, rx, computed, mergePatch }) => {
    let signalName = modifyCasing(key, mods)
    signalName = modifyScope(signalName, el, mods)
    mergePatch(pathToObj({}, { [signalName]: computed(rx) }))
  },
}
