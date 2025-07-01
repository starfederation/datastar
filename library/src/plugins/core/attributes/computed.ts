import type { AttributePlugin } from '../../../engine/types'
import { pathToObj } from '../../../utils/paths'
import { modifyCasing } from '../../../utils/text'

export const Computed: AttributePlugin = {
  type: 'attribute',
  name: 'computed',
  keyReq: 'must',
  valReq: 'must',
  isExpr: true,
  onLoad: ({ key, mods, rx, computed, mergePatch }) => {
    key = modifyCasing(key, mods)
    mergePatch(pathToObj({}, { [key]: computed(rx) }))
  },
}
