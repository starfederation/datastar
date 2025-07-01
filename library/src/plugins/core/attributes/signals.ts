import type { AttributePlugin } from '../../../engine/types'
import { pathToObj } from '../../../utils/paths'
import { modifyCasing, modifyScope } from '../../../utils/text'

export const Signals: AttributePlugin = {
  type: 'attribute',
  name: 'signals',
  isExpr: true,
  onLoad: ({ el, key, mods, rx, mergePatch }) => {
    const ifMissing = mods.has('ifmissing')

    if (key) {
      key = modifyScope(modifyCasing(key, mods), el, mods)
      mergePatch(pathToObj({}, { [key]: rx() }), { ifMissing })
    } else {
      const patch = rx<Record<string, any>>()
      const pathObj: Record<string, any> = {}
      for (const key in patch) {
        pathObj[modifyScope(key, el, mods)] = patch[key]
      }
      mergePatch(pathToObj({}, pathObj), { ifMissing })
    }
  },
}
