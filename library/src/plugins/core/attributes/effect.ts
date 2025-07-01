import type { AttributePlugin } from '../../../engine/types'

export const Effect: AttributePlugin = {
  type: 'attribute',
  name: 'effect',
  keyReq: 'denied',
  valReq: 'must',
  onLoad: ({ effect, rx }) => effect(rx),
}
