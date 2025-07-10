// Icon: oui:security-signal
// Slug: Executes an expression when signals change.
// Description: Executes an expression on page load and whenever any signals in the expression change.

import type { AttributePlugin } from '../../engine/types'

export const Effect: AttributePlugin = {
  type: 'attribute',
  name: 'effect',
  keyReq: 'denied',
  valReq: 'must',
  onLoad: ({ effect, rx }) => effect(rx),
}
