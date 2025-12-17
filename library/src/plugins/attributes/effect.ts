// Icon: oui:security-signal
// Slug: Executes an expression when signals change.
// Description: Executes an expression on page load and whenever any signals in the expression change.

import { attribute } from '@engine'
import { effect } from '@engine/signals'

attribute({
  name: 'effect',
  requirement: {
    key: 'denied',
    value: 'must',
  },
  apply: ({ rx }) => effect(rx),
})
