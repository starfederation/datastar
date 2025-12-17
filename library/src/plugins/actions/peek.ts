// Icon: ion:eye
// Slug: Access signals without subscribing to changes.
// Description: Allows accessing signals without subscribing to their changes in expressions.

import { action } from '@engine'
import { startPeeking, stopPeeking } from '@engine/signals'

action({
  name: 'peek',
  apply(_, fn: () => any) {
    startPeeking()
    try {
      return fn()
    } finally {
      stopPeeking()
    }
  },
})
