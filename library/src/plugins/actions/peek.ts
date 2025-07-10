// Icon: ion:eye
// Slug: Access signals without subscribing to changes.
// Description: Allows accessing signals without subscribing to their changes in expressions.

import type { ActionPlugin } from '../../engine/types'

export const Peek: ActionPlugin = {
  type: 'action',
  name: 'peek',
  fn: ({ peek }, fn: () => any) => {
    return peek(fn)
  },
}
