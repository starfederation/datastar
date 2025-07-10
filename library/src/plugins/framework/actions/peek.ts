// Icon: ion:eye
// Slug: Peek at signals that match the signal path
// Description: Use a signal or computed signal without subscribing to it.

import type { ActionPlugin } from '../../../engine/types'

export const Peek: ActionPlugin = {
  type: 'action',
  name: 'peek',
  fn: ({ peek }, fn: () => any) => {
    return peek(fn)
  },
}
