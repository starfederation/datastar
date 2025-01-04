// Icon: material-symbols:arrows-input
// Slug: Use a PUT request to fetch data from a server using Server-Sent Events matching the Datastar SDK interface
// Description: Remember, SSE is just a regular SSE request but with the ability to send 0-inf messages to the client.

import {
  type ActionPlugin,
  PluginType,
  type RuntimeContext,
} from '../../../../engine/types'
import { type SSEArgs, sse } from './sse'

export const PUT: ActionPlugin = {
  type: PluginType.Action,
  name: 'put',
  fn: async (ctx: RuntimeContext, url: string, args: SSEArgs) => {
    return sse(ctx, 'PUT', url, { ...args })
  },
}
