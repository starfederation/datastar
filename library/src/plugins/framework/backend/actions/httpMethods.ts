// Factory for creating HTTP method action plugins
import type { ActionPlugin } from '../../../../engine/types'
import { type FetchArgs, sse } from './fetch'

// Helper to create HTTP method plugins with consistent structure
export const createHttpMethod = (
  name: string,
  method: string,
): ActionPlugin => ({
  type: 'action',
  name,
  fn: (ctx, url: string, args: FetchArgs) => sse(ctx, method, url, args),
})
