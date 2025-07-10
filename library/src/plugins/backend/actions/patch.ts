// Icon: fluent:patch-24-filled
// Slug: Sends a `PATCH` request.
// Description: Sends a `PATCH` request to the backend using `fetch`.

import { createHttpMethod } from './fetch'

export const PATCH = createHttpMethod('patch', 'PATCH')
