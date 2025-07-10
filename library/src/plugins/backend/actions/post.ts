// Icon: ri:signpost-fill
// Slug: Sends a `POST` request.
// Description: Sends a `POST` request to the backend using `fetch`.

import { createHttpMethod } from './fetch'

export const POST = createHttpMethod('post', 'POST')
