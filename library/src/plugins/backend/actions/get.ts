// Icon: ic:baseline-get-app
// Slug: Sends a `GET` request.
// Description: Sends a `GET` request to the backend using `fetch`.

import { createHttpMethod } from './fetch'

export const GET = createHttpMethod('get', 'GET')
