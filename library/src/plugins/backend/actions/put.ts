// Icon: material-symbols:arrows-input
// Slug: Sends a `PUT` request.
// Description: Sends a `PUT` request to the backend using `fetch`.

import { createHttpMethod } from './fetch'

export const PUT = createHttpMethod('put', 'PUT')
