// Icon: material-symbols:delete-outline
// Slug: Sends a `DELETE` request.
// Description: Sends a `DELETE` request to the backend using `fetch`.

import { createHttpMethod } from './fetch'

export const DELETE = createHttpMethod('delete', 'DELETE')
