import json
from functools import wraps
from typing import Any

from starlette.requests import Request
from starlette.responses import StreamingResponse as _StreamingResponse

from .sse import SSE_HEADERS, ServerSentEventGenerator


class DatastarStreamingResponse(_StreamingResponse, ServerSentEventGenerator):
    @wraps(_StreamingResponse.__init__)
    def __init__(self, *args, **kwargs):
        kwargs["headers"] = {**SSE_HEADERS, **kwargs.get("headers", {})}
        super().__init__(*args, **kwargs)


async def read_signals(request: Request) -> dict[str, Any]:
    if request.method == "GET":
        return json.loads(request.query_params.get("datastar"))
    else:
        return await request.json()
