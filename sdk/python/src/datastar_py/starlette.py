from functools import wraps

from starlette.responses import StreamingResponse as _StreamingResponse

from .sse import SSE_HEADERS, ServerSentEventGenerator


class DatastarStreamingResponse(_StreamingResponse, ServerSentEventGenerator):
    @wraps(_StreamingResponse.__init__)
    def __init__(self, *args, **kwargs):
        kwargs["headers"] = {**SSE_HEADERS, **kwargs.get("headers", {})}
        super().__init__(*args, **kwargs)
