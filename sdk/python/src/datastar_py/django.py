from functools import wraps

from django.http import StreamingHttpResponse as _StreamingHttpResponse

from .sse import SSE_HEADERS, ServerSentEventGenerator


class DatastarStreamingHttpResponse(_StreamingHttpResponse):
    @wraps(_StreamingHttpResponse.__init__)
    def __init__(self, *args, **kwargs):
        kwargs["headers"] = {**SSE_HEADERS, **kwargs.get("headers", {})}
        super().__init__(*args, **kwargs)
