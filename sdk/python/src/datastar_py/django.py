import json
from functools import wraps
from typing import Any

from django.http import StreamingHttpResponse as _StreamingHttpResponse, HttpRequest

from .sse import SSE_HEADERS, ServerSentEventGenerator


class DatastarStreamingHttpResponse(_StreamingHttpResponse, ServerSentEventGenerator):
    @wraps(_StreamingHttpResponse.__init__)
    def __init__(self, *args, **kwargs):
        kwargs["headers"] = {**SSE_HEADERS, **kwargs.get("headers", {})}
        super().__init__(*args, **kwargs)

def read_signals(request: HttpRequest) -> dict[str, Any]:
    if request.method == "GET":
        return json.loads(request.GET.get("datastar"))
    else:
        return json.loads(request.body)