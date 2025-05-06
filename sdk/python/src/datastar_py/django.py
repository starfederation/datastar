from __future__ import annotations

import json
from functools import wraps
from typing import Any

from django.http import HttpRequest
from django.http import StreamingHttpResponse as _StreamingHttpResponse

from .sse import SSE_HEADERS, ServerSentEventGenerator

__all__ = [
    "SSE_HEADERS",
    "DatastarStreamingHttpResponse",
    "ServerSentEventGenerator",
    "read_signals",
]


class DatastarStreamingHttpResponse(_StreamingHttpResponse):
    @wraps(_StreamingHttpResponse.__init__)
    def __init__(self, *args, **kwargs):
        kwargs["headers"] = {**SSE_HEADERS, **kwargs.get("headers", {})}
        super().__init__(*args, **kwargs)


def read_signals(request: HttpRequest) -> dict[str, Any] | None:
    if not request.headers.get("Datastar-Request"):
        return None
    if request.method == "GET":
        if "datastar" not in request.GET:
            return None
        return json.loads(request.GET["datastar"])
    elif request.headers.get("Content-Type") == "application/json":
        return json.loads(request.body)
    return None