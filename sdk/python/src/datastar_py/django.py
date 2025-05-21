from __future__ import annotations

from functools import wraps
from typing import Any

from django.http import HttpRequest
from django.http import StreamingHttpResponse as _StreamingHttpResponse

from .sse import SSE_HEADERS, ServerSentEventGenerator
from . import _read_signals


class DatastarStreamingHttpResponse(_StreamingHttpResponse):
    @wraps(_StreamingHttpResponse.__init__)
    def __init__(self, *args, **kwargs):
        kwargs["headers"] = {**SSE_HEADERS, **kwargs.get("headers", {})}
        super().__init__(*args, **kwargs)


def read_signals(request: HttpRequest) -> dict[str, Any] | None:
    return _read_signals(request.method, request.headers, request.GET, request.body)
