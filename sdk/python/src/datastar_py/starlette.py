from __future__ import annotations

from functools import wraps
from typing import Any

from starlette.requests import Request
from starlette.responses import StreamingResponse as _StreamingResponse

from . import _read_signals
from .sse import SSE_HEADERS, ServerSentEventGenerator

__all__ = [
    "SSE_HEADERS",
    "DatastarStreamingResponse",
    "ServerSentEventGenerator",
    "read_signals",
]


class DatastarStreamingResponse(_StreamingResponse):
    @wraps(_StreamingResponse.__init__)
    def __init__(self, *args, **kwargs):
        kwargs["headers"] = {**SSE_HEADERS, **kwargs.get("headers", {})}
        super().__init__(*args, **kwargs)


async def read_signals(request: Request) -> dict[str, Any] | None:
    return _read_signals(
        request.method, request.headers, request.query_params, await request.body()
    )
