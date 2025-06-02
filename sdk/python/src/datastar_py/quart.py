from __future__ import annotations

from typing import Any

from quart import make_response as _make_response
from quart import request

from .sse import SSE_HEADERS, ServerSentEventGenerator
from . import _read_signals

__all__ = [
    "SSE_HEADERS",
    "ServerSentEventGenerator",
    "make_datastar_response",
    "read_signals",
]


async def make_datastar_response(async_generator):
    response = await _make_response(async_generator, SSE_HEADERS)
    response.timeout = None
    return response


async def read_signals() -> dict[str, Any] | None:
    return _read_signals(request.method, request.headers, request.args, await request.get_data())
