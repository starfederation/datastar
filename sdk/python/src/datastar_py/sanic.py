from __future__ import annotations

from typing import TYPE_CHECKING, Any

from .sse import SSE_HEADERS, ServerSentEventGenerator
from . import _read_signals

if TYPE_CHECKING:
    from sanic import HTTPResponse, Request

__all__ = [
    "SSE_HEADERS",
    "ServerSentEventGenerator",
    "datastar_respond",
    "read_signals",
]


async def datastar_respond(request: Request) -> HTTPResponse:
    response = await request.respond(headers=SSE_HEADERS)
    return response


async def read_signals(request: Request) -> dict[str, Any] | None:
    return _read_signals(request.method, request.headers, request.args, request.body)
