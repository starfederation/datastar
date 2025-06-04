from __future__ import annotations

from typing import TYPE_CHECKING, Any

from . import _read_signals
from .sse import SSE_HEADERS, ServerSentEventGenerator

if TYPE_CHECKING:
    from sanic import HTTPResponse, Request
    from sanic.compat import Header

__all__ = [
    "SSE_HEADERS",
    "ServerSentEventGenerator",
    "datastar_respond",
    "read_signals",
]


async def datastar_respond(
    request: Request, *, status: int = 200, headers: Header | dict[str, str] | None = None
) -> HTTPResponse:
    response = await request.respond(status=status, headers={**SSE_HEADERS, **(headers or {})})
    return response


async def read_signals(request: Request) -> dict[str, Any] | None:
    return _read_signals(request.method, request.headers, request.args, request.body)
