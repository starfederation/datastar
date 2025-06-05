from __future__ import annotations

from inspect import isasyncgen, isgenerator
from typing import TYPE_CHECKING, Any

from quart import Response, request

from . import _read_signals
from .sse import SSE_HEADERS, DatastarEvents, ServerSentEventGenerator

if TYPE_CHECKING:
    from collections.abc import Mapping

__all__ = [
    "SSE_HEADERS",
    "ServerSentEventGenerator",
    "make_datastar_response",
    "read_signals",
]


async def make_datastar_response(
    content: DatastarEvents = None,
    status_or_headers: int | Mapping[str, str] | None = None,
    headers: Mapping[str, str] | None = None,
    /,
) -> Response:
    """Respond with 0..N `DatastarEvent`s"""
    if status_or_headers is not None and not isinstance(status_or_headers, int):
        status, headers = None, status_or_headers
    else:
        status = status_or_headers
    if not content:
        return Response(status=status or 204, headers=headers)
    headers = {**SSE_HEADERS, **(headers or {})}
    response = Response(content, status=status, headers=headers)
    if isgenerator(content) or isasyncgen(content):
        response.timeout = None
    return response


async def read_signals() -> dict[str, Any] | None:
    return _read_signals(request.method, request.headers, request.args, await request.get_data())
