from __future__ import annotations

from collections.abc import AsyncIterable, Iterable
from inspect import isasyncgen, isgenerator
from typing import TYPE_CHECKING, Any

from quart import make_response as _make_response
from quart import request

from . import _read_signals
from .sse import SSE_HEADERS, ServerSentEventGenerator

if TYPE_CHECKING:
    from quart.typing import HeadersValue, ResponseTypes, StatusCode

__all__ = [
    "SSE_HEADERS",
    "ServerSentEventGenerator",
    "make_datastar_response",
    "read_signals",
]


async def make_datastar_response(
    response_content: str | Iterable | AsyncIterable | None,
    status_or_headers: StatusCode | HeadersValue = None,
    headers: HeadersValue = None,
    /,
) -> ResponseTypes:
    status = status_or_headers
    if status_or_headers is not None and not isinstance(status_or_headers, int):
        status, headers = None, status_or_headers
    headers = {**SSE_HEADERS, **(headers or {})}
    response = await _make_response(response_content, status, headers)
    if isgenerator(response_content) or isasyncgen(response_content):
        response.timeout = None
    return response


async def read_signals() -> dict[str, Any] | None:
    return _read_signals(request.method, request.headers, request.args, await request.get_data())
