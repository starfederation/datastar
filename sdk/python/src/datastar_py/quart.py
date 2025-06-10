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
    "DatastarResponse",
    "ServerSentEventGenerator",
    "read_signals",
]


class DatastarResponse(Response):
    """Respond with 0..N `DatastarEvent`s"""

    default_headers: dict[str, str] = SSE_HEADERS.copy()

    def __init__(
        self,
        content: DatastarEvents = None,
        status: int | None = None,
        headers: Mapping[str, str] | None = None,
    ) -> None:
        if not content:
            status = status or 204
        else:
            headers = {**self.default_headers, **(headers or {})}
        super().__init__(content, status=status, headers=headers)
        if isgenerator(content) or isasyncgen(content):
            self.timeout = None


async def read_signals() -> dict[str, Any] | None:
    return _read_signals(request.method, request.headers, request.args, await request.get_data())
