from __future__ import annotations

from typing import TYPE_CHECKING, Any

from django.http import HttpRequest
from django.http import StreamingHttpResponse as _StreamingHttpResponse

from . import _read_signals
from .sse import SSE_HEADERS, DatastarEvent, DatastarEvents, ServerSentEventGenerator

if TYPE_CHECKING:
    from collections.abc import Mapping


__all__ = [
    "SSE_HEADERS",
    "DatastarResponse",
    "ServerSentEventGenerator",
    "read_signals",
]


class DatastarResponse(_StreamingHttpResponse):
    """Respond with 0..N `DatastarEvent`s"""

    def __init__(
        self,
        content: DatastarEvents = None,
        *,
        status: int | None = None,
        headers: Mapping[str, str] | None = None,
    ) -> None:
        if not content:
            super().__init__(tuple(), status=status or 204, headers=headers)
            return
        headers = {**SSE_HEADERS, **(headers or {})}
        if isinstance(content, DatastarEvent):
            content = (content,)
        super().__init__(content, status=status, headers=headers)


def read_signals(request: HttpRequest) -> dict[str, Any] | None:
    return _read_signals(request.method, request.headers, request.GET, request.body)
