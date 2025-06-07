from __future__ import annotations

from typing import TYPE_CHECKING, Any

from django.http import HttpRequest
from django.http import StreamingHttpResponse as _StreamingHttpResponse

from . import _read_signals
from .sse import (
    SSE_HEADERS,
    DatastarEvents,
    ServerSentEventGenerator,
    _to_events_iterable,
)

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
            status = status or 204
        else:
            headers = {**SSE_HEADERS, **(headers or {})}
        super().__init__(_to_events_iterable(content), status=status, headers=headers)


def read_signals(request: HttpRequest) -> dict[str, Any] | None:
    return _read_signals(request.method, request.headers, request.GET, request.body)
