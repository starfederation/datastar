from __future__ import annotations

from collections.abc import Mapping
from typing import TYPE_CHECKING, Any
from warnings import deprecated

from starlette.requests import Request
from starlette.responses import StreamingResponse as _StreamingResponse

from . import _read_signals
from .sse import SSE_HEADERS, DatastarEvent, DatastarEvents, ServerSentEventGenerator

if TYPE_CHECKING:
    from starlette.background import BackgroundTask

__all__ = [
    "SSE_HEADERS",
    "DatastarResponse",
    "DatastarStreamingResponse",
    "ServerSentEventGenerator",
    "read_signals",
]


class DatastarResponse(_StreamingResponse):
    """Respond with 0..N `DatastarEvent`s"""

    def __init__(
        self,
        content: DatastarEvents = None,
        status_code: int | None = None,
        headers: Mapping[str, str] | None = None,
        background: BackgroundTask | None = None,
    ) -> None:
        if not content:
            super().__init__(tuple(), status_code=status_code or 204, headers=headers)
            return
        status_code = status_code or 200
        headers = {**SSE_HEADERS, **(headers or {})}
        if isinstance(content, DatastarEvent):
            content = (content,)
        super().__init__(content, status_code=status_code, headers=headers, background=background)


@deprecated("Use DatastarResponse instead")
class DatastarStreamingResponse(DatastarResponse):
    pass


async def read_signals(request: Request) -> dict[str, Any] | None:
    return _read_signals(
        request.method, request.headers, request.query_params, await request.body()
    )
