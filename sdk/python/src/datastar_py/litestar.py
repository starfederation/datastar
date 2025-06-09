from __future__ import annotations

from typing import TYPE_CHECKING, Any

from litestar.response import Stream

from . import _read_signals
from .sse import SSE_HEADERS, DatastarEvent, DatastarEvents, ServerSentEventGenerator

if TYPE_CHECKING:
    from collections.abc import Mapping

    from litestar import Request
    from litestar.background_tasks import BackgroundTask, BackgroundTasks
    from litestar.types import ResponseCookies

__all__ = [
    "SSE_HEADERS",
    "DatastarResponse",
    "ServerSentEventGenerator",
    "read_signals",
]


class DatastarResponse(Stream):
    """Respond with 0..N `DatastarEvent`s"""

    default_headers: dict[str, str] = SSE_HEADERS.copy()

    def __init__(
        self,
        content: DatastarEvents = None,
        *,
        background: BackgroundTask | BackgroundTasks | None = None,
        cookies: ResponseCookies | None = None,
        headers: Mapping[str, str] | None = None,
        status_code: int | None = None,
        # Enables this to be used as a response_class
        **_,  # noqa: ANN003
    ) -> None:
        if not content:
            status_code = status_code or 204
            content = tuple()
        else:
            headers = {**self.default_headers, **(headers or {})}
        if isinstance(content, DatastarEvent):
            content = (content,)
        super().__init__(
            content,
            background=background,
            cookies=cookies,
            headers=headers,
            status_code=status_code,
        )


async def read_signals(request: Request) -> dict[str, Any] | None:
    return _read_signals(
        request.method, request.headers, request.query_params, await request.body()
    )
