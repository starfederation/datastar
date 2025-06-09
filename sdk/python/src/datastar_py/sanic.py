from __future__ import annotations

from typing import TYPE_CHECKING, Any

from sanic import HTTPResponse, Request

from . import _read_signals
from .sse import SSE_HEADERS, DatastarEvent, ServerSentEventGenerator

if TYPE_CHECKING:
    from collections.abc import Collection, Mapping

__all__ = [
    "SSE_HEADERS",
    "DatastarResponse",
    "ServerSentEventGenerator",
    "datastar_respond",
    "read_signals",
]


class DatastarResponse(HTTPResponse):
    default_headers: dict[str, str] = SSE_HEADERS.copy()

    def __init__(
        self,
        content: DatastarEvent | Collection[DatastarEvent] | None = None,
        status: int | None = None,
        headers: Mapping[str, str] | None = None,
    ) -> None:
        if not content:
            status = status or 204
        super().__init__(
            content, status=status or 200, headers={**self.default_headers, **(headers or {})}
        )

    async def send(
        self,
        event: DatastarEvent | None = None,
        end_stream: bool | None = None,
    ) -> None:
        if event and self.status == 204:
            # When the response is created with no content, it's set to a 204 by default
            # if we end up streaming to it, change the status code to 200 before sending.
            self.status = 200
        await super().send(event, end_stream=end_stream)


async def datastar_respond(
    request: Request, *, status: int = 200, headers: Mapping[str, str] | None = None
) -> DatastarResponse:
    return await request.respond(DatastarResponse(status=status, headers=headers))


async def read_signals(request: Request) -> dict[str, Any] | None:
    return _read_signals(request.method, request.headers, request.args, request.body)
