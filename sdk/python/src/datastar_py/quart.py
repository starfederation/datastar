from __future__ import annotations

from collections.abc import Awaitable, Mapping
from functools import wraps
from inspect import isasyncgen, isasyncgenfunction, isgenerator
from typing import Any, Callable, ParamSpec

from quart import Response, copy_current_request_context, request, stream_with_context

from . import _read_signals
from .sse import SSE_HEADERS, DatastarEvents, ServerSentEventGenerator

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


P = ParamSpec("P")


def datastar_response(
    func: Callable[P, Awaitable[DatastarEvents] | DatastarEvents],
) -> Callable[P, Awaitable[DatastarResponse]]:
    """A decorator which wraps a function result in DatastarResponse.

    Can be used on a sync or async function or generator function.
    """

    @wraps(func)
    async def wrapper(*args: P.args, **kwargs: P.kwargs) -> DatastarResponse:
        if isasyncgenfunction(func):
            return DatastarResponse(stream_with_context(func)(*args, **kwargs))
        return DatastarResponse(await copy_current_request_context(func)(*args, **kwargs))

    wrapper.__annotations__["return"] = "DatastarResponse"
    return wrapper


async def read_signals() -> dict[str, Any] | None:
    return _read_signals(request.method, request.headers, request.args, await request.get_data())
