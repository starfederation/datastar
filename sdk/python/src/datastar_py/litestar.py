from __future__ import annotations

from functools import wraps
from typing import Any, Callable, TYPE_CHECKING

from litestar.response import Stream

from .sse import SSE_HEADERS, ServerSentEventGenerator
from . import _read_signals

if TYPE_CHECKING:
    from litestar import Request
    from litestar.types.helper_types import StreamType

__all__ = ["SSE_HEADERS", "DatastarSSE", "ServerSentEventGenerator", "read_signals"]


class DatastarSSE(Stream):
    @wraps(Stream.__init__)
    def __init__(
        self,
        content: StreamType[str | bytes] | Callable[[], StreamType[str | bytes]],
        **kwargs: Any,
    ) -> None:
        """
        Similar to litestar's ServerSentEvent, but since our event generator just returns text we don't need
        anything fancy.
        """
        kwargs["headers"] = {**SSE_HEADERS, **kwargs.get("headers", {})}
        kwargs["media_type"] = "text/event-stream"
        # Removing this argument allows the class to be used as a 'response_class' on a route
        kwargs.pop("type_encoders", None)
        super().__init__(
            content,
            **kwargs,
        )


async def read_signals(request: Request) -> dict[str, Any] | None:
    return _read_signals(
        request.method, request.headers, request.query_params, await request.body()
    )
