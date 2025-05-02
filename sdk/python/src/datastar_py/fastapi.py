from typing import Annotated, Any

from fastapi import Depends

from .sse import SSE_HEADERS, ServerSentEventGenerator
from .starlette import DatastarStreamingResponse, read_signals

__all__ = [
    "SSE_HEADERS",
    "ServerSentEventGenerator",
    "ReadSignals",
    "read_signals",
    "DatastarStreamingResponse",
]


ReadSignals = Annotated[dict[str, Any], Depends(read_signals)]
