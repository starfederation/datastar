from typing import Annotated, Any, Union

from fastapi import Depends

from .sse import SSE_HEADERS, ServerSentEventGenerator
from .starlette import DatastarResponse, datastar_response, read_signals

__all__ = [
    "SSE_HEADERS",
    "DatastarResponse",
    "ReadSignals",
    "ServerSentEventGenerator",
    "datastar_response",
    "read_signals",
]


ReadSignals = Annotated[Union[dict[str, Any], None], Depends(read_signals)]
