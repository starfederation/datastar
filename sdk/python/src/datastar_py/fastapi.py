from typing import Annotated, Any

from fastapi import Depends
from .starlette import DatastarStreamingResponse, ServerSentEventGenerator, read_signals


SignalsDep = Annotated[dict[str, Any], Depends(read_signals)]
