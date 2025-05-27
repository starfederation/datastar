from typing import Annotated, Any, Union

from fastapi import Depends

from .sse import SSE_HEADERS, ServerSentEventGenerator
from .starlette import DatastarStreamingResponse, read_signals


ReadSignals = Annotated[Union[dict[str, Any], None], Depends(read_signals)]
