import typing
from functools import wraps

from starlette.responses import StreamingResponse as _StreamingResponse

from .sse import SSE_HEADERS, ServerSentEventGenerator, _wrap_event, _async_map


class DatastarStreamingResponse(_StreamingResponse):
    @wraps(_StreamingResponse.__init__)
    def __init__(self, *args, **kwargs):
        kwargs["headers"] = {**SSE_HEADERS, **kwargs.get("headers", {})}
        super().__init__(*args, **kwargs)


def sse_generator(generator_func):
    @wraps(generator_func)
    def _wrapper(*args, **kwargs):
        content = generator_func(*args, **kwargs)
        if isinstance(content, typing.AsyncIterable):
            content = _async_map(_wrap_event, content)
        else:
            content = map(_wrap_event, content)

        return DatastarStreamingResponse(content)
    return _wrapper