import typing
from functools import wraps

from starlette.responses import StreamingResponse as _StreamingResponse

from .sse import SSE_HEADERS, ServerSentEventGenerator, _wrap_event, _async_map, _sse_iterable_wrapper

if typing.TYPE_CHECKING:
    from starlette.responses import ContentStream


class DatastarStreamingResponse(_StreamingResponse):
    @wraps(_StreamingResponse.__init__)
    def __init__(self, content: ContentStream, *args, **kwargs):
        kwargs["headers"] = {**SSE_HEADERS, **kwargs.get("headers", {})}
        content = _sse_iterable_wrapper(content)
        super().__init__(content, *args, **kwargs)


def sse_generator(generator_func):
    @wraps(generator_func)
    def _wrapper(*args, **kwargs):
        content = generator_func(*args, **kwargs)
        return DatastarStreamingResponse(content)
    return _wrapper