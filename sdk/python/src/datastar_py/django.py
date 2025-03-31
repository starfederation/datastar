import typing
from functools import wraps

from django.http import StreamingHttpResponse as _StreamingHttpResponse

from .sse import SSE_HEADERS, ServerSentEventGenerator, _async_map, _wrap_event


class DatastarStreamingHttpResponse(_StreamingHttpResponse):
    @wraps(_StreamingHttpResponse.__init__)
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

        return DatastarStreamingHttpResponse(content)
    return _wrapper
