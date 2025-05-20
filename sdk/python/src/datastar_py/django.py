import typing
from functools import wraps

from django.http import StreamingHttpResponse as _StreamingHttpResponse

from .sse import SSE_HEADERS, ServerSentEventGenerator, _sse_iterable_wrapper


class DatastarStreamingHttpResponse(_StreamingHttpResponse):
    @wraps(_StreamingHttpResponse.__init__)
    def __init__(self, streaming_content=(), *args, **kwargs):
        kwargs["headers"] = {**SSE_HEADERS, **kwargs.get("headers", {})}
        streaming_content = _sse_iterable_wrapper(streaming_content)
        super().__init__(streaming_content, *args, **kwargs)


def sse_generator(generator_func):
    @wraps(generator_func)
    def _wrapper(*args, **kwargs):
        content = generator_func(*args, **kwargs)
        return DatastarStreamingHttpResponse(content)
    return _wrapper
