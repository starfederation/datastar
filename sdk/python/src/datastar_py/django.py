import typing
from functools import wraps

from django.http import StreamingHttpResponse as _StreamingHttpResponse

from .sse import SSE_HEADERS, ServerSentEventGenerator, _sse_iterable_wrapper


class DatastarStreamingHttpResponse(_StreamingHttpResponse):
    @wraps(_StreamingHttpResponse.__init__)
    def __init__(self, *args, **kwargs):
        kwargs["headers"] = {**SSE_HEADERS, **kwargs.get("headers", {})}
        super().__init__(*args, **kwargs)


def sse_generator(generator_func):
    @wraps(generator_func)
    def _wrapper(*args, **kwargs):
        content = generator_func(*args, **kwargs)
        return DatastarStreamingHttpResponse(_sse_iterable_wrapper(content))
    return _wrapper
