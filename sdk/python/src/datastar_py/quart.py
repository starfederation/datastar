from functools import wraps

from quart import make_response as _make_response

from .sse import ServerSentEventGenerator, SSE_HEADERS, _async_map, _wrap_event


async def make_datastar_response(async_generator):
    response = await _make_response(async_generator, SSE_HEADERS)
    response.timeout = None
    return response


def sse_generator(generator_func):
    @wraps(generator_func)
    async def _wrapper(*args, **kwargs):
        content = _async_map(_wrap_event, generator_func(*args, **kwargs))

        return await make_datastar_response(content)
    return _wrapper
