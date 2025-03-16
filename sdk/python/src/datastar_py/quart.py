from quart import make_response as _make_response

from .sse import ServerSentEventGenerator, SSE_HEADERS


async def make_datastar_response(iterator):
    response = await _make_response(iterator, SSE_HEADERS)
    response.timeout = None
    return response

