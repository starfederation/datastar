from .sse import SSE_HEADERS, ServerSentEventGenerator

try:
    from django.http import StreamingHttpResponse as DjangoStreamingHttpResponse
except ImportError:

    class DjangoStreamingHttpResponse: ...


try:
    from fastapi.responses import StreamingResponse as FastAPIStreamingResponse
except ImportError:

    class FastAPIStreamingResponse: ...


try:
    from quart import make_response
except ImportError:
    pass


class DatastarDjangoResponse(DjangoStreamingHttpResponse):
    def __init__(self, generator, *args, **kwargs):
        kwargs["headers"] = SSE_HEADERS
        super().__init__(generator(ServerSentEventGenerator), *args, **kwargs)


class DatastarFastAPIResponse(FastAPIStreamingResponse):
    def __init__(self, generator, *args, **kwargs):
        kwargs["headers"] = SSE_HEADERS
        super().__init__(generator(ServerSentEventGenerator), *args, **kwargs)


async def make_datastar_quart_response(generator):
    response = await make_response(generator(ServerSentEventGenerator), SSE_HEADERS)
    response.timeout = None
    return response


async def make_datastar_sanic_response(request):
    response = await request.respond(headers=SSE_HEADERS)
    return response
