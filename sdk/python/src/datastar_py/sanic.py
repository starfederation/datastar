from .sse import SSE_HEADERS, ServerSentEventGenerator


async def make_datastar_response(request):
    response = await request.respond(headers=SSE_HEADERS)
    return response
