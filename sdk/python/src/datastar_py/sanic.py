from typing import TYPE_CHECKING

from .sse import SSE_HEADERS, ServerSentEventGenerator

if TYPE_CHECKING:
    from sanic import Request, HTTPResponse


async def datastar_respond(request: "Request") -> "HTTPResponse":
    response = await request.respond(headers=SSE_HEADERS)
    return response
