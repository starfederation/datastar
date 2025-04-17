import json
from typing import TYPE_CHECKING, Any

from .sse import SSE_HEADERS, ServerSentEventGenerator

if TYPE_CHECKING:
    from sanic import Request, HTTPResponse


async def datastar_respond(request: "Request") -> "HTTPResponse":
    response = await request.respond(headers=SSE_HEADERS)
    return response

async def read_signals(request: "Request") -> dict[str, Any]:
    if request.method == "GET":
        return json.loads(request.args.get("datastar"))
    else:
        return await request.json()
