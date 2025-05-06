from __future__ import annotations

import json
from typing import TYPE_CHECKING, Any

from .sse import SSE_HEADERS, ServerSentEventGenerator

if TYPE_CHECKING:
    from sanic import HTTPResponse, Request

__all__ = [
    "SSE_HEADERS",
    "ServerSentEventGenerator",
    "datastar_respond",
    "read_signals",
]


async def datastar_respond(request: Request) -> HTTPResponse:
    response = await request.respond(headers=SSE_HEADERS)
    return response


async def read_signals(request: Request) -> dict[str, Any] | None:
    if not request.headers.get("Datastar-Request"):
        return None
    if request.method == "GET":
        if "datastar" not in request.args:
            return None
        return json.loads(request.args["datastar"])
    elif request.headers.get("Content-Type") == "application/json":
        return await request.json()
    return None
