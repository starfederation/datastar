from __future__ import annotations

import json
from typing import Any

from quart import make_response as _make_response
from quart import request

from .sse import SSE_HEADERS, ServerSentEventGenerator

__all__ = [
    "SSE_HEADERS",
    "ServerSentEventGenerator",
    "make_datastar_response",
    "read_signals",
]


async def make_datastar_response(async_generator):
    response = await _make_response(async_generator, SSE_HEADERS)
    response.timeout = None
    return response


async def read_signals() -> dict[str, Any] | None:
    if not request.headers.get("Datastar-Request"):
        return None
    if request.method == "GET":
        if "datastar" not in request.args:
            return None
        return json.loads(request.args["datastar"])
    elif request.headers.get("Content-Type") == "application/json":
        return await request.get_json()
    return None
