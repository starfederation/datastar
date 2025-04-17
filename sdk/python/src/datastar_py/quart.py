import json
from typing import Any

from quart import make_response as _make_response, request

from .sse import ServerSentEventGenerator, SSE_HEADERS


async def make_datastar_response(async_generator):
    response = await _make_response(async_generator, SSE_HEADERS)
    response.timeout = None
    return response


async def read_signals() -> dict[str, Any]:
    if request.method == "GET":
        return json.loads(request.args.get("datastar"))
    else:
        return await request.get_json()
