from __future__ import annotations

import json
from collections.abc import Mapping
from typing import Any

from .sse import SSE_HEADERS, ServerSentEventGenerator
from .attributes import attribute_generator

__all__ = ["attribute_generator", "ServerSentEventGenerator", "SSE_HEADERS"]


def _read_signals(
    method: str, headers: Mapping, params: Mapping, body: str | bytes
) -> dict[str, Any] | None:
    if "Datastar-Request" not in headers:
        return None
    if method == "GET":
        data = params.get("datastar")
    elif headers.get("Content-Type") == "application/json":
        data = body
    else:
        return None
    return json.loads(data) if data else None
