from __future__ import annotations

import json
from collections.abc import Mapping
from typing import Any

from .attributes import attribute_generator
from .sse import SSE_HEADERS, ServerSentEventGenerator

__all__ = ["SSE_HEADERS", "ServerSentEventGenerator", "attribute_generator"]


def _read_signals(
    method: str, headers: Mapping[str, str], params: Mapping, body: str | bytes
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
