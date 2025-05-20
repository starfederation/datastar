from typing import TYPE_CHECKING, Optional, AnyStr

from sanic.response import HTTPResponse as _HTTPResponse

from .sse import SSE_HEADERS, ServerSentEventGenerator, _wrap_event

if TYPE_CHECKING:
    from sanic import Request


class DatastarResponse(_HTTPResponse):
    def __init__(self, *args, **kwargs):
        kwargs["headers"] = {**SSE_HEADERS, **kwargs.get("headers", {})}
        super().__init__(*args, **kwargs)

    async def send(
        self,
        data: Optional[AnyStr] = None,
        end_stream: Optional[bool] = None,
    ) -> None:
        data = _wrap_event(data)
        await super().send(data, end_stream)


# TODO: Deprecate this in favor of `request.respond(DatastarResponse())`?
async def datastar_respond(request: "Request", *args, **kwargs) -> "_HTTPResponse":
    response = await request.respond(DatastarResponse(*args, **kwargs))
    return response
