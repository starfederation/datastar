# /// script
# requires-python = ">=3.13"
# dependencies = [
#     "datastar-py",
#     "sanic",
# ]
# [tool.uv.sources]
# datastar-py = { path = "." }
# ///

"""
Runs a test server that the SDK tests can be run against.
1. Start this server with `uv run sdk-test.py`
2. Move to the sdk/tests folder.
3. Run `test-all.sh http://127.0.0.1:8000` to run the tests.
"""

import re

from sanic import Request, Sanic

from datastar_py import ServerSentEventGenerator as SSE
from datastar_py.sanic import DatastarResponse, read_signals
from datastar_py.sse import DatastarEvent

app = Sanic("datastar-sdk-test")


@app.route("/test", methods=["GET", "POST"])
async def test_route(request: Request) -> None:
    signals = await read_signals(request)
    events: list[dict] = signals["events"]

    response = await request.respond(response=DatastarResponse())

    for event in events:
        await response.send(build_event(event))


def build_event(input: dict) -> DatastarEvent:
    event_type = input.pop("type")
    signals_raw = input.pop("signals-raw", None)
    kwargs = {camel_to_snake(k): v for k, v in input.items()}
    if signals_raw:
        kwargs["signals"] = signals_raw
    return getattr(SSE, camel_to_snake(event_type))(**kwargs)


def camel_to_snake(text: str) -> str:
    return re.sub(r"(.)([A-Z])", r"\1_\2", text).lower()


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=8000)
