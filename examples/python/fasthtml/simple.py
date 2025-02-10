import asyncio
import json
from datetime import datetime

from fasthtml.common import *
from datastar_py import DatastarFastHTMLResponse


# Import datastar from a CDN and as a module
app, rt = fast_app(
    live=True,
    hdrs=(
        Script(
            type="module",
            src="https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.0-beta.5/bundles/datastar.js",
        ),
    ),
)

example_style = Style(
    "html, body { height: 100%; width: 100%; } h1 { color: #ccc; text-align: center } body { background-image: linear-gradient(to right bottom, oklch(0.424958 0.052808 253.972015), oklch(0.189627 0.038744 264.832977)); } .container { display: grid; place-content: center; } .time { padding: 2rem; border-radius: 8px; margin-top: 3rem; font-family: monospace, sans-serif; background-color: oklch(0.916374 0.034554 90.5157); color: oklch(0.265104 0.006243 0.522862 / 0.6); font-weight: 600; }"
)


@rt("/")
async def index():
    now = datetime.isoformat(datetime.now())
    return Titled(
        "Datastar FastHTML example",
        example_style,
        Body(data_signals=json.dumps({"currentTime": now}))(
            Div(cls="container")(
                # Initiate a GET request on load to the /updates endpoint, which returns a stream
                Div(data_on_load="@get('/updates')", cls="time")(
                    "Current time from fragment: ",
                    # Use currentTime ID for fragment replacement
                    Span(id="currentTime")(now),
                ),
                Div(cls="time")(
                    "Current time from signal: ",
                    # Use currentTime as data-text for signal replacement
                    Span(data_text="$currentTime")(now),
                ),
            ),
        ),
    )


async def clock(sse):
    while True:
        now = datetime.isoformat(datetime.now())
        yield sse.merge_fragments([Span(id="currentTime")(now)])
        await asyncio.sleep(1)
        yield sse.merge_signals({"currentTime": f"{now}"})
        await asyncio.sleep(1)


@rt
async def updates():
    return DatastarFastHTMLResponse(clock)


serve()
