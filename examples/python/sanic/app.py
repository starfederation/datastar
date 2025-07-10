# /// script
# dependencies = [
#   "datastar-py",
#   "sanic",
# ]
# [tool.uv.sources]
# datastar-py = { path = "../../../sdk/python" }
# ///

import asyncio
from datetime import datetime

from datastar_py.consts import ElementPatchMode, VERSION
from datastar_py.sanic import (
    DatastarResponse,
    ServerSentEventGenerator,
    datastar_respond,
    read_signals,
)

from sanic import Sanic
from sanic.response import html

app = Sanic("DataStarApp")

HTML = """\
	<!DOCTYPE html>
	<html lang="en">
		<head>
			<title>DATASTAR on Sanic</title>
			<meta name="viewport" content="width=device-width, initial-scale=1.0"/>
            <script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@VERSION/bundles/datastar.js"></script>
			<style>
            html, body { height: 100%; width: 100%; }
            body { background-image: linear-gradient(to right bottom, oklch(0.424958 0.052808 253.972015), oklch(0.189627 0.038744 264.832977)); }
            .container { display: grid; place-content: center; }
            .time { padding: 2rem; border-radius: 8px; margin-top: 3rem; font-family: monospace, sans-serif; background-color: oklch(0.916374 0.034554 90.5157); color: oklch(0.265104 0.006243 0.522862 / 0.6); font-weight: 600; }
            button { padding: 1rem; margin-top:1rem; display: inline-block;}
			</style>
		</head>
		<body
            data-signals="{currentTime: 'CURRENT_TIME'}"
		>
        <div
        id="timers"
        class="container"
            data-on-load="@get('/updates')"
        >
            <button data-on-click="@get('/add_element')">Add element timer</button>
            <button data-on-click="@get('/add_signal')">Add signal timer</button>
            <div class="time element">
            Current time from element: CURRENT_TIME
            </div>
            <div class="time signal" >
            Current time from signal: <span data-text="$currentTime">CURRENT_TIME</span>
            </div>
        </div>
		</body>
	</html>
""".replace("VERSION", VERSION)


@app.get("/")
async def hello_world(request):
    return html(HTML.replace("CURRENT_TIME", f"{datetime.isoformat(datetime.now())}"))


@app.get("/add_signal")
async def add_signal(request):
    return DatastarResponse(
        ServerSentEventGenerator.patch_elements(
            """
            <div class="time signal">
            Current time from signal: <span data-text="$currentTime">CURRENT_TIME</span>
            </div>
            """,
            selector="#timers",
            mode=ElementPatchMode.APPEND,
        )
    )


@app.get("/add_element")
async def add_element(request):
    return DatastarResponse(
        ServerSentEventGenerator.patch_elements(
            f"""\
            <div class="time element">
            Current time from element: {datetime.now().isoformat()}
            </div>
            """,
            selector="#timers",
            mode=ElementPatchMode.APPEND,
        )
    )


@app.get("/updates")
async def updates(request):
    # Signals can be parsed from the request using the `read_signals` helper
    signals = await read_signals(request)
    print(signals)

    response = await datastar_respond(request)

    while True:
        await response.send(
            ServerSentEventGenerator.patch_elements(
                f"""
                <div class="time element" >
                Current time from element: {datetime.now().isoformat()}
                </div>
                """,
                selector=".element",
            )
        )
        await asyncio.sleep(1)
        await response.send(
            ServerSentEventGenerator.patch_signals(
                {"currentTime": f"{datetime.now().isoformat()}"}
            )
        )
        await asyncio.sleep(1)


if __name__ == "__main__":
    app.run(dev=True)
