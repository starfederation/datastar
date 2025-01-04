import asyncio
from datetime import datetime

from sanic import Sanic
from sanic.response import html

from datastar_py import ServerSentEventGenerator as SSE
from datastar_py.consts import FragmentMergeMode
from datastar_py.responses import make_datastar_sanic_response

app = Sanic("DataStarApp")

HTML = """\
	<!DOCTYPE html>
	<html lang="en">
		<head>
			<title>DATASTAR on Sanic</title>
			<meta name="viewport" content="width=device-width, initial-scale=1.0"/>
            <script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar/bundles/datastar.js"></script>
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
            data-on-load="sse('/updates')"
        >
            <button data-on-click="sse('/add_fragment')">Add fragment timer</button>
            <button data-on-click="sse('/add_signal')">Add signal timer</button>
            <div class="time fragment">
            Current time from fragment: CURRENT_TIME
            </div>
            <div class="time signal" >
            Current time from signal: <span data-text="currentTime.value">CURRENT_TIME</span>
            </div>
        </div>
		</body>
	</html>
"""


@app.get("/")
async def hello_world(request):
    return html(HTML.replace("CURRENT_TIME", f"{datetime.isoformat(datetime.now())}"))


@app.get("/add_signal")
async def add_signal(request):
    response = await make_datastar_sanic_response(request)

    await response.send(
        SSE.merge_fragments(
            [
                """
            <div class="time signal">
            Current time from signal: <span data-text="currentTime.value">CURRENT_TIME</span>
            </div>
            """
            ],
            selector="#timers",
            merge_mode=FragmentMergeMode.FragmentMergeModeAppend,
        )
    )

    await response.eof()


@app.get("/add_fragment")
async def add_fragment(request):
    response = await make_datastar_sanic_response(request)

    await response.send(
        SSE.merge_fragments(
            [
                f"""\
            <div class="time fragment">
            Current time from fragment: {datetime.now().isoformat()}
            </div>
            """
            ],
            selector="#timers",
            merge_mode=FragmentMergeMode.FragmentMergeModeAppend,
        )
    )

    await response.eof()


@app.get("/updates")
async def updates(request):
    response = await make_datastar_sanic_response(request)

    while True:
        await response.send(
            SSE.merge_fragments(
                [
                    f"""
            <div class="time fragment" >
            Current time from fragment: {datetime.now().isoformat()}
            </div>
                """
                ],
                selector=".fragment",
            )
        )
        await asyncio.sleep(1)
        await response.send(
            SSE.merge_signals({"currentTime": f"{datetime.now().isoformat()}"})
        )
        await asyncio.sleep(1)
