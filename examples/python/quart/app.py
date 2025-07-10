# /// script
# dependencies = [
#   "datastar-py",
#   "quart",
# ]
# [tool.uv.sources]
# datastar-py = { path = "../../../sdk/python" }
# ///

import asyncio
from datetime import datetime

from datastar_py.consts import VERSION
from datastar_py.quart import (
    DatastarResponse,
    ServerSentEventGenerator,
    read_signals,
)

from quart import Quart

app = Quart(__name__)

HTML = """\
	<!DOCTYPE html>
	<html lang="en">
		<head>
			<title>DATASTAR on Quart</title>
			<meta name="viewport" content="width=device-width, initial-scale=1.0"/>
            <script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@VERSION/bundles/datastar.js"></script>
			<style>
            html, body { height: 100%; width: 100%; }
            body { background-image: linear-gradient(to right bottom, oklch(0.424958 0.052808 253.972015), oklch(0.189627 0.038744 264.832977)); }
            .container { display: grid; place-content: center; }
            .time { padding: 2rem; border-radius: 8px; margin-top: 3rem; font-family: monospace, sans-serif; background-color: oklch(0.916374 0.034554 90.5157); color: oklch(0.265104 0.006243 0.522862 / 0.6); font-weight: 600; }
			</style>
		</head>
		<body
            data-signals="{currentTime: 'CURRENT_TIME'}"
		>
        <div class="container">
            <div
            class="time"
            data-on-load="@get('/updates')"
            >
            Current time from element: <span id="currentTime">CURRENT_TIME</span>
            </div>
            <div
            class="time"
            >
            Current time from signal: <span data-text="$currentTime">CURRENT_TIME</span>
            </div>
        </div>
		</body>
	</html>
""".replace("VERSION", VERSION)


@app.route("/updates")
async def updates():
    # If you need to load the signals, the `read_signals helper` is available
    signals = await read_signals()
    print(signals)

    async def time_updates():
        while True:
            yield ServerSentEventGenerator.patch_elements(
                f"""<span id="currentTime">{datetime.now().isoformat()}"""
            )
            await asyncio.sleep(1)
            yield ServerSentEventGenerator.patch_signals(
                {"currentTime": f"{datetime.now().isoformat()}"}
            )
            await asyncio.sleep(1)

    return DatastarResponse(time_updates())


@app.route("/")
async def hello():
    return HTML.replace("CURRENT_TIME", f"{datetime.isoformat(datetime.now())}")


if __name__ == "__main__":
    app.run()
