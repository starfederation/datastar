# /// script
# dependencies = [
#   "datastar-py",
#   "litestar",
# ]
# [tool.uv.sources]
# datastar-py = { path = "../../../sdk/python" }
# ///

import asyncio
from collections.abc import AsyncGenerator
from datetime import datetime

import uvicorn
from datastar_py.litestar import (
    ServerSentEventGenerator,
    read_signals,
    DatastarResponse,
)

from litestar import Litestar, MediaType, get
from litestar.di import Provide

HTML = """\
	<!DOCTYPE html>
	<html lang="en">
		<head>
			<title>DATASTAR on Litestar</title>
			<meta name="viewport" content="width=device-width, initial-scale=1.0"/>
            <script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-beta.11/bundles/datastar.js"></script>
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
            Current time from fragment: <span id="currentTime">CURRENT_TIME</span>
            </div>
            <div
            class="time"
            >
            Current time from signal: <span data-text="$currentTime">CURRENT_TIME</span>
            </div>
        </div>
		</body>
	</html>
"""


@get("/", media_type=MediaType.HTML)
async def read_root() -> str:
    return HTML.replace("CURRENT_TIME", f"{datetime.isoformat(datetime.now())}")


async def time_updates() -> AsyncGenerator[str, None]:
    while True:
        yield ServerSentEventGenerator.merge_fragments(
            f"""<span id="currentTime">{datetime.now().isoformat()}"""
        )
        await asyncio.sleep(1)
        yield ServerSentEventGenerator.merge_signals(
            {"currentTime": f"{datetime.now().isoformat()}"}
        )
        await asyncio.sleep(1)


# We aren't using the signals for anything meaningful here, but `read_signals` can be
# used as a dependency to automatically parse the signals from the request.
@get("/updates", dependencies={"signals": Provide(read_signals)})
async def updates(signals: dict | None) -> DatastarResponse:
    print(signals)
    return DatastarResponse(time_updates())


app = Litestar(route_handlers=[read_root, updates])


if __name__ == "__main__":
    uvicorn.run(app)
