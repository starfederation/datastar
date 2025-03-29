import asyncio
import time
from datetime import datetime

from django.http import HttpResponse

from datastar_py.django import DatastarStreamingHttpResponse


# ASGI Example

HTML_ASGI = """\
    <!DOCTYPE html>
    <html lang="en">
        <head>
            <title>DATASTAR on Django (ASGI)</title>
            <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
            <script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-beta.9/bundles/datastar.js"></script>
            <style>
            html, body { height: 100%; width: 100%; }
            body { background-image: linear-gradient(to right bottom, oklch(0.424958 0.052808 253.972015), oklch(0.189627 0.038744 264.832977)); }
            .container { display: grid; place-content: center; }
            .time { padding: 2rem; border-radius: 8px; margin-top: 3rem; font-family: monospace, sans-serif; background-color: oklch(0.916374 0.034554 90.5157); color: oklch(0.265104 0.006243 0.522862 / 0.6); font-weight: 600; }
            </style>
        </head>
        <body data-signals="{currentTime: 'CURRENT_TIME'}">
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


async def home_asgi(request):
    return HttpResponse(
        HTML_ASGI.replace("CURRENT_TIME", f"{datetime.isoformat(datetime.now())}")
    )


async def updates_asgi(request):
    async def time_updates():
        while True:
            yield DatastarStreamingHttpResponse.merge_fragments(
                [f"""<span id="currentTime">{datetime.now().isoformat()}"""]
            )
            await asyncio.sleep(1)
            yield DatastarStreamingHttpResponse.merge_signals(
                {"currentTime": f"{datetime.now().isoformat()}"}
            )
            await asyncio.sleep(1)

    return DatastarStreamingHttpResponse(time_updates())


# WSGI Example

HTML_WSGI = """\
    <!DOCTYPE html>
    <html lang="en">
        <head>
            <title>DATASTAR on Django (WSGI)</title>
            <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
            <script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-beta.9/bundles/datastar.js"></script>
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
            data-on-load="@get('/updates-wsgi/')"
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


def home_wsgi(request):
    return HttpResponse(
        HTML_WSGI.replace("CURRENT_TIME", f"{datetime.isoformat(datetime.now())}")
    )


def updates_wsgi(request):
    def time_updates():
        while True:
            yield DatastarStreamingHttpResponse.merge_fragments(
                [f"""<span id="currentTime">{datetime.now().isoformat()}"""]
            )
            time.sleep(0.5)
            yield DatastarStreamingHttpResponse.merge_signals(
                {"currentTime": f"{datetime.now().isoformat()}"}
            )
            time.sleep(0.5)

    return DatastarStreamingHttpResponse(time_updates())
