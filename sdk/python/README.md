# datastar-py

The `datastar-py` package provides backend helpers for the [Datastar](https://data-star.dev) JS library.

Datastar sends responses back to the browser using SSE. This allows the backend to
send any number of events, from zero to infinity in response to a single request.

`datastar-py` has helpers for creating those responses, formatting the events,
reading signals from the frontend, and generating the data-* HTML attributes.

The event generator can be used with any framework. There are also custom
helpers included for the following frameworks:

* [Django](https://www.djangoproject.com/)
* [FastAPI](https://fastapi.tiangolo.com/)
* [FastHTML](https://fastht.ml/)
* [Litestar](https://litestar.dev/)
* [Quart](https://quart.palletsprojects.com/en/stable/)
* [Sanic](https://sanic.dev/en/)
* [Starlette](https://www.starlette.io/)

## Event Generation Helpers

To use `datastar-py`, import the SSE generator in your app and then use
it in your route handler:

```python
from datastar_py import ServerSentEventGenerator as SSE

# ... various app setup.
# The example below is for the Quart framework, and is only using the event generation helpers.

@app.route("/updates")
async def updates():
    async def time_updates():
        while True:
            yield SSE.merge_fragments(
                [f"""<span id="currentTime">{datetime.now().isoformat()}"""]
            )
            await asyncio.sleep(1)
            yield SSE.merge_signals({"currentTime": f"{datetime.now().isoformat()}"})
            await asyncio.sleep(1)

    response = await make_response(time_updates(), SSE_HEADERS)
    response.timeout = None
    return response
```

## Response Helpers

A datastar response consists of 0..N datastar events. There are response
classes included to make this easy in all of the supported frameworks.

The following examples will work across all supported frameworks when the
response class is imported from the appropriate framework package.
e.g. `from datastar_py.quart import DatastarResponse` The containing functions
are not shown here, as they will differ per framework.


```python
# 0 events, a 204
return DatastarResponse()
# 1 event
return DatastarResponse(ServerSentEventGenerator.merge_fragments("<div id='mydiv'></div>"))
# 2 events
return DatastarResponse([
    ServerSentEventGenerator.merge_fragments("<div id='mydiv'></div>"),
    ServerSentEventGenerator.merge_signals({"mysignal": "myval"}),
])
# N events, a long lived stream (for all frameworks but sanic)
async def updates():
    while True:
        yield ServerSentEventGenerator.merge_fragments("<div id='mydiv'></div>")
        await asyncio.sleep(1)
return DatastarResponse(updates())
# A long lived stream for sanic
response = await datastar_respond(request)
# which is just a helper for the following
# response = await request.respond(DatastarResponse())
while True:
    await response.send(ServerSentEventGenerator.merge_fragments("<div id='mydiv'></div>"))
    await asyncio.sleep(1)
```

## Signal Helpers
The current state of the datastar signals is included by default in every 
datastar request. A helper is included to load those signals for each
framework. `read_signals`

```python
from datastar_py.quart import read_signals

@app.route("/updates")
async def updates():
    signals = await read_signals()
```

## Attribute Generation Helper
Datastar allows HTML generation to be done on the backend. datastar-py includes
a helper to generate data-* attributes in your HTML with IDE completion and
type checking. It can be used with many different HTML generation libraries.

```python
from datastar_py import attribute_generator as data

# htpy
button(data.on("click", "console.log('clicked')").debounce(1000).stop)["My Button"]
# FastHTML
Button("My Button", **data.on("click", "console.log('clicked')").debounce(1000).stop)
# After next release of FastHTML you don't have to unpack the datastar helpers e.g.
Button("My Button", data.on("click", "console.log('clicked')").debounce(1000).stop)
# f-strings
f"<button {data.on("click", "console.log('clicked')").debounce(1000).stop}>My Button</button>"
# Jinja, but no editor completion :(
<button {{data.on("click", "console.log('clicked')").debounce(1000).stop}}>My Button</button>
```